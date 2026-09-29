(ns org.parkerici.datomic-query-improver-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [org.parkerici.datomic-query-improver :as sut]
            [org.parkerici.datomic-query-improver.test-db :as db :refer [run]]))

(use-fixtures :once db/with-db)

(defn suggest [q]
  (sut/suggest db/*attr-counts* q))

(defn check-suggestion
  "Suggests an ordering for q and checks that it returns the same results as q
  and keeps the same clauses. Returns the suggested query and the query stats."
  [q inputs]
  (let [suggested (suggest q)
        original (apply run q inputs)
        improved (apply run suggested inputs)]
    (is (nil? (:error original)) "test query should be valid")
    (is (nil? (:error improved)) (str "suggested query should be valid: " (pr-str suggested)))
    (is (= (:result original) (:result improved)))
    (is (seq (:result original)) "test query should match the synthetic data")
    (is (= (frequencies (:where (if (map? q) q (#'sut/->map-form q))))
           (frequencies (:where suggested)))
        "suggested query should have the same where clauses")
    {:suggested suggested :original original :improved improved}))

(defn check-improved
  "As check-suggestion, and also checks the suggested ordering does no more
  work than q. Returns the suggested query."
  [q & inputs]
  (let [{:keys [suggested original improved]} (check-suggestion q inputs)]
    (is (<= (:rows improved 0) (:rows original 0))
        (str "suggested ordering did more work: " (:rows improved) " vs " (:rows original)
             " rows\n" (pr-str (:where suggested))))
    suggested))

(deftest list-form-becomes-map-form
  (is (= '{:find [?e] :in [$ ?n] :where [[?e :artist/name ?n]]}
         (suggest '[:find ?e :in $ ?n :where [?e :artist/name ?n]])))
  (is (= '{:find [?e] :with [?n] :where [[?e :artist/name ?n]]}
         (suggest '[:find ?e :with ?n :where [?e :artist/name ?n]]))))

(deftest map-form-keys-are-kept
  (is (= '{:find [?e] :keys [e] :where [[?e :artist/name "Artist 1"]]}
         (suggest '{:find [?e] :keys [e] :where [[?e :artist/name "Artist 1"]]}))))

(deftest join-along-from-most-selective-clause
  (let [q '[:find ?name
            :where
            [?r :release/name ?name]
            [?r :release/label ?l]
            [?l :label/name "Label 3"]]
        suggested (check-improved q)]
    (is (= '[[?l :label/name "Label 3"]
             [?r :release/label ?l]
             [?r :release/name ?name]]
           (:where suggested)))))

(deftest inputs-are-initial-bindings
  (let [q '[:find ?r
            :in $ ?artist-name
            :where
            [?r :release/artists ?a]
            [?a :artist/name ?artist-name]]
        suggested (check-improved q "Artist 7")]
    (is (= '[?a :artist/name ?artist-name] (first (:where suggested))))))

(deftest fewer-datoms-breaks-ties
  (testing "both clauses bind one new var; :artist/active has fewer datoms"
    ;; Not a cost check: attribute counts don't capture value selectivity, and
    ;; here active=true (25 artists) is less selective than country=C1 (20).
    (let [q '[:find ?a :where [?a :artist/country "C1"] [?a :artist/active true]]]
      (is (= '[[?a :artist/active true] [?a :artist/country "C1"]]
             (:where (:suggested (check-suggestion q []))))))))

(deftest data-pattern-queries-improve
  (doseq [[q inputs]
          [['[:find ?tname
              :where
              [?t :track/name ?tname]
              [?t :track/release ?r]
              [?r :release/year 1975]
              [?r :release/format :release.format/lp]]
            []]
           ['[:find ?aname ?year
              :in $ ?label
              :where
              [?a :artist/name ?aname]
              [?r :release/artists ?a]
              [?r :release/year ?year]
              [?r :release/label ?l]
              [?l :label/name ?label]]
            ["Label 2"]]
           ['[:find (count ?t)
              :where
              [?t :track/release ?r]
              [?r :release/artists ?a]
              [?a :artist/country "C3"]
              [?a :artist/active true]]
            []]]]
    (testing (pr-str q)
      (apply check-improved q inputs))))

(def by-artist-rules
  ;; [?a] is a required binding: the rule can't be called with ?a unbound
  '[[(by-artist [?a] ?r) [?r :release/artists ?a]]])

(def complex-queries
  "Queries whose clauses depend on bindings from other clauses in ways that
  aren't visible from a clause's top level vars, as [query inputs]."
  [;; https://github.com/ParkerICI/datomic-query-improver/issues/1 (case 1)
   ['[:find ?a
      :in $ ?country
      :where
      [?a :artist/country ?country]
      (or-join [?a] [(missing? $ ?a :artist/active)])]
    ["C1"]]
   ;; https://github.com/ParkerICI/datomic-query-improver/issues/1 (case 2)
   ['[:find ?a
      :in $ ?country
      :where
      [?a :artist/country ?country]
      (or-join [?a] (not-join [?a] [?a :artist/active _]))]
    ["C1"]]
   ['[:find ?r
      :where
      [?r :release/year ?y]
      [(> ?y 1995)]
      [?r :release/label ?l]
      [?l :label/name "Label 1"]]
    []]
   ['[:find ?t ?mins
      :where
      [?t :track/duration ?d]
      [(quot ?d 60) ?mins]
      [?t :track/release ?r]
      [?r :release/year 1970]]
    []]
   ['[:find ?a
      :where
      [?a :artist/country "C2"]
      (not [?a :artist/active true])]
    []]
   ['[:find ?a
      :where
      [?a :artist/country "C0"]
      (not-join [?a]
        [?r :release/artists ?a]
        [?r :release/year 1999])]
    []]
   ['[:find ?r
      :where
      [?r :release/label ?l]
      [?l :label/name "Label 4"]
      (or [?r :release/format :release.format/lp]
          [?r :release/format :release.format/cd])]
    []]
   ['[:find ?r
      :where
      [?r :release/label ?l]
      [?l :label/name "Label 2"]
      (or-join [?r]
        (and [?r :release/year ?y]
             [(< ?y 1965)])
        [?r :release/format :release.format/digital])]
    []]
   ['[:find ?name
      :in $ %
      :where
      [?a :artist/name "Artist 3"]
      (by-artist ?a ?r)
      [?r :release/name ?name]]
    [by-artist-rules]]
   ['[:find ?t
      :in $ [?y ...]
      :where
      [?t :track/release ?r]
      [?r :release/year ?y]]
    [[1970 1971]]]
   ['[:find ?r
      :in $ [[?aname ?year]]
      :where
      [?r :release/artists ?a]
      [?r :release/year ?year]
      [?a :artist/name ?aname]]
    [[["Artist 1" 1980] ["Artist 2" 1990] ["Artist 3" 1985]]]]
   ['[:find ?a
      :in $
      :where
      [$ ?a :artist/country "C1"]
      [(ground true) ?active]
      [$ ?a :artist/active ?active]]
    []]
   ['[:find ?r ?yy
      :where
      [?r :release/label ?l]
      [?l :label/name "Label 5"]
      [?r :release/year ?y]
      [(+ ?y 1) ?y1]
      [(str ?y1) ?yy]]
    []]])

(deftest complex-queries-stay-valid
  (doseq [[q inputs] complex-queries]
    (testing (pr-str q)
      (check-suggestion q inputs))))
