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
