(ns org.parkerici.datomic-query-improver.gen
  "test.check generator for queries over the test-db schema, and helpers to
  measure the cost of every ordering of a query's clauses."
  (:require [clojure.test.check.generators :as gen]
            [clojure.math.combinatorics :as combo]
            [org.parkerici.datomic-query-improver.test-db :as db]))

(defn- one-of-or-none
  "Generates [] or a vector of one clause from gens."
  [gens]
  (gen/one-of [(gen/return []) (gen/fmap vector (gen/one-of gens))]))

(def gen-label-facts
  (one-of-or-none
    [(gen/fmap (fn [n] ['?l :label/name (str "Label " n)]) (gen/choose 0 9))]))

(def gen-artist-facts
  (one-of-or-none
    [(gen/fmap (fn [n] ['?a :artist/name (str "Artist " n)]) (gen/choose 0 99))
     (gen/fmap (fn [n] ['?a :artist/country (str "C" n)]) (gen/choose 0 4))
     (gen/fmap (fn [b] ['?a :artist/active b]) gen/boolean)]))

(def gen-release-facts
  (gen/one-of
    [(gen/return [])
     (gen/fmap (fn [y] [['?r :release/year y]]) (gen/choose 1960 1999))
     (gen/fmap (fn [[op y]] [['?r :release/year '?y] [(list op '?y y)]])
               (gen/tuple (gen/elements '[< >]) (gen/choose 1960 1999)))
     (gen/fmap (fn [f] [['?r :release/format f]])
               (gen/elements [:release.format/lp :release.format/cd :release.format/digital]))
     (gen/return '[(not [?r :release/format :release.format/cd])])
     (gen/return '[(or [?r :release/format :release.format/lp]
                       [?r :release/format :release.format/digital])])]))

(def gen-track-facts
  (gen/one-of
    [(gen/return [])
     (gen/fmap (fn [d] [['?t :track/duration '?d] [(list '< '?d d)]]) (gen/choose 60 460))]))

(def gen-query
  "Connected queries through ?r (a release) to its label ?l, artists ?a and
  tracks ?t, with a mix of selective and unselective facts."
  (gen/let [with-label gen/boolean
            with-artist gen/boolean
            with-track gen/boolean
            label-facts gen-label-facts
            artist-facts gen-artist-facts
            release-facts gen-release-facts
            track-facts gen-track-facts
            artist-not-join? (gen/frequency [[4 (gen/return false)] [1 (gen/return true)]])]
    (let [with-label (or with-label (not (or with-artist with-track)))
          where (concat
                  release-facts
                  (when with-label (into '[[?r :release/label ?l]] label-facts))
                  (when with-artist
                    (concat '[[?r :release/artists ?a]]
                            artist-facts
                            (when artist-not-join?
                              '[(not-join [?a] [?a :artist/active false])])))
                  (when with-track (into '[[?t :track/release ?r]] track-facts)))]
      {:find (if with-track '[(count ?t)] '[(count ?r)])
       :where (vec where)})))

(defn orderings
  "Orderings of q's :where clauses, as queries: all of them when there are at
  most max-n, otherwise a deterministic sample of max-n."
  [q max-n]
  (let [perms (combo/permutations (:where q))
        perms (if (<= (combo/count-permutations (:where q)) max-n)
                perms
                (let [rnd (java.util.Random. 7)]
                  (repeatedly max-n #(let [l (java.util.ArrayList. ^java.util.Collection (:where q))]
                                       (java.util.Collections/shuffle l rnd)
                                       (vec l)))))]
    (map #(assoc q :where (vec %)) (distinct perms))))

(def timed-out
  "Cost assigned to orderings Datomic couldn't complete within the timeout."
  Long/MAX_VALUE)

(defn cost
  "Rows produced running q, timed-out if it ran out of time, nil if invalid."
  [q]
  (binding [db/*timeout-ms* (or db/*timeout-ms* 500)]
    (let [{:keys [rows error]} (db/run q)]
      (cond
        rows rows
        (= error :timeout) timed-out))))

(defn ordering-costs
  "Costs of up to 120 orderings of q, as a sorted vector of [cost ordering]
  for the orderings Datomic accepts."
  [q]
  (->> (orderings q 120)
       (keep (fn [o] (when-let [c (cost o)] [c o])))
       (sort-by first)
       (vec)))
