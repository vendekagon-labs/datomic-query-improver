(ns org.parkerici.datomic-query-improver.test-db
  "In-memory Datomic database with synthetic, deliberately skewed music data
  (few labels and countries, many releases and tracks) so that clause order
  makes a measurable difference to query cost."
  (:require [datomic.api :as d])
  (:import (java.util Random)))

(def schema
  [{:db/ident :artist/name :db/valueType :db.type/string :db/cardinality :db.cardinality/one
    :db/index true}
   {:db/ident :artist/country :db/valueType :db.type/string :db/cardinality :db.cardinality/one
    :db/index true}
   {:db/ident :artist/active :db/valueType :db.type/boolean :db/cardinality :db.cardinality/one}
   {:db/ident :label/name :db/valueType :db.type/string :db/cardinality :db.cardinality/one
    :db/index true}
   {:db/ident :release/name :db/valueType :db.type/string :db/cardinality :db.cardinality/one
    :db/index true}
   {:db/ident :release/year :db/valueType :db.type/long :db/cardinality :db.cardinality/one
    :db/index true}
   {:db/ident :release/artists :db/valueType :db.type/ref :db/cardinality :db.cardinality/many}
   {:db/ident :release/label :db/valueType :db.type/ref :db/cardinality :db.cardinality/one}
   {:db/ident :release/format :db/valueType :db.type/keyword :db/cardinality :db.cardinality/one}
   {:db/ident :track/name :db/valueType :db.type/string :db/cardinality :db.cardinality/one
    :db/index true}
   {:db/ident :track/release :db/valueType :db.type/ref :db/cardinality :db.cardinality/one}
   {:db/ident :track/duration :db/valueType :db.type/long :db/cardinality :db.cardinality/one}])

(def n-countries 5)
(def n-labels 10)
(def n-artists 100)
(def n-releases 600)
(def tracks-per-release 6)

(defn- data []
  (let [rnd (Random. 42)
        pick (fn [n] (.nextInt rnd n))]
    (concat
      (for [l (range n-labels)]
        {:db/id (str "label-" l) :label/name (str "Label " l)})
      (for [a (range n-artists)]
        (cond-> {:db/id (str "artist-" a)
                 :artist/name (str "Artist " a)
                 :artist/country (str "C" (mod a n-countries))}
          ;; only some artists have the attribute, for missing? / not tests
          (even? a) (assoc :artist/active (zero? (mod a 4)))))
      (for [r (range n-releases)]
        {:db/id (str "release-" r)
         :release/name (str "Release " r)
         :release/year (+ 1960 (pick 40))
         :release/artists (vec (distinct [(str "artist-" (pick n-artists))
                                          (str "artist-" (pick n-artists))]))
         :release/label (str "label-" (pick n-labels))
         :release/format ([:release.format/lp :release.format/cd :release.format/digital] (pick 3))})
      (for [r (range n-releases)
            t (range tracks-per-release)]
        {:track/name (str "Track " r "-" t)
         :track/release (str "release-" r)
         :track/duration (+ 60 (pick 400))}))))

(def ^:dynamic *db* nil)

(defn attr-counts
  "Datom count per attribute, the stats form `suggest` accepts."
  [db]
  (into {}
        (for [{:keys [db/ident]} schema]
          [ident (count (seq (d/datoms db :aevt ident)))])))

(defn attr-stats
  "Datom, distinct value and distinct entity counts per attribute, the richer
  stats form `suggest` accepts."
  [db]
  (into {}
        (for [{:keys [db/ident]} schema
              :let [datoms (seq (d/datoms db :aevt ident))]]
          [ident {:count (count datoms)
                  :distinct-values (count (distinct (map :v datoms)))
                  :distinct-entities (count (distinct (map :e datoms)))}])))

(def ^:dynamic *attr-counts* nil)
(def ^:dynamic *attr-stats* nil)

(defn create-db
  "Creates and populates an in-memory database, returning [uri conn]."
  []
  (let [uri (str "datomic:mem://query-improver-test-" (random-uuid))]
    (d/create-database uri)
    (let [conn (d/connect uri)]
      @(d/transact conn schema)
      @(d/transact conn (data))
      [uri conn])))

(defn with-db
  "clojure.test fixture binding *db*, *attr-counts* and *attr-stats*."
  [f]
  (let [[uri conn] (create-db)
        db (d/db conn)]
    (try
      (binding [*db* db
                *attr-counts* (attr-counts db)
                *attr-stats* (attr-stats db)]
        (f))
      (finally
        (d/delete-database uri)))))

(def ^:dynamic *timeout-ms*
  "Datomic query timeout used by run, if any."
  nil)

(defn- timeout? [e]
  (some #(instance? java.util.concurrent.TimeoutException %)
        (take-while some? (iterate ex-cause e))))

(defn run
  "Runs query q (list or map form) with inputs, returning {:result .. :rows ..}
  where :rows is the total rows produced by every clause per Datomic's
  query-stats: a deterministic measure of how much work the ordering caused.
  Errors are returned as {:error ..} rather than thrown, and running out of
  time (see *timeout-ms*) as {:error :timeout}."
  [q & inputs]
  (let [arg-map {:query q :args (cons *db* inputs)}]
    (try
      ;; Datomic doesn't enforce :timeout when collecting :query-stats, so
      ;; check the query completes in time before running it again for stats.
      (when *timeout-ms*
        (d/query (assoc arg-map :timeout *timeout-ms*)))
      (let [{:keys [ret query-stats]} (d/query (assoc arg-map :query-stats true))]
        {:result ret
         :rows (reduce + (for [phase (:phases query-stats)
                               clause (:clauses phase)]
                           (:rows-out clause 0)))})
      (catch Throwable e
        (if (timeout? e)
          {:error :timeout}
          {:error (or (:db/error (ex-data e)) (ex-message e))})))))
