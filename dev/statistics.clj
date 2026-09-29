(ns statistics
  (:require [datomic.api :as d]))

(def schema
  [{:db/ident :query-optimizer.statistics/datom-count
    :db/valueType :db.type/long
    :db/cardinality :db.cardinality/one
    :db/doc "Metadata installed by datomic-query-optimizer to track counts of attributes."}
   {:db/ident :query-optimizer.statistics/distinct-values
    :db/valueType :db.type/long
    :db/cardinality :db.cardinality/one
    :db/doc "Metadata installed by datomic-query-optimizer: distinct values of an attribute."}
   {:db/ident :query-optimizer.statistics/distinct-entities
    :db/valueType :db.type/long
    :db/cardinality :db.cardinality/one
    :db/doc "Metadata installed by datomic-query-optimizer: distinct entities with an attribute."}])

(defn schema-installed? [db]
  (= (count schema)
     (count (d/q '[:find [?e ...]
                   :in $ [?ident ...]
                   :where
                   [?e :db/ident ?ident]]
                 db (map :db/ident schema)))))

(defn install-schema [conn]
  (let [db (d/db conn)]
    (when-not (schema-installed? db)
      @(d/transact conn schema))))

(defn all-attrs [db]
  (d/q '[:find [?e ...]
         :where
         [_ :db.install/attribute ?e]]
       db))

(defn eid->ident [db eid]
  (-> (d/pull db '[:db/ident] eid)
      (:db/ident)))

(defn schema-attr?
  "Sort of a heuristic, but covers known cases (and won't usually conflict)."
  [db eid]
  (when-let [ident (eid->ident db eid)]
   (let [ident-ns (namespace ident)]
    (or (= "db" ident-ns)
        (= "fressian" ident-ns)
        (.startsWith ^String ident-ns "db.")))))

(defn stats-attr?
  [db eid]
  ((set (map :db/ident schema)) (eid->ident db eid)))

(defn installed-attrs [db]
  (->> (all-attrs db)
       (remove (partial schema-attr? db))
       (remove (partial stats-attr? db))))

(defn attr->stats
  "Datom count, distinct values and distinct entities for attr, in one pass."
  [db attr]
  (let [[n vs es] (reduce (fn [[n vs es] datom]
                            [(inc n) (conj! vs (:v datom)) (conj! es (:e datom))])
                          [0 (transient #{}) (transient #{})]
                          (d/datoms db :aevt attr))]
    {:count n
     :distinct-values (count (persistent! vs))
     :distinct-entities (count (persistent! es))}))

(defn attribute-stats [db]
  (let [attrs (installed-attrs db)]
    (zipmap (map (partial eid->ident db) attrs)
            (map (partial attr->stats db) attrs))))

(defn create-tx-data [db]
  (for [[attr {:keys [count distinct-values distinct-entities]}] (attribute-stats db)]
    {:db/id attr
     :query-optimizer.statistics/datom-count count
     :query-optimizer.statistics/distinct-values distinct-values
     :query-optimizer.statistics/distinct-entities distinct-entities}))

(defn update! [conn]
  (install-schema conn)
  (let [db (d/db conn)
        updates (create-tx-data db)]
    @(d/transact conn updates)))


(defn retrieve
  "Stored stats per attribute ident, in the form `suggest` accepts: a map of
  {:count :distinct-values :distinct-entities}, or just the datom count for
  stats stored before distinct counts were tracked."
  [db]
  (->> (d/q '[:find ?attr ?count ?distinct-values ?distinct-entities
              :where
              [?a :query-optimizer.statistics/datom-count ?count]
              [?a :db/ident ?attr]
              [(get-else $ ?a :query-optimizer.statistics/distinct-values -1) ?distinct-values]
              [(get-else $ ?a :query-optimizer.statistics/distinct-entities -1) ?distinct-entities]]
            db)
       (map (fn [[attr count distinct-values distinct-entities]]
              [attr (if (neg? distinct-values)
                      count
                      {:count count
                       :distinct-values distinct-values
                       :distinct-entities distinct-entities})]))
       (into {})))

(comment
  (update! conn))
