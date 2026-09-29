(ns org.parkerici.datomic-query-improver.statistics-test
  (:require [clojure.test :refer [deftest is]]
            [datomic.api :as d]
            [statistics]
            [org.parkerici.datomic-query-improver.test-db :as db]))

(deftest stored-stats-match-computed-stats
  (let [[uri conn] (db/create-db)]
    (try
      (statistics/update! conn)
      (let [db (d/db conn)
            stored (statistics/retrieve db)]
        (is (= (db/attr-stats db)
               (select-keys stored (map :db/ident db/schema))))
        (is (= (set (map :db/ident db/schema)) (set (keys stored)))
            "only installed, non-schema, non-stats attributes get stats"))
      (statistics/update! conn)
      (is (= (db/attr-stats (d/db conn))
             (select-keys (statistics/retrieve (d/db conn)) (map :db/ident db/schema)))
          "updating again doesn't change the stats")
      (finally
        (d/delete-database uri)))))
