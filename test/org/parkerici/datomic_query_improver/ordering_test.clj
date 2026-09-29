(ns org.parkerici.datomic-query-improver.ordering-test
  "Generates queries over the test db, and compares the improver's ordering
  against the measured cost of other orderings of the same clauses."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [org.parkerici.datomic-query-improver :as sut]
            [org.parkerici.datomic-query-improver.gen :as g]
            [org.parkerici.datomic-query-improver.test-db :as db]))

(use-fixtures :once db/with-db)

(defn- same-clauses? [q1 q2]
  (= (frequencies (:where q1)) (frequencies (:where q2))))

(defspec suggestions-are-equivalent-queries 100
  (prop/for-all [q (gen/bind g/gen-query
                             (fn [q] (gen/fmap #(assoc q :where (vec %))
                                               (gen/shuffle (:where q)))))]
    (let [expected (binding [db/*timeout-ms* 2000] (db/run q))]
      (every? (fn [stats]
                (let [suggested (sut/suggest stats q)
                      actual (binding [db/*timeout-ms* 2000] (db/run suggested))]
                  (and (same-clauses? q suggested)
                       (nil? (:error actual))
                       (or (= :timeout (:error expected))
                           (= (:result expected) (:result actual))))))
              [db/*attr-counts* db/*attr-stats*]))))

(def ^:private n-queries 20)

(defn- measure [q]
  (let [costs (g/ordering-costs q)]
    {:q q
     :best (first (first costs))
     :median (first (nth costs (quot (count costs) 2)))
     :worst-q (second (peek costs))}))

(deftest improves-worst-orderings
  (let [measured (map #(measure (gen/generate g/gen-query 30 %)) (range n-queries))
        suggested (for [{:keys [worst-q] :as m} measured]
                    (assoc m :suggested (g/cost (sut/suggest db/*attr-stats* worst-q))))]
    (doseq [{:keys [q best median suggested]} suggested]
      (testing (pr-str (:where q))
        (is (some? suggested) "suggested ordering should be valid")
        (is (< suggested g/timed-out) "suggested ordering should not time out")
        (is (<= suggested median)
            (str "suggested ordering (" suggested " rows) should be no worse than the median"
                 " ordering (" median "); best is " best))))
    (testing "in aggregate, close to the best orderings"
      (let [total-best (reduce + (map :best suggested))
            total-suggested (reduce + (map :suggested suggested))]
        (is (<= total-suggested (* 1.10 total-best))
            (str total-suggested " rows for suggested orderings vs " total-best " for the best"))))))
