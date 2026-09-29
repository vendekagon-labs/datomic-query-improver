(ns org.parkerici.datomic-query-improver
  (:require [clojure.set :as set]))

(defn- ->map-form
  "Converts list form Datomic query into map form."
  [q-edn]
  (->> (partition-by #{:find :keys :syms :strs :with :in :where} q-edn)
       (partition-all 2)
       (map (fn [[k v]]
              [(first k) (vec v)]))
       (into {})))


(defn- datalog-var?
  [x]
  (and (symbol? x)
       (.startsWith ^String (name x) "?")))

(defn- src-var?
  [x]
  (and (symbol? x)
       (.startsWith ^String (name x) "$")))

(defn- form->vars
  "All datalog vars occurring anywhere in form."
  [form]
  (into #{} (filter datalog-var?) (tree-seq coll? seq form)))

(def ^:private clause-ops
  '#{or or-join not not-join and})

(defn- clause-info
  "Describes a :where clause by what it needs bound before it can run
  (:requires) and what it binds (:binds). Clauses may be lists or vectors
  (e.g. [or ...] as well as (or ...)), with an optional leading src var.

  For or / or-join / rule invocations these depend on the rule definitions or
  on Datomic's evaluation, so they are taken conservatively from the original
  query: whichever of the clause's vars were already bound where the clause
  originally appeared (`bound-before`) are required, the rest are bound."
  [clause bound-before]
  (let [body (if (src-var? (first clause)) (rest clause) clause)
        [head & more] body
        op (when (symbol? head) (clause-ops head))]
    (cond
      (#{'not} op)
      {:kind :not :requires (form->vars more) :binds #{}}

      (#{'not-join} op)
      {:kind :not-join :requires (form->vars (first more)) :binds #{}}

      (#{'or 'and} op)
      (let [vars (form->vars more)]
        {:kind :or :requires (set/intersection vars bound-before) :binds vars})

      (#{'or-join} op)
      (let [[join-vars] more
            explicitly-required (form->vars (filter vector? join-vars))
            vars (form->vars join-vars)]
        {:kind :or-join
         :requires (set/union explicitly-required (set/intersection vars bound-before))
         :binds vars})

      ;; [(pred ?a ?b)] or [(f ?a ?b) binding]
      (seq? head)
      (let [[_ & args] head]
        {:kind :expression :requires (form->vars args) :binds (form->vars more)})

      ;; (rule-name ?a ?b)
      (seq? clause)
      (let [vars (form->vars more)]
        {:kind :rule :requires (set/intersection vars bound-before) :binds vars})

      :else
      (let [[e a v] body]
        {:kind :data-pattern
         :requires #{}
         :binds (form->vars body)
         :e e
         :attr (when (keyword? a) a)
         :v v}))))

(defn- analyze-clauses
  "clause-info for each clause, in order, given the vars bound by :in."
  [in-vars clauses]
  (loop [bound in-vars
         [clause & more :as clauses] clauses
         infos []]
    (if-not (seq clauses)
      infos
      (let [info (assoc (clause-info clause bound) :clause clause :index (count infos))]
        (recur (set/union bound (:binds info))
               more
               (conj infos info))))))

(defn- min-key*
  "Like min-key, but compares keys with compare, so keys can be vectors."
  [k x & xs]
  (reduce (fn [a b] (if (neg? (compare (k b) (k a))) b a)) x xs))

(defn db-stats->attr-counts
  "Given database stats as returned by datomic client:
  `datomic-client-api/db-stats`, returns attr-counts map in form that
  `suggest` arg accepts."
  [{:keys [attrs]}]
  (->> (for [[a {:keys [count]}] attrs]
         [a count])
       (into {})))


(defn- known?
  "True if x is a constant or an already bound var."
  [bound x]
  (and (some? x)
       (not= '_ x)
       (or (not (symbol? x))
           (contains? bound x))))

(defn- estimated-datoms
  "Estimates the datoms a clause will match given the vars bound so far.

  attr-stats values are either a datom count, used as is, or a map of
  {:count n :distinct-values v :distinct-entities e} (as computed by
  dev/statistics.clj), from which the datoms per bound entity or value are
  estimated."
  [attr-stats bound {:keys [kind e attr v]}]
  (let [stats (get attr-stats attr)]
    (cond
      (not= :data-pattern kind) 0
      ;; Note: defaults to counting "0" for any attributes not included in stats
      (nil? stats) 0
      (number? stats) stats
      :else
      (let [{:keys [count distinct-values distinct-entities]} stats
            e-known (known? bound e)
            v-known (known? bound v)]
        (cond
          (and e-known v-known) 1
          (and e-known distinct-entities) (/ count (max 1 distinct-entities))
          (and v-known distinct-values) (/ count (max 1 distinct-values))
          :else count)))))

(defn- score
  "Ranks a clause (as per clause-info) that is ready to be placed next, given
  the vars bound so far; lowest is best. Heuristics, in order: join along
  (fewest new vars), most selective (fewest estimated datoms), then the
  clause's original position."
  [attr-stats bound {:keys [binds index] :as info}]
  [(count (set/difference binds bound))
   (estimated-datoms attr-stats bound info)
   index])

(defn suggest
  "Given dictionary of attribute counts (as per stats/retrieve or derived from
  db-stats) and list or map form query, returns a map form query in which
  :where clauses have been (possibly) re-ordered into a more efficient ordering.
  Uses two heuristics: join along, and most restrictive clauses first.

  Attribute counts may instead be maps of {:count :distinct-values
  :distinct-entities} (see dev/statistics.clj), which lets selectivity account
  for clauses whose entity or value is already bound or a constant.

  A clause is only placed once the vars it needs bound are bound (e.g. the
  inputs of a predicate, or the join vars of a not-join), so a valid query
  stays valid. Ties keep the clauses' original relative order."
  [attr-stats q-edn]
  (let [q (if (map? q-edn)
            q-edn
            (->map-form q-edn))
        {:keys [where in]} q
        in-vars (form->vars in)]
    (loop [bound in-vars
           remaining (analyze-clauses in-vars where)
           out-ordering []]
      (if-not (seq remaining)
        (assoc q :where out-ordering)
        (let [ready (filter #(set/subset? (:requires %) bound) remaining)
              best (if (seq ready)
                     (apply min-key* #(score attr-stats bound %) ready)
                     ;; nothing's requirements are met (e.g. a predicate that the original
                     ;; query placed before its inputs are bound): keep original order
                     (first remaining))]
          (recur (set/union bound (:binds best))
                 (remove #(= (:index %) (:index best)) remaining)
                 (conj out-ordering (:clause best))))))))
