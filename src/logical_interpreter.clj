(ns logical-interpreter)

(require '[clojure.string :as str])

;PATTERNS:
(def query-pattern (re-pattern "(.*)\\((.*)\\)"))
(def fact-pattern (re-pattern "(.*)\\((.*)\\)"))
(def rule-pattern (re-pattern "(.*)\\((.*)\\) :- (.*)"))

;;;;;;;;;;;;;;;;;;;;;;;;;;

;GETTERS:
(defn get-name-of
  ;Returns the name of a query, fact or rule.
  [item pattern]
  (second (re-find pattern item))
)

(defn get-params-of
  ;Returns vector with the params of a query or rule.
  [item pattern]
  (str/split (nth (re-find pattern item) 2) #", " )
)

(defn get-facts-of-rule
  ;Returns the facts of a rule.
  [rule]
  (nth (re-find rule-pattern rule) 3)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;

;VALIDATES:
(defn is-invalid-database
  [db]
  (distinct? (count (filter #(re-matches fact-pattern %) db)) (count db) )
)

(defn is-invalid-query
  [query]
  (nil? (re-matches fact-pattern query))
)

(defn is-rule-in-database
  [query db]
  (let [query-name (get-name-of query query-pattern)]
    (= (count (filter (fn[item](= query-name (get-name-of item rule-pattern))) db)) 1) ;=1 because rule is unic
  )
)

(defn is-fact-in-database
  [query db]
  (let [query-name (get-name-of query query-pattern)]
    (> (count (filter (fn[item](= query-name (get-name-of item fact-pattern))) db)) 0 ) ;>0 because 0 because there may be more than one fact with the same name
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;

;FOR FACTS:
(defn query-fact
  ;If query matches with fact in DB, returns True. If not, False.
  [db query]
  (= (count (filter (fn [fact] (= query fact)) db) ) 1)
)

;FOR RULES:
(defn replace-rule-with-query-params
  ;Recursive function. Returns the complete rule with the query parameters.
  [rule index params-db params-query]
  (let [param (re-pattern (nth params-db index))
        rule-with-query-params (str/replace rule param (nth params-query index))]
    (cond
	    (= index (- (count params-db) 1) ) rule-with-query-params
	    :else (replace-rule-with-query-params rule-with-query-params (+ index 1) params-db params-query)
    )
  ) 
)

(defn evaluate-facts-of-rule
  ;Evaluates each fact of rule facts. Returns a list with the facts evaluate.
  [db rule-facts]
  (for [fact rule-facts
      :let [result (query-fact db fact)]
      ]
  result)
)

(defn query-rule
  ;If all facts of rule are true, returns True. If not, False.
  [db query]
  (let [name-query (get-name-of query query-pattern)
        rule (first (filter (fn[item](= name-query (get-name-of item rule-pattern))) db )) ;Complete rule
        name-rule (get-name-of rule rule-pattern)
        
        params-query-rule (get-params-of query query-pattern)
        params-db-rule (get-params-of rule rule-pattern)
        
        rule-with-query-params(replace-rule-with-query-params rule 0 params-db-rule params-query-rule)
        rule-facts (get-facts-of-rule rule-with-query-params)
        
        rule-facts-r (str/replace rule-facts #"\)," ")-") ;Change facts rule separator: , for -
        rule-facts-s (str/split rule-facts-r #"- ")] ;Split with the new separator
    
    (reduce (fn [a b] (and a b)) (evaluate-facts-of-rule db rule-facts-s) )    
  ) 
)

;;;;;;;;;;;;;;;;;;;;;;;;;;

;MAIN:
(defn evaluate-query
  "Returns true if the rules and facts in database imply query, false if not. If
  either input can't be parsed, returns nil"
  [db-file query]
  (let [db-str (slurp (java.io.FileReader. db-file))
        db (str/split db-str #".\n")]                
    (cond
      (is-invalid-database db) nil
      (is-invalid-query query) nil
      (is-fact-in-database query db) (query-fact db query)
      (is-rule-in-database query db) (query-rule db query)
      :else false
    ) 
  )  
)
