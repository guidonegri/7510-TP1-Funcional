(ns logical-interpreter-unit-test
  (:require [clojure.test :refer :all]
            [logical-interpreter :refer :all]))

(deftest get-name-of-test
  ;"get-name-of" returns the name of a query, fact or rule.

  (testing "get name of a query"
    (is (= (get-name-of "varon(jorge)" query-pattern)
           "varon"))) 
           
  (testing "get name of a fact with one parameter"
    (is (= (get-name-of "mujer(maria)" fact-pattern)
           "mujer")))
           
  (testing "get name of a fact with with more than one parameter"
    (is (= (get-name-of "add(one, one, two)" fact-pattern)
           "add")))

  (testing "get name of a rule with one fact"
    (is (= (get-name-of "subtract(X, Y, Z) :- add(Y, Z, X)" rule-pattern)
           "subtract")))

  (testing "get name of a rule with more than one fact"
    (is (= (get-name-of "hijo(X, Y) :- varon(X), padre(Y, X)" rule-pattern)
           "hijo")))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest get-params-of-test
  ;"get-params-of" returns a vector with the params of a query or rule.
  
  (testing "get params of a query"
    (is (= (get-params-of "varon(jorge)" query-pattern)
           ["jorge"]))) 
           
  (testing "get params of a query with two parameters"
    (is (= (get-params-of "padre(juan, pepa)" query-pattern)
           ["juan" "pepa"])))
           
  (testing "get params of a query with three parameters"
    (is (= (get-params-of "add(two, two, one)" query-pattern)
           ["two" "two" "one"])))

  (testing "get params of a rule with one parameter"
    (is (= (get-params-of "hijo(X) :- varon(X), hermano(X)" rule-pattern)
           ["X"])))

  (testing "get params of a rule with more than one parameter"
    (is (= (get-params-of "subtract(X, Y, Z) :- add(Y, Z, X)" rule-pattern)
           ["X" "Y" "Z"])))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest get-facts-of-rule-test
  ;"get-facts-of-rule" returns the facts of a rule.
  
  (testing "get facts of a rule with one fact"
    (is (= (get-facts-of-rule "subtract(X, Y, Z) :- add(Y, Z, X)")
           "add(Y, Z, X)"))) 
           
  (testing "get facts of a rule with two facts"
    (is (= (get-facts-of-rule "hijo(X, Y) :- varon(X), padre(Y, X)")
           "varon(X), padre(Y, X)"))) 
)

;;;;;;;;;;;;;;;;;;;;;;;;;;

(def valid-database-without-rules ["mujer(maria)" "varon(jorge)"])
(def valid-database-with-rules ["mujer(maria)" "varon(jorge)" "subtract(X, Y, Z) :- add(Y, Z, X)"])
(def valid-database-without-facts ["hijo(X, Y) :- varon(X), padre(Y, X)" "hija(X, Y) :- mujer(X), padre(Y, X)"])
(def invalid-database ["mujer(maria)" "varon"])
(def invalid-database-empty [" "])

(deftest is-invalid-database-test
  (testing "is invalid database (db without rules) should be false"
    (is (= (is-invalid-database valid-database-without-rules)
           false))) 
           
  (testing "is invalid database (db with rules) should be false"
    (is (= (is-invalid-database valid-database-with-rules)
           false))) 
           
  (testing "is invalid database (db without facts) should be false"
    (is (= (is-invalid-database valid-database-without-facts)
           false))) 
           
  (testing "is invalid database (db invalid-database) should be true"
    (is (= (is-invalid-database invalid-database)
           true))) 

  (testing "is invalid database (db invalid-database-empty) should be true"
    (is (= (is-invalid-database invalid-database-empty)
           true))) 
)

;;;;;;;;;;;;;;;;;;;;;;;;;;

(def invalid-query "mujer")
(def valid-query "mujer(maria)")

(deftest is-invalid-query-test
  (testing "is invalid query should be true"
    (is (= (is-invalid-query invalid-query)
           true)))

  (testing "is invalid query should be false"
    (is (= (is-invalid-query valid-query)
           false)))           
           
)

;;;;;;;;;;;;;;;;;;;;;;;;;;

;(def valid-database-without-rules ["mujer(maria)" "varon(jorge)"])
;(def valid-database-with-rules ["mujer(maria)" "varon(jorge)" "subtract(X, Y, Z) :- add(Y, Z, X)"])
;(def invalid-database-empty [" "])

(deftest is-rule-in-database-test
  (testing "is rule in database (db with rules, query about rule) should be true"
    (is (= (is-rule-in-database "subtract(X, Y, Z)" valid-database-with-rules)
           true)))

  (testing "is rule in database (db with rules, query about fact) should be false"
    (is (= (is-rule-in-database "mujer(maria)" valid-database-with-rules)
           false)))
           
  (testing "is rule in database (db without rules, query about rule) should be false"
    (is (= (is-rule-in-database "subtract(X, Y, Z)" valid-database-without-rules)
           false)))
           
  (testing "is rule in database (db without rules, query about fact) should be false"
    (is (= (is-rule-in-database "mujer(maria)" valid-database-without-rules)
           false)))

  (testing "is rule in database (db empty) should be false"
    (is (= (is-rule-in-database "subtract(X, Y, Z)" invalid-database-empty)
           false)))

)

;;;;;;;;;;;;;;;;;;;;;;;;;;

;(def valid-database-without-rules ["mujer(maria)" "varon(jorge)"])
;(def valid-database-with-rules ["mujer(maria)" "varon(jorge)" "subtract(X, Y, Z) :- add(Y, Z, X)"])
;(def valid-database-without-facts ["hijo(X, Y) :- varon(X), padre(Y, X)" "hija(X, Y) :- mujer(X), padre(Y, X)"])
;(def invalid-database-empty [" "])

(deftest is-fact-in-database-test
  (testing "is fact in database (db with facts and rules, query about rule) should be false"
    (is (= (is-fact-in-database "subtract(X, Y, Z)" valid-database-with-rules)
           false)))

  (testing "is fact in database (db with facts and rules, query about fact) should be true"
    (is (= (is-fact-in-database "mujer(maria)" valid-database-with-rules)
           true)))
           
  (testing "is fact in database (db without rules, query about fact) should be true"
    (is (= (is-fact-in-database "varon(jorge)" valid-database-without-rules)
           true)))
           
  (testing "is rule in database (db empty) should be false"
    (is (= (is-rule-in-database "mujer(maria)" invalid-database-empty)
           false)))

)

;;;;;;;;;;;;;;;;;;;;;;;;;;

;(def valid-database-without-rules ["mujer(maria)" "varon(jorge)"])
;(def valid-database-with-rules ["mujer(maria)" "varon(jorge)" "subtract(X, Y, Z) :- add(Y, Z, X)"])
;(def valid-database-without-facts ["hijo(X, Y) :- varon(X), padre(Y, X)" "hija(X, Y) :- mujer(X), padre(Y, X)"])
;(def invalid-database-empty [" "])

(deftest query-fact-test
  (testing "query matches in database (db only with facts)"
    (is (= (query-fact valid-database-without-rules "mujer(maria)")
           true)))
           
  (testing "query doesn't match in database (db only with facts)"
    (is (= (query-fact valid-database-without-rules "mujer(ernestina)")
           false)))

  (testing "query matches in database (db with facts and rules)"
    (is (= (query-fact valid-database-with-rules "varon(jorge)")
           true)))
           
  (testing "query doesn't match in database (db with facts and rules)"
    (is (= (query-fact valid-database-with-rules "varon(federico)")
           false)))

  (testing "query doesn't match in database (db without facts)"
    (is (= (query-fact valid-database-without-facts "varon(jorge)")
           false)))
           
  (testing "query doesn't match in database (db empty)"
    (is (= (query-fact invalid-database-empty "mujer(maria)")
           false)))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;
  
(deftest replace-rule-with-query-params-test
  (testing "rule with one fact"
    (is (= (replace-rule-with-query-params "subtract(X, Y, Z) :- add(Y, Z, X)" 0 ["X" "Y" "Z"] ["one" "one" "two"])
           "subtract(one, one, two) :- add(one, two, one)")))

  (testing "rule with more than one fact"
    (is (= (replace-rule-with-query-params "hijo(X, Y) :- varon(X), padre(Y, X)" 0 ["X" "Y"] ["roman" "riquelme"])
           "hijo(roman, riquelme) :- varon(roman), padre(riquelme, roman)")))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;
  
(def database-parent ["varon(pedro)" "padre(juan, pedro)" "hijo(X, Y) :- varon(X), padre(Y, X)"])
(def database-number ["add(zero, zero, zero)" "add(zero, one, one)" "subtract(X, Y, Z) :- add(Y, Z, X)"])

(deftest evaluate-facts-of-rule-test
  (testing "rule with one fact, valid query"
	;query is "subtract(one, zero, one)"
    (is (= (evaluate-facts-of-rule database-number ["add(zero, one, one)"])
           [true])))
           
  (testing "rule with one fact, valid query, but fact not in db"
	;query is "subtract(two, zero, two)"
    (is (= (evaluate-facts-of-rule database-number ["add(zero, two, two)"])
           [false])))

  (testing "rule with more than one fact, valid query"
	;query is "hijo(pedro, juan)"
    (is (= (evaluate-facts-of-rule database-parent ["varon(pedro)" "padre(juan, pedro)"])
           [true true])))
           
  (testing "rule with more than one fact, but one fact not in db"
	;query is "hijo(pedro, jorge)"
    (is (= (evaluate-facts-of-rule database-parent ["varon(pedro)" "padre(jorge, pedro)"])
           [true false])))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;

;(def database-parent ["varon(pedro)" "padre(juan, pedro)" "hijo(X, Y) :- varon(X), padre(Y, X)"])
;(def database-number ["add(zero, zero, zero)" "add(zero, one, one)" "subtract(X, Y, Z) :- add(Y, Z, X)"])
  
(deftest query-rule-test
  (testing "rule with one fact, valid query, fact is true"
    (is (= (query-rule database-number "subtract(one, zero, one)")
           true)))
           
  (testing "rule with one fact, valid query, but fact not in db"
    (is (= (query-rule database-number "subtract(two, zero, two)")
           false)))
           
  (testing "rule with more than one fact, valid query, all rule facts are true"
    (is (= (query-rule database-parent "hijo(pedro, juan)")
           true)))
           
  (testing "rule with more than one fact, valid query, but one rule fact is false"
    (is (= (query-rule database-parent "hijo(pedro, jorge)")
           false)))
)
