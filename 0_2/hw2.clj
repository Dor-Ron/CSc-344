;File: hw2.clj
;Authors: Dor Rondel and Nirender
;Instructor: Prof. Daniel Schlegel
;Course: CSc 344
;Institution: SUNY Oswego 

(ns hw2)

; Supplied to us by Prof. Schlegel
(defn notexp [e1] (list 'not e1))

; Redefined orexp and andexp to take variable # of parameters
(defn orexp [& args]
  (reduce
    (fn [e1 e2]
      (list 'or e1 e2)) args))

(defn andexp [& args]
  (reduce
    (fn [e1 e2]
      (list 'and e1 e2)) args))


;Binding provided by Prof. Schlegel
(defn bind-values [l m]
  (map #(cond
          (seq? %) (bind-values % m)
          :default (m % %))
       l))

; make boolean checker function
; Built-in to clojure >1.9 but for backward compatability purposes..
(defn boolean? [x]
 (instance? Boolean x))

;Substitute expressions for simpler forms cases
(defn subExpr [lst]
  (println "\nGoing back up recursively")
  (println lst)
  (if (or (boolean? lst) (symbol? lst)) lst
    (let [[op & args][(first lst) (rest lst)]]
        (cond
          (and (and (= op 'or) (= (count (first args)) 1)) (= (first args) '(true))) true        ;(or true)
          (and (and (= op 'or) (= (count (first args)) 1)) (= (first args) '(false))) false      ;(or false)
          (and (and (= op 'and) (= (count (first args)) 1)) (= (first args) '(true))) true       ;(and true)
          (and (and (= op 'and) (= (count (first args)) 1)) (= (first args) '(false))) false     ;(and false)
          (and (and (and (and (= op 'or) (= (count (first args)) 2) (not (= (first (first args)) true)) (= (second (first args)) false))))) (first (first args))      ;(or <const> false)
          (and (and (and (and (= op 'or) (= (count (first args)) 2) (not (= (second (first args)) true)) (= (first (first args)) false))))) (second (first args))     ;(or <const> false)
          (and (and (and (and (= op 'or) (= (count (first args)) 2) (= (first (first args)) true))))) true                       ;(or true <const>)
          (and (and (and (and (= op 'or) (= (count (first args)) 2) (= (second (first args)) true))))) true                      ;(or <const> true)
          (and (and (and (and (= op 'and) (= (count (first args)) 2) (= (second (first args)) false))))) false                   ;(and <const> false)
          (and (and (and (and (= op 'and) (= (count (first args)) 2) (= (first (first args)) false))))) false                    ;(and false <const>)
          (and (and (and (and (= op 'and) (= (count (first args)) 2) (= (second (first args)) true))))) (first (first args))     ;(and <const> true)
          (and (and (and (and (= op 'and) (= (count (first args)) 2) (= (first (first args)) true))))) (second (first args))     ;(and true <const>)
          (and (and (= op 'not) (= (count (first args)) 1)) (= (first args) '(true))) false      ;(not true)
          (and (and (= op 'not) (= (count (first args)) 1)) (= (first args) '(false))) true      ;(not false)
          (and (= op 'or) (= (count (first args)) 3)) (orexp (second (first args)) (nth (first args) 2) (first (first args)))    ;(or x y z)
          (and (= op 'and) (= (count (first args)) 3)) (andexp (second (first args)) (nth (first args) 2) (first (first args)))  ;(and x y z)
          (and (= op 'not) (= (first (first (first args))) 'and)) (orexp (notexp (second (first (first args)))) (notexp (nth (first (first args)) 2)))  ;(not (and x y))
          (and (= op 'not) (= (first (first (first args))) 'or)) (andexp (notexp (second (first (first args)))) (notexp (nth (first (first args)) 2)))  ;(not (or x y))
          ;---- Own Simplifications
          (and (and (and (and (= op 'and) (= (count (first args)) 2) (= (first (first args)) true) (= (second (first args)) false))))) false            ;(and true false)
          (and (and (and (and (= op 'and) (= (count (first args)) 2) (= (second (first args)) true) (= (first (first args)) false))))) false            ;(and false true)
          (and (and (and (and (= op 'or) (= (count (first args)) 2) (= (first (first args)) true) (= (second (first args)) false))))) true              ;(and true false)
          (and (and (and (and (= op 'or) (= (count (first args)) 2) (= (second (first args)) true) (= (first (first args)) false))))) true              ;(and false true)
          :else lst
      ))))

; Recursively applies subExpr to unevalueted boolean expression list
(defn simplify [lst]
    (println "Going Down Recursively:\n***")
    (println lst)
      (cond
        (and (and (and (and (not (boolean? lst)) (not (seq? (first lst)))) (= (count lst) 3)) (not (seq? (second lst)))) (not (seq? (nth lst 2)))) (subExpr lst)
        (and (and (and (not (boolean? lst)) (not (seq? (first lst)))) (= (count lst) 2)) (not (seq? (second lst)))) (subExpr lst)
        (and (and (and (not (boolean? lst)) (seq? (first lst))) (= (count lst) 2)) (not (seq? (second lst)))) (subExpr (list (second lst) (simplify (first lst))))
        (and (and (and (not (boolean? lst)) (seq? (second lst))) (= (count lst) 2)) (not (seq? (first lst)))) (subExpr (list (first lst) (simplify (second lst))))
        (and (and (and (and (not (boolean? lst)) (seq? (first lst))) (and (= (count lst) 3))) (not (seq? (second lst)))) (not (seq? (nth lst 2)))) (subExpr (list (second lst) (nth lst 2) (simplify (first lst))))
        (and (and (and (and (not (boolean? lst)) (not (seq? (first lst)))) (= (count lst) 3)) (seq? (second lst))) (not (seq? (nth lst 2)))) (subExpr (list (first lst) (simplify (second lst)) (nth lst 2)))
        (and (and (and (and (not (boolean? lst)) (not (seq? (first lst)))) (= (count lst) 3)) (not (seq? (second lst)))) (seq? (nth lst 2))) (subExpr (list (first lst) (second lst) (simplify (nth lst 2))))
        :else lst
  ))

; Main function
(defn evalexp [exp bindings] (simplify (bind-values bindings exp)))
