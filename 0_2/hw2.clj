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
          ;(or true)
          (and
            (= op 'or)
            (= (count (first args)) 1)
            (= (first args) '(true))) true

          ;(or false)
          (and
            (= op 'or)
            (= (count (first args)) 1)
            (= (first args) '(false))) false

          ;(and true)
          (and
            (= op 'and)
            (= (count (first args)) 1)
            (= (first args) '(true))) true

          ;(and false)
          (and
            (= op 'and)
            (= (count (first args)) 1)
            (= (first args) '(false))) false

          ;(or <const> false)
          (and
            (= op 'or)
            (= (count (first args)) 2)
            (not (= (first (first args)) true))
            (= (second (first args)) false)) (first (first args))

          ;(or false <const>)
          (and
            (= op 'or)
            (= (count (first args)) 2)
            (not (= (second (first args)) true))
            (= (first (first args)) false)) (second (first args))

          ;(or true <const>)
          (and
            (= op 'or)
            (= (count (first args)) 2)
            (= (first (first args)) true)) true

          ;(or <const> true)
          (and
            (= op 'or)
            (= (count (first args)) 2)
            (= (second (first args)) true)) true

          ;(and <const> false)
          (and
            (= op 'and)
            (= (count (first args)) 2)
            (= (second (first args)) false)) false

          ;(and false <const>)
          (and
            (= op 'and)
            (= (count (first args)) 2)
            (= (first (first args)) false)) false

          ;(and <const> true)
          (and
            (= op 'and)
            (= (count (first args)) 2)
            (= (second (first args)) true)) (first (first args))

          ;(and true <const>)
          (and
            (= op 'and)
            (= (count (first args)) 2)
            (= (first (first args)) true)) (second (first args))

          ;(not true)
          (and
            (= op 'not)
            (= (count (first args)) 1)
            (= (first args) '(true))) false

          ;(not false)
          (and
            (= op 'not)
            (= (count (first args)) 1)
            (= (first args) '(false))) true

          ;(or x y z)
          (and
            (= op 'or)
            (= (count (first args)) 3)) (orexp (second (first args)) (nth (first args) 2) (first (first args)))

          ;(and x y z)
          (and
            (= op 'and)
            (= (count (first args)) 3)) (andexp (second (first args)) (nth (first args) 2) (first (first args)))

          ;(not (and x y))
          (and
            (= op 'not)
            (= (first (first (first args))) 'and)) (orexp (notexp (second (first (first args)))) (notexp (nth (first (first args)) 2)))

          ;(not (or x y))
          (and
            (= op 'not)
            (= (first (first (first args))) 'or)) (andexp (notexp (second (first (first args)))) (notexp (nth (first (first args)) 2)))
          :else lst
      ))))


; Recursively applies subExpr to unevalueted boolean expression list
(defn simplify [lst]
    (println "Going Down Recursively:\n***")
    (println lst)
      (cond
        ; Base Case 1, Count == 3, no index is a list
        (and
          (not (boolean? lst))
          (not (seq? (first lst)))
          (= (count lst) 3)
          (not (seq? (second lst)))
          (not (seq? (nth lst 2)))) (subExpr lst)

        ; Base Case 2, Count == 2, no index is a list
        (and
          (not (boolean? lst))
          (not (seq? (first lst)))
          (= (count lst) 2)
          (not (seq? (second lst)))) (subExpr lst)

        ; Recursively simplify first index if its a list and count == 2
        (and
          (not (boolean? lst))
          (seq? (first lst))
          (= (count lst) 2)
          (not (seq? (second lst)))) (subExpr
                                        (list
                                          (second lst)
                                          (simplify (first lst))))

        ; Recursively simplify second index if its a list and count == 2
        (and
          (not (boolean? lst))
          (seq? (second lst))
          (= (count lst) 2)
          (not (seq? (first lst)))) (subExpr
                                      (list
                                        (first lst)
                                        (simplify (second lst))))

        ; Recursively simplify first index if its a list and count == 3
        (and
          (not (boolean? lst))
          (seq? (first lst))
          (= (count lst) 3)
          (not (seq? (second lst)))
          (not (seq? (nth lst 2)))) (subExpr
                                      (list
                                        (second lst)
                                        (nth lst 2)
                                        (simplify (first lst))))

        ; Recursively simplify second index if its a list and count == 3
        (and
          (not (boolean? lst))
          (not (seq? (first lst)))
          (= (count lst) 3)
          (seq? (second lst))
          (not (seq? (nth lst 2)))) (subExpr
                                      (list
                                        (first lst)
                                        (simplify (second lst))
                                        (nth lst 2)))

        ; Recursively simplify third index if its a list and count == 3
        (and
          (not (boolean? lst))
          (not (seq? (first lst)))
          (= (count lst) 3)
          (not (seq? (second lst)))
          (seq? (nth lst 2))) (subExpr
                                (list
                                  (first lst)
                                  (second lst)
                                  (simplify (nth lst 2))))

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; Added everything below after meeting with Prof. Schegel ;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

        ; Recursively simplify first index is a list AND second index is a list and count == 3
        (and
          (not (boolean? lst))
          (= (count lst) 3)
          (seq? (first lst))
          (seq? (second lst))
          (not (seq? (nth lst 2)))) (subExpr
                                      (list
                                        (simplify (first lst))
                                        (simplify (second lst))
                                        (nth lst 2)))

        ; Recursively simplify first index is a list AND third index is a list and count == 3
        (and
          (not (boolean? lst))
          (= (count lst) 3)
          (seq? (first lst))
          (not (seq? (second lst)))
          (seq? (nth lst 2))) (subExpr
                                (list
                                  (simplify (first lst))
                                  (second lst)
                                  (simplify (nth lst 2))))

        ; Recursively simplify second index is a list AND third index is a list and count == 3
        (and
          (not (boolean? lst))
          (= (count lst) 3)
          (not (seq? (first lst)))
          (seq? (second lst))
          (seq? (nth lst 2))) (subExpr
                                (list
                                  (first lst)
                                  (simplify (second lst))
                                  (simplify (nth lst 2))))

        ; Recursively simplify if all 3 indices are lists and count == 3
        (and
          (not (boolean? lst))
          (= (count lst) 3)
          (seq? (first lst))
          (seq? (second lst))
          (seq? (nth lst 2))) (subExpr
                                (list
                                  (simplify (first lst))
                                  (simplify (second lst))
                                  (simplify (nth lst 2))))

          ; Recursively simplify if both indices are lists and count == 2
          (and
            (not (boolean? lst))
            (= (count lst) 2)
            (seq? (first lst))
            (seq? (second lst))) (subExpr
                                  (list
                                    (simplify (first lst))
                                    (simplify (second lst))))

          ; if one of the first indices is false and op=and, expr=false
          (and
            (symbol? (first lst))
            (= (first lst) 'and)
            (or
              (= (second lst) false)
              (= (nth lst 2) false))) false

        :else lst
  ))

; Main function
(defn evalexp [exp bindings] (simplify (bind-values bindings exp)))
