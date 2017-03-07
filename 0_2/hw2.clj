;File: hw2.clj
;Authors: Dor Rondel and Nirender

(ns hw2)

;Redefined orexp and andexp to take variable # of parameters
(defn orexp [& args]
  (reduce
    (fn [e1 e2]
      (list 'or e1 e2)) args))

(defn andexp [& args]
  (reduce
    (fn [e1 e2]
      (list 'and e1 e2)) args))

;Redefined not exp
(defn notexp [& args]
  (reduce 
   (fn [e1 e2]
    (list not e1 or e2)) args ))
  
;Boolean Algebra Simplication HashMap.class Supplied
  (def boolExpMap
  (hash-map '(or true) true,
    '(or false) false,
    '(and true) true,
    '(and false) false,
    '(or x false) 'x,
    '(or false x) 'x,
    '(or true x) true,
    '(or x true) true,
    '(and x false) false,
    '(and false x) false,
    '(and x true) 'x,
    '(and true x) 'x,
    '(not false) true,
      **--
    '(not true) false,
    '(or x y z) '(or x (or y z)),
    '(and x y z) '(and x (and y z)),
    '(not (and x y)) '(or (not x) (not y)),
    '(not (or x y)) '(and (not x) (not y)))),

   ;; I tried doing the evalexp but i cant seem to get anywhere with it 
   (defn evalexp 
     (stringmap "["e1" orexp, "e2" andexp]" (simplify (bind-values bindings exp)))