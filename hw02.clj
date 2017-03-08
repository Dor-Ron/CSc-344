;File: hw2.clj
;Authors: Dor Rondel and Nirender Rathor

(ns hw2)

;Redefined orexp and andexp to take variable # of parameters
(defn orexp [& args]
  (reducex
    (fn [e1 e2]
      (list 'or e1 e2)) args))

(defn andexp [& args]\
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
    '(not (or x y)) '(and (not x) (not y))))

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
  ;;;; This one of the functions i came up with
   I tried doing the evalexp but I cant seem to get anywhere with it 
   (defn evalexp 
    (stringmap "["e1" orexp, "e2" andexp]" (simplify (bind-values bindings exp)))
     
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     
     
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    
    
       
 ;;;;This is the second one
 
 ;;Defining a function and calling the hashmaptable. 
     ( defn Evalexp [boolExp Map])
  ;;the first element of the nested list and 
     (nth Evalexp 0)
 ;next element on the list 
     (nth Evalexp 1)
      (nth ([]0))
 ;;out of bound exception
      (nth []0  "nothing herer")
      (nth [Evalexp])
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
