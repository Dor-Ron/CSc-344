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
