(ns autograd.core
	(:require 
		[clojure.walk]
		[clojure.core.matrix]
	)
	(:gen-class)
)

(defn -main
	"I don't do a whole lot ... yet."
	[& args]
	(println "Hello, World!"))

; Define our operations.
;(defn op-add [& x] `(+ ~@x)) ; ~@ expands x into an array from a list.
;(defn op-prod [a b] `(* ~a ~b)) ; Only two for now.

; Differentiation!
; d of a constant is zero.
; d of a variable is zero wrt another variable.
; d of a variable is one wrt itself.
; d of a sum is sum of differentiated.
; d of a product is the sum of the individually differentiated products.  (abc)' = a'bc + ab'c + abc'

(defn grad
	[f wrt]
	(cond
		(keyword? f) (if (= f wrt) 1 0)
		(number? f) 0
		(seq? f) (let [op (first f), params (rest f)]
			(cond
				(or (= op 'clojure.core/+) (= op '+)) `(+ ~@(map #(grad % wrt) params))
				(or (= op 'clojure.core/*) (= op '*)) `(+ (* ~(grad (first params) wrt) ~(second params)) (* ~(first params) ~(grad (second params) wrt)))
				:else (print "SHIT"))
			)
		:else (print "Goddamnit")
	)
)

(defn eval-fun [fun varmap] (eval (clojure.walk/postwalk-replace varmap fun)))
(defn eval-grad [fun varmap wrt] (eval (clojure.walk/postwalk-replace varmap (grad fun wrt))))

; (def sums_proc '(+ :x :y :z (+ :y :y :z)))
; (eval-fun sums_proc {:x 10 :y 5 :z 2})
; 29
; (eval-grad sums_proc {:x 10 :y 5 :z 2} :x)
; 1


