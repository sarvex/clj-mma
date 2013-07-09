(ns primes
  (require [mma.core :as m])
  (use [clarity core utils]))

(use-clarity)
(clarity

(declare primes, prime?)

(defn prime? [n]
  (->> primes
       (take-while
         (λ <= % (Math/sqrt n)))
       (map
         (λ mod n %))
       (not-any? zero?)))

(def primes
  (->> (iterate inc 3)
       (filter prime?)
       (cons 2)))

(def fibs (lazy-cat [0 1] (map + fibs (rest fibs))))

;; Take 2

declare primes, prime?

defn prime? [n]
  ->> primes
      take-while
        λ <= % : Math/sqrt n
      map
        λ mod n %
      not-any? zero?

def primes
  ->> (iterate inc 3)
      filter prime?
      cons 2

def fibs
  lazy-cat [0 1] : map + fibs : rest fibs

(defn foo [& args]
  (let [f  (first args)
        i (second args)]
    (map f (range i))))

defn foo [& args]
  let [f  (first args)
       i (second args)]
    map f
      range i

)
