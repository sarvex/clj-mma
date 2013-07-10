(ns mma.conversion
  (:refer-clojure :exclude [replace])
  (require [clojure.string :as str :refer [replace]])
  (use clarity.core))

(use-clarity)
(clarity

declare ->expr make-list

defprotocol Mathematica
  ->mma [this]

(extend-protocol Mathematica
  Object
    (->mma [this] (pr-str this))

  nil
    (->mma [this] "Null")
    
  clojure.lang.Keyword
    (->mma [this] (name this))

  clojure.lang.PersistentVector
    (->mma [this] (apply make-list this))

  clojure.lang.ISeq
    (->mma [this]
      (if (symbol? (first this))
        (apply ->expr this)
        (apply make-list this))))

defmulti ->expr (fn [head & args] head)

defmethod ->expr :default [head & args]
  str head "[" (str/join "," (map ->mma args)) "]"

defmethod ->expr 'do [_ & exprs]
  str/join ";" : map ->mma exprs

(defmacro defalias [sym sub]
 `(defmethod ->expr '~sym [_# & args#]
    (apply ->expr '~sub args#)))

defalias + Plus
defalias - Subtract
defalias * Times
defalias / Divide
defalias ** Power
defalias . Dot

defn make-list [& xs]
  str "{" (str/join "," : map ->mma xs) "}"

)
