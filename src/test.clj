(ns test)

(defprotocol PTest
  (->test [this]))

(def t (reify PTest (->test [this] true)))

(->test t)

(map ->test [t])
