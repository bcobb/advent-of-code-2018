(ns advent.one
  (:require [clojure.string :as str]))

(defn read-input []
  (str/split (slurp "resources/one.txt") #"\n"))

(def change-functions
  {\+ +
   \- -})

(defn change-to-operation [change]
  (let [change-function (get change-functions (nth change 0))
        change-argument (Integer/parseInt (apply str (rest change)))]
    (fn [frequency] (change-function frequency change-argument))))

(defn first-solution-for-changes [changes]
  (let [operations (map change-to-operation changes)]
    ((apply comp operations) 0)))

(defn second-solution-for-changes [changes]
  (loop [current-change (first changes)
         remaining-changes (rest changes)
         frequency 0
         known-frequencies #{0}]
    (let [operation (change-to-operation current-change)
          frequency' (operation frequency)]
      (if (known-frequencies frequency')
        frequency'
        (recur (first remaining-changes)
               (rest remaining-changes)
               frequency'
               (conj known-frequencies frequency'))))))

(defn first-solution []
  (first-solution-for-changes (read-input)))

(defn second-solution []
  (second-solution-for-changes (cycle (read-input))))
