(ns advent.two
  (:require [clojure.string :as str]))

(defn read-input []
  (str/split (slurp "resources/two.txt") #"\n"))

(defn box-id-repetitions [box-id]
  (set (map (comp count val) (group-by identity box-id))))

(defn repeats-a-letter-n-times [n]
  (fn [repetitions]
    (contains? repetitions n)))

(defn checksum [box-ids]
  (let [box-ids-repetitions (map box-id-repetitions box-ids)
        twos (filter (repeats-a-letter-n-times 2) box-ids-repetitions)
        threes (filter (repeats-a-letter-n-times 3) box-ids-repetitions)]
    (* (count twos) (count threes))))

(defn first-solution []
  (checksum (read-input)))
