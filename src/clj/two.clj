(ns advent.two
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combo]))

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

(defn pairwise [word other-word]
  (map vector word other-word))

(defn identical-pair? [pair]
  (apply = pair))

(defn pairwise-difference [word other-word]
  (->> (pairwise word other-word)
       (remove identical-pair?)
       count))

(defn one-letter-different? [pair]
  (= 1 (apply pairwise-difference pair)))

(defn common-letters [word other-word]
  (->> (pairwise word other-word)
       (filter identical-pair?)
       (map first)))

(defn first-solution []
  (checksum (read-input)))

(defn second-solution []
  (->> (combo/combinations (read-input) 2)
       (filter one-letter-different?)
       first
       (apply common-letters)
       (apply str)))
