(ns advent.three
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combo]))

(defn read-input []
  (str/split (slurp "resources/three.txt") #"\n"))

(def claim-fields [:number :left :top :width :height])
(def claim-pattern #"^#(\d+) @ (\d+),(\d+): (\d+)x(\d+)$")

(defn line-to-claim [line]
  (->> line
       (re-seq claim-pattern)
       first
       rest
       (map #(Integer/parseInt %))
       (interleave claim-fields)
       (apply hash-map)))

(defn claim-to-points [{left :left
                        top :top
                        width :width
                        height :height}]
  (combo/cartesian-product
   (range left (+ left width))
   (range top (+ top height))))

(defn points-histogram [claims]
  (->> claims
       (map claim-to-points)
       (mapcat identity)
       (reduce (fn [histogram point] (update histogram point (fnil inc 0))) {})))

(defn overlapping-points [claims]
  (->> claims
       points-histogram
       (filter (fn [entry] (> (val entry) 1)))
       (map first)
       set))

(defn solitary-points [claims]
  (->> claims
       points-histogram
       (filter (fn [entry] (= (val entry) 1)))
       (map first)
       set))

(defn comprised-of? [points]
  (fn [claim]
    (every?
     (partial contains? points)
     (claim-to-points claim))))

(defn first-solution []
  (->> (read-input)
       (map line-to-claim)
       overlapping-points
       count))

(defn second-solution []
  (let [claims (->> (read-input)
                    (map line-to-claim))
        points-without-overlap (solitary-points claims)
        stands-alone? (comprised-of? points-without-overlap)]
    (->> claims
         (filter stands-alone?)
         first
         :number)))
