(ns save-christmas.day5.run
 (:require
   [save-christmas.util :as util]))

(defn gen-range [from to]
  (cond
    (< from to) (range from (+ to 1) 1)
    (> from to) (range from (- to 1) -1)
    (= from to) (repeat from)))

(defn fill [[item-1 item-2]]
 (let [
       [[from-x from-y] [to-x to-y]] [item-1 item-2]
        x (if (not= from-x to-x) (gen-range from-x to-x) from-x)
        y (if (not= from-y to-y) (gen-range from-y to-y) from-y)]

   (cond
     (and (seq? x) (seq? y))       (map vector x y)
     (and (seq? x) (not (seq? y))) (map vector x (take (count x) (repeat y)))
     (and (not (seq? x)) (seq? y)) (map vector (take (count y) (repeat x)) y))))

(comment
 (fill [[7 0] [7 4]])
 (fill [[7 4] [7 0]])

 (fill [[0 7] [4 7]])
 (fill [[4 7] [0 7]])

 (fill [[1 1] [3 3]])
 (fill [[3 3] [1 1]])

 (fill [[7 9] [9 7]])
 (fill [[9 7] [7 9]])

 (fill [[9 4] [3 4]])
 (fill [[6 4] [2 0]]))


;; partI 6856
(def sample (->> (remove empty? (util/load-data "day5.txt"))
                 (mapv #(into [] (rest (re-find (re-matcher #"(\d+),(\d+) -> (\d+),(\d+)" %)))))
                 (mapv (fn [item1] (mapv (fn [item2] (util/str->int item2)) item1)))
                 (filter (fn [[x1 y1 x2 y2]] (or (= x1 x2) (= y1 y2))))
                 (mapv #(partition 2 %))
                 (mapv (fn [item1] (mapv (fn [item2] (into [] item2)) item1)))
                 (mapv #(fill %))
                 (reduce concat [])
                 (mapv #(sorted-set %))
                 frequencies
                 (map last)
                 frequencies)) ;{2 6581, 1 90567, 3 265, 4 10})) 6581+265+10

;; partII 20666
(def sample2 (->> (remove empty? (util/load-data "day5.txt"))
                  (mapv #(into [] (rest (re-find (re-matcher #"(\d+),(\d+) -> (\d+),(\d+)" %)))))
                  (mapv (fn [item1] (mapv (fn [item2] (util/str->int item2)) item1)))
                  (mapv #(partition 2 %))
                  (mapv (fn [item1] (mapv (fn [item2] (into [] item2)) item1)))
                  (mapv #(fill %))
                  (reduce concat [])
                  (mapv #(sorted-set %))
                  frequencies
                  (map last)
                  frequencies)) ; {2 18456, 1 145894, 3 1985, 4 215, 5 10}
