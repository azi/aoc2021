(ns save-christmas.day7.run
 (:require
   [save-christmas.util :as util]
   [clojure.string :as string]))

(def sample (-> (remove empty? (util/load-data "day7.txt"))
                first
                (string/split #",")
                (->> (mapv util/str->int))
                sort))


;; partI => 337488
(defn part-i [sample]
  (let [result (reduce
                (fn [{:keys [sample row] :as result} idx-n]
                  (let [coll (->> (map (fn [item] (Math/abs (- item idx-n))) sample)
                                  (apply +))]
                   (-> result
                       (assoc-in [:row] (conj row (hash-map (str idx-n) coll))))))
                {:sample sample
                 :row {}}
                sample)]

    (->> (:row result)
         (sort-by last)
         first)))

(part-i sample)

;; partII => 89647695
(defn part-ii [iterate-list sample]
  (let [result (reduce
                (fn [{:keys [sample row] :as result} idx-n]
                  (let [coll (->> sample
                                  (map (fn [item] (Math/abs (- item idx-n))))
                                  (map (fn [item] (apply + (take item (iterate inc 1)))))
                                  (apply +))]
                   (-> result
                       (assoc-in [:row] (conj row (hash-map idx-n coll))))))
                {:sample sample
                 :row {}}
                iterate-list)]

    (->> (:row result)
         (sort-by last)
         first)))

(part-ii (take (last sample) (iterate inc 1)) sample)
