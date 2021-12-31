(ns save-christmas.day6.run
 (:require
   [save-christmas.util :as util]
   [clojure.string :as string]))

(def sample (-> (remove empty? (util/load-data "day6.txt"))
                first
                (string/split #",")
                (->> (mapv #(util/str->int %)))))

;; Initial state: 3,4,3,1,2
;; After  1 day:  2,3,2,0,1
;; After  2 days: 1,2,1,6,0,8
;; After  3 days: 0,1,0,5,6,7,8
;; After  4 days: 6,0,6,4,5,6,7,8,8
;; After  5 days: 5,6,5,3,4,5,6,7,7,8
;; After  6 days: 4,5,4,2,3,4,5,6,6,7
;; After  7 days: 3,4,3,1,2,3,4,5,5,6
;; After  8 days: 2,3,2,0,1,2,3,4,4,5
;; After  9 days: 1,2,1,6,0,1,2,3,3,4,8
;; After 10 days: 0,1,0,5,6,0,1,2,2,3,7,8
;; After 11 days: 6,0,6,4,5,6,0,1,1,2,6,7,8,8,8
;; After 12 days: 5,6,5,3,4,5,6,0,0,1,5,6,7,7,7,8,8
;; After 13 days: 4,5,4,2,3,4,5,6,6,0,4,5,6,6,6,7,7,8,8
;; After 14 days: 3,4,3,1,2,3,4,5,5,6,3,4,5,5,5,6,6,7,7,8
;; After 15 days: 2,3,2,0,1,2,3,4,4,5,2,3,4,4,4,5,5,6,6,7
;; After 16 days: 1,2,1,6,0,1,2,3,3,4,1,2,3,3,3,4,4,5,5,6,8
;; After 17 days: 0,1,0,5,6,0,1,2,2,3,0,1,2,2,2,3,3,4,4,5,7,8
;; After 18 days: 6,0,6,4,5,6,0,1,1,2,6,0,1,1,1,2,2,3,3,4,6,7,8,8,8,8

;; (<= 1 3 6)
;; (<= 1 1 6)
;; (<= 1 6 6)
;; (<= 1 0 6)
;; (<= 1 7 6)

(defn calc-items [sample]
  (let [
        repeat-eight  (->> (filter #(= 0 %) sample)
                           count)
        dec-sample (->> sample
                       (mapv (fn [item]
                               (cond
                                 (= 0 item) 6
                                 :else (- item 1)))))
        new-sample (apply conj dec-sample (take repeat-eight (repeat 8)))]

   new-sample))


;; (calc-items [3 4 3 1 2])
;; (calc-items [2 3 2 0 1])


(reduce
  (fn [{:keys [sample result] :as accu} day]
    (let [new-sample (calc-items sample)]
     (-> accu
         (assoc-in [:sample] new-sample)
         (assoc-in [:result] (conj result [day (count new-sample)])))))

  {:sample sample :result []}
  (take 256 (iterate inc 1)))

;; (defn part-one [{:keys [run-days day result] :as opt}]
;;   (if (and (= day run-days) (empty? run-list))
;;       result
;;       (part-one calc(opt))))

;; (part-one {:run-list sample :run-days 18 :day 1 :result 0 :data {}})


