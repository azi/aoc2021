(ns save-christmas.day2.run
 (:require
   [save-christmas.util :as util]))

(def ssample ["forward 5" "down 5" "forward 8" "up 3" "down 8" "forward 2"])
(def sample  (->> (util/load-data "day2.txt")
                  (map #(re-find #"(\w+)\s(\d+)" %))
                  (map #(into [] [(keyword (get % 1)) (util/str->int (get % 2))]))))

;; part I => 1654760
(let [ result (reduce
                (fn [result [command step]]
                  (update-in result [command] + step))
                {:forward 0 :up 0 :down 0}
                sample)]

   (* (:forward result) (- (:down result) (:up result))))


;; part II => 1956047400
(let [result (reduce  ;replace reduce with reductions can see the results of each step
               (fn [{:keys [aim] :as result} [command step]]
                (case command
                   :up      (update-in result [:aim] - step)
                   :down    (update-in result [:aim] + step)
                   :forward (-> result
                                (update-in [:forward] + step)
                                (update-in [:depth] + (* aim step)))))
               {:forward 0 :depth 0 :aim 0}
               sample)]

   (* (:forward result) (:depth result)))

(comment
 ((re-find #"(\w+)\s(\d+)" (first ssample))))
