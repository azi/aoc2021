(ns save-christmas.day2.run
 (:require
   [save-christmas.util :as util]))

;; part I => 1654760
(let [sample (util/load-data "day2.txt")
      result (reduce
               (fn [result line]
                 (let [[_ command step] (re-find #"(\w+)\s(\d+)" line)]
                    (update-in result [(keyword command)] + (util/str->int step))))
               {:forward 0 :up 0 :down 0}
               sample)]

   (* (:forward result) (- (:down result) (:up result))))


;; part II => 1956047400
(let [sample (util/load-data "day2.txt")
      result (reduce
               (fn [result line]
                 (let [[_ command _step] (re-find #"(\w+)\s(\d+)" line)
                        step (util/str->int _step)]
                   (cond
                      (= "up" command)      (update-in result [:aim] - step)
                      (= "down" command)    (update-in result [:aim] + step)
                      (= "forward" command) (-> result
                                                (update-in [:forward] + step)
                                                ((fn [{:keys [aim depth] :as item}]
                                                    (assoc-in item [:depth] (+ depth (* aim step)))))))))
               {:forward 0 :depth 0 :aim 0}
               sample)]

   (* (:forward result) (:depth result)))

(comment)
 ;; ["forward 5" "down 5" "forward 8" "up 3" "down 8" "forward 2"])]
