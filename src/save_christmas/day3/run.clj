(ns save-christmas.day3.run
 (:require
   [save-christmas.util :as util]
   [clojure.string :as string]))

(def sample  (->> (util/load-data "day3.txt")
                  (mapv #(string/split % #""))
                  (mapv (fn [item1] (mapv (fn [item2] (util/str->int item2)) item1)))))

(defn calc [sample]
  (reduce
    (fn [{:keys [gamma epsilon] :as result} item]
      (let [odd-count  (count (filter odd? item))
            even-count (count (filter even? item))
            new-gamma   (if (> odd-count even-count) 1 0)
            new-epsilon (if (< odd-count even-count) 1 0)]
       (-> result
           (assoc-in [:gamma] (str gamma new-gamma))
           (assoc-in [:epsilon] (str epsilon new-epsilon)))))
    {:gamma "" :epsilon ""}
    (apply map vector sample)))

;; part I => 2967914
(let [result (calc sample)]
  (apply * (map #(Integer/parseInt % 2) (vals result))))

;; part II => 7041258
(defn naming-is-hard [{:keys [rest-sample-1 rest-sample-2 idx] :as result}]
  (let [calc (fn [item]
               (let [odd-count  (count (filter odd? item))
                     even-count (count (filter even? item))
                     gamma   (if (= odd-count even-count)
                                 1
                                 (if (> odd-count even-count) 1 0))
                     epsilon (if (= odd-count even-count)
                                 0
                                 (if (< odd-count even-count) 1 0))]
                {:gamma gamma :epsilon epsilon}))

         result-1 (calc (get (apply mapv vector rest-sample-1) idx))
         result-2 (calc (get (apply mapv vector rest-sample-2) idx))]

    (cond-> result
            (> (count rest-sample-1) 1) (assoc-in [:rest-sample-1] (filterv #(= (:gamma result-1) (get % idx)) rest-sample-1))
            (> (count rest-sample-2) 1) (assoc-in [:rest-sample-2] (filterv #(= (:epsilon result-2) (get % idx)) rest-sample-2))
            true (assoc-in [:idx] (+ 1 idx)))))

(defn call-me-again [result]
  (if (= 1 (count (:rest-sample-1 result)) (count (:rest-sample-2 result)))
      result
      (call-me-again (naming-is-hard result))))

(let [{:keys [rest-sample-1 rest-sample-2]} (call-me-again {:rest-sample-1 sample :rest-sample-2 sample :idx 0})]
  (* (Integer/parseInt (string/join #"" (first rest-sample-1)) 2)
     (Integer/parseInt (string/join #"" (first rest-sample-2)) 2)))

(comment
 (Integer/parseInt "10110" 2)
 (string/join #"" [1 3 4])
 (get (apply mapv vector sample) 1))
