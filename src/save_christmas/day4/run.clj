(ns save-christmas.day4.run
 (:require
   [save-christmas.util :as util]
   [clojure.string :as string]))

(def drawn-list (string/split "17,11,37,7,89,48,99,28,56,55,57,27,83,59,53,72,6,87,33,82,13,23,35,40,71,47,78,2,39,4,51,1,67,31,79,69,15,73,80,22,92,95,91,43,26,97,36,34,12,96,86,52,66,94,61,76,64,77,85,98,42,68,84,63,60,30,65,19,54,58,24,20,25,75,93,16,18,44,14,88,45,10,9,3,70,74,81,90,46,38,21,49,29,50,0,5,8,32,62,41" #","))
;; (def drawn-list (string/split "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1" #","))

(defn gen-items [y row]
  (into [] (map-indexed #(apply hash-map [:x %1 :y (mod y 5) :marked false :value %2]) row)))

(def sample (->> (remove empty? (util/load-data "day4.txt"))
                 (mapv #(into [] (remove empty? (string/split % #" "))))
                 (map-indexed #(gen-items %1 %2))
                 flatten
                 (partition 25)
                 (map-indexed (fn [idx items] (->> items
                                                   (map #(assoc-in % [:board-num] (+ 1 idx))))))
                 flatten
                 (into [])))

(defn marked [drawn-num item]
  (cond-> item
          (= drawn-num (:value item)) (assoc-in [:marked] true)))


(defn find-winner [{:keys [sample drawn-list idx] :as result}]
  (let [drawn-num (get drawn-list idx)
        new-data (->> sample
                      (map #(marked drawn-num %)))

        checking-x (->> new-data
                        (group-by (juxt :board-num :x))
                        (filter (fn [items] (every? true? (map :marked (last items)))))
                        last
                        last)
        checking-y (->> new-data
                        (group-by (juxt :board-num :y))
                        (filter (fn [items] (every? true? (map :marked (last items)))))
                        last
                        last)]

       (-> result
           (assoc-in [:sample] new-data)
           (assoc-in [:winner] (or checking-x checking-y))
           (assoc-in [:idx] (+ 1 idx))
           (assoc-in [:drawn-num] drawn-num))))

(defn process [result]
   (if (util/present? (:winner result))
       result
       (process (find-winner result))))

;; part I => 55770
(let [result  (process {:sample sample :drawn-list drawn-list :winner nil :idx 0})
      board-num (-> (:winner result)
                    first
                    :board-num)
      unmark-item (->> (:sample result)
                       (filter #(and (= board-num (:board-num %)) (= (:marked %) false))))]

    (*(apply + (map #(util/str->int (:value %)) unmark-item))
      (util/str->int (:drawn-num result))))


(defn find-winner-board [{:keys [sample winner-board winner-board-order] :as result} drawn-num]
  (let [marked-sample (->> sample
                           (map #(marked drawn-num %)))

        checking-x (->> marked-sample
                        (filter #(not (contains? (set winner-board-order) (:board-num %))))
                        (group-by (juxt :board-num :x))
                        (filter (fn [items] (every? true? (map :marked (last items))))))

        checking-y (->> marked-sample
                        (filter #(not (contains? (set winner-board-order) (:board-num %))))
                        (group-by (juxt :board-num :y))
                        (filter (fn [items] (every? true? (map :marked (last items))))))

        find-board-nums (->> (concat (map last checking-x) (map last checking-y))
                             (map last)
                             (map :board-num)
                             (distinct)
                             (into []))]

       (cond-> result
               (< (count winner-board-order) 100) (assoc-in [:sample] marked-sample)
               (< (count winner-board-order) 100) (assoc-in [:drawn-num] drawn-num)
               (< (count winner-board-order) 100) (assoc-in [:winner-board-order] (into winner-board-order find-board-nums)))))


;; part II => 2980
(let [data    {:sample sample :drawn-list drawn-list :winner nil :winner-board-order []}
      result  (reduce
                find-winner-board
                data
                drawn-list)

      last-winner-board-num (last (:winner-board-order result))

      unmark-item (->> (:sample result)
                       (filter #(and (= last-winner-board-num (:board-num %)) (= (:marked %) false))))]

    (*(apply + (map #(util/str->int (:value %)) unmark-item))
      (util/str->int (:drawn-num result))))

(comment
 (->> sample
      (map #(marked "7" %))
      (map #(marked "4" %))
      (map #(marked "9" %))
      (map #(marked "5" %))
      (map #(marked "11" %))
      (map #(marked "17" %))
      (map #(marked "23" %))
      (map #(marked "2" %))
      (map #(marked "0" %))
      (map #(marked "14" %))
      (map #(marked "21" %))
      (map #(marked "24" %))
      (map #(marked "10" %))
      (map #(marked "16" %))
      (map #(marked "13" %))
      (group-by (juxt :board-num :y))
      (filter (fn [items] (every? true? (map :marked (last items)))))
      (map last)

  (def matcher (re-matcher #"\d+" "8  2 23  4 24"))
  (re-find matcher)))

