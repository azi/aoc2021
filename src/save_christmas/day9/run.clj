(ns save-christmas.day9.run
 (:require
   [save-christmas.util :as util]
   [clojure.string :as string]
   [clojure.set :as set-obj]))

;; part I: 554
(def sample (->> (remove empty? (util/load-data "day9-sample.txt"))
                 (map #(string/split % #""))
                 (mapv (fn [item1] (mapv (fn [item2] (util/str->int item2)) item1)))))

(defn find-lowest [x y data]
  (let [top   [x (- y 1)]
        down  [x (+ y 1)]
        left  [(- x 1) y]
        right [(+ x 1) y]
        check-point (get-in data [x y])]

    (->> (map #(get-in data %) [top down left right])
         (remove nil?)
         (every? #(> % check-point)))))

(comment
  (find-lowest 2 2 sample)
  (find-lowest 0 0 sample)
  (find-lowest 0 8 sample)
  (->> (flatten [[1 2 3] [4 5 6]])
       (partition 3))
  (mod 0 3)
  (mod 1 3)
  (mod 2 3)
  (int (Math/floor (/ 2 3))))


(defn part-one [data]
  (->> data
       (map-indexed
        (fn [idy row]
          (let []
                ;; _ (prn "y:" idy row)]
           (->> row
                (map-indexed
                 (fn [idx column]
                  (let []
                        ;; _ (prn "x:" idx idy column)]
                    ;; column
                    [column (find-lowest idy idx data)])))
                (filter #(true? (last %)))))))))
       ;; (remove empty?)
       ;; (apply concat)
       ;; (map #(+ 1 (first %)))
       ;; (apply +)))

(comment
 (map-indexed #(prn %1 %2) sample)
 (apply map concat [[[123] [456]] [[555] [999]]])
 (part-one sample))


(defn find-basin-list [{:keys [x y data basin-list] :as result}]
  (let [
        up          [(- y 1) x]
        down        [(+ y 1) x]
        left        [y (- x 1)]
        right       [y (+ x 1)]

        up-val      (get-in data up)
        down-val    (get-in data down)
        left-val    (get-in data left)
        right-val   (get-in data right)

        _ (prn "azi:" x y up-val up (and (< up-val 9) (> 0 (first up)) (> 0 (last up))))
        _ (prn "azi-2" result)

        ;; (if (not empty? run-items)
        ;;   (find-basin-list result))]
        _ (-> result
              (assoc-in [basin-list] (conj basin-list (sorted-set [x y]))))

          ([up down left right])]

     (cond->> result
               true ((fn [r]
                      (if (and (< up-val 9) (> (first up) 0) (> (last up) 0)) (assoc-in result [:basin-list] (find-basin-list (-> r
                                                                                                                                  (assoc-in [:y] (first up))
                                                                                                                                  (assoc-in [:x] (last up))
                                                                                                                                  (assoc-in [:basin-list] (conj basin-list (sorted-set up))))))
                        r)))
               true ((fn [r]
                      (if (and (< right-val 9) (> (first right) 0) (> (last right) 0)) (find-basin-list (-> r
                                                                                                            (assoc-in [:y] (first right))
                                                                                                            (assoc-in [:x] (last right))
                                                                                                            (assoc-in [:basin-list] (conj basin-list (sorted-set right)))))
                        r))))))

             ;; (and (< right-val 9) (> (first right) 0) (> (last right) 0))  (as-> $ (find-basin-list (-> $
             ;;                                                                                            (assoc-in [:y] (first right))
             ;;                                                                                            (assoc-in [:x] (last right))
             ;;                                                                                            (assoc-in [:basin-list] (conj basin-list (sorted-set right-val))))))

             ;; (and (< down-val 9) (> (first down) 0) (> (last down) 0)) (as-> $ (find-basin-list (-> $
             ;;                                                                                        (assoc-in [:y] (first down))
             ;;                                                                                        (assoc-in [:x] (last down))
             ;;                                                                                        (assoc-in [:basin-list] (conj basin-list (sorted-set down-val))))))

             ;; (and (< left-val 9) (> (first left) 0) (> (last left) 0)) (as-> $ (find-basin-list (-> $
                                                                                                    ;; (assoc-in [:y] (first left))
                                                                                                    ;; (assoc-in [:x] (last left))
                                                                                                    ;; (assoc-in [:basin-list] (conj basin-list (sorted-set left-val))))))))))

(comment
  (find-basin-list {:x 2 :y 2 :data sample :basin-list #{}})
  (find-basin-list {:x 2 :y 2 :data sample :basin-list []})
  (find-basin-list {:x 2 :y 2 :data sample :basin-list []})
  (find-basin-list {:x 2 :y 2 :data sample :basin-list []})
  (get-in sample [4 2]))
  ;; (find-basin 0 0 sample)
  ;; (find-basin 0 8 sample))

;; part-two
(defn part-two [data]
  (->> data
       (map-indexed
        (fn [idy row]
          (let []
           (->> row
                (map-indexed
                 (fn [idx _column]
                  (let []
                    (when (find-lowest idy idx data)
                      (cond->> {:x idy :y idx :data data :basin-list #{}}
                               (find-basin-list))))))
                      ;; (cond->> {:x idy :y idx :data data :basin-list [] :run-items []}
                      ;;          true (find-basin-list :up)
                      ;;          true (find-basin-list :right)
                      ;;          true (find-basin-list :down)
                      ;;          true (find-basin-list :left))))))
                (remove empty?)))))))
       ;; (remove empty?)))
       ;; flatten))
       ;; (into #{})
       ;; count))
       ;; (apply concat)
       ;; (map #(+ 1 (first %)))
       ;; (apply +)))
(comment)
  ;; (part-two sample))
  ;; (-> (into #{} (map sorted-set [[1 2] [0 2]]))
  ;;     (conj (sorted-set [3 4]))
  ;;     (conj (sorted-set [3 4])))
  ;; (find-basin 2 2 sample)
  ;; (find-basin 0 0 sample)
  ;; (find-basin 0 8 sample))
