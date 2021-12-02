(ns save-christmas.aoc2021
 (:require
   [clojure.java.io :as io]))

(defn load-data [path]
  (with-open [txt (io/reader (str "src/save_christmas/data/" path))]
   (->> (line-seq txt)
        (into []))))

(comment
  (load-data "day1.txt"))
