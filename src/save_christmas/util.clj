(ns save-christmas.util
 (:require
   [clojure.java.io :as io]))

(defn load-data [path]
  (with-open [txt (io/reader (str "src/save_christmas/data/" path))]
   (->> (line-seq txt)
        (into []))))

(def str->int #(try (Integer/parseInt %) (catch Exception _ex %)))

(comment
  (load-data "day1.txt"))
