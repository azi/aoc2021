(ns save-christmas.util
 (:require
   [clojure.java.io :as io]))

(defn load-data [path]
  (with-open [txt (io/reader (str "src/save_christmas/data/" path))]
   (->> (line-seq txt)
        (into []))))

(def str->int #(try (Integer/parseInt %) (catch Exception _ex %)))

(defn blank? [v] (if (seqable? v) (empty? v) (not v)))

(def present? (complement blank?))

(comment
  (load-data "day1.txt"))
