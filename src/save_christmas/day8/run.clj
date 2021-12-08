(ns save-christmas.day8.run
 (:require
   [save-christmas.util :as util]
   [clojure.string :as string]
   [clojure.set :as set-obj]))

;; part I: 301
(def part-one (->> (remove empty? (util/load-data "day8.txt"))
                   (map (fn [item] (rest (re-find #"\| (\w+) (\w+) (\w+) (\w+)" item))))
                   flatten
                   (filter #(contains? #{2 4 3 7} (count %)))
                   count))

(comment
 (rest (re-find #"(\w+) (\w+) (\w+) (\w+) (\w+) (\w+) (\w+) (\w+) (\w+) (\w+)" "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe"))
 (->> (rest (re-find #"\| (\w+) (\w+) (\w+) (\w+)" "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe"))
      (map set))

 (set-obj/difference #{1 2 3})
 (set-obj/difference #{\a \b \c \d \e \f \g} #{\b})
 (set-obj/difference #{\b} #{\a \b \c \d \e \f \g})
 (set-obj/superset? #{\a \b \c \d \e \f \g} #{\b})
 (set-obj/subset? #{\a \b \c \d \e \f \g} #{\b})
 (set-obj/subset? #{\b} #{\a \b \c \d \e \f \g})
 (set-obj/intersection #{\b} #{\a \b \c \d \e \f \g})
 (set-obj/intersection #{\a \b \c \d \e \f \g} #{\b})
 (set-obj/union #{\3} #{\a \b \c \d \e \f \g})
 (= #{\3} #{\3}))

(defn parse-data [input]
  (let [init         (->> (rest (re-find #"(\w+) (\w+) (\w+) (\w+) (\w+) (\w+) (\w+) (\w+) (\w+) (\w+)" input))
                          (map set))
        decoded-list (->> (rest (re-find #"\| (\w+) (\w+) (\w+) (\w+)" input))
                          (map set))

        four  (first (filter #(= 4 (count %)) init))
        one   (first (filter #(= 2 (count %)) init))
        eight (first (filter #(= 7 (count %)) init))
        seven (first (filter #(= 3 (count %)) init))

        four-one-diff (set-obj/difference four one)
        four-seven-eight (set-obj/difference eight (set-obj/union seven four))]


    (->> decoded-list
         (map (fn [item]
               (cond
                 (and (= 5 (count item)) (set-obj/superset? item one)) 3
                 (and (= 5 (count item)) (= 2 (count (set-obj/intersection item four-one-diff)))) 5
                 (and (= 5 (count item)) (= 1 (count (set-obj/intersection item four-one-diff)))) 2
                 (and (= 6 (count item)) (= 1 (count (set-obj/intersection item one)))) 6
                 (and (= 6 (count item)) (= 1 (count (set-obj/intersection item four-seven-eight)))) 9
                 (and (= 6 (count item)) (= 2 (count (set-obj/intersection item four-seven-eight)))) 0
                 (= one item) 1
                 (= seven item) 7
                 (= four item) 4
                 (= eight item) 8)))
         (string/join #"")
         (util/str->int))))


(comment
 (parse-data "ga befgac fcgedba fabced agfce edgfc gbea aebcf acg cagfdb | gecdf afebcd bgdfac cga"))

;; part-two: 908067
(def sample (->> (remove empty? (util/load-data "day8.txt"))
                 (mapv parse-data)
                 (apply +)))
