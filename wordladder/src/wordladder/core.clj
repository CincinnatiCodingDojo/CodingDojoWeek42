(ns wordladder.core
  (:require [clojure.java.io :refer :all])
  (:gen-class))

(def alphabet (set (map char (range 97 123))))

(defn read-word-set []
  (set (with-open [rdr (reader "word.list")]
    (doall (line-seq rdr)))))

(def dictionary (read-word-set))

(defn find-word [word word-set]
  (contains? word-set word))

(defn all-other-letters [letter]
  (disj alphabet letter))

(defn vector-contains [previous-words]
  (fn [element]
    (some #(= element %) previous-words)))

(defn generate-next-words [word previous-words]
  (remove (vector-contains previous-words) (let [letters (apply vector word)]
    (for [i (range 0 (count letters))
          replacement (all-other-letters (nth letters i))]
      (apply str (assoc letters i replacement))))))

(defn filter-words-in-list [listGen listOrg]
  (filter listOrg listGen))

;(defn inner-solve [current-word end-word path]
;	(if (= current-word end-word)
;		(conj path current-word) ;should only do if not yet added to path
;	  (let [generated-words (generate-next-words current-word path)
;	  	    valid-words (filter-words-in-list generated-words dictionary)]
	; HELP ME - need to mark the start of each path and go through them sequentially until we find an answer or run out.
	; then we can take each start and run the above in parallel
;	(print valid-words)
;	(inner-solve (first valid-words) end-word (first (conj path valid-words))))))

(defn reverse-link-list [end-word word-vector]
  (loop [current-word end-word
    path [end-word]]
    (let [parent ((first (filter #(= current-word (% :word)) word-vector)) :parent)]
      (if (= parent current-word)
        path
        (recur parent (cons parent path))))))

(defn inner-solve [start-word end-word word-vector]
  (loop [index 1
         path word-vector]
    (let [current-word ((nth path index) :word)]
      (if (some #(= end-word (% :word)) path)
        (reverse-link-list end-word path)
        (recur (inc index) (concat path (map #(hash-map :word %, :parent current-word) (generate-next-words current-word (map :word path)))))))))

(defn solve [start-word end-word]
  (if (= start-word end-word)
    start-word
    (inner-solve start-word end-word (map #(hash-map :word %, :parent start-word) (conj (generate-next-words start-word []) start-word)))))
