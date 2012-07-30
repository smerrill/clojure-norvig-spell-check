(ns norvig-checker.core)
(use '[clojure.string :only (lower-case)])
(defn words [x] (re-seq #"[a-z]+" (lower-case x)))
(def NWORDS (frequencies (words (slurp "small.txt")))) ; @TODO: Move from slurp.
;(words (slurp "big.txt"))
(def alphabet "abcdefghijklmnopqrstuvwxyz")
(defn correct [words] (filter #(contains? NWORDS %) (list words)))
(defn edits1 [word]
  (let [len (count word)]
    (for [a (range len)]
      (concat
        (concat (take a word) (drop (inc a) word)) ; Deletions
        (if (< a (dec len)) (concat (take a word) (list (get word (inc a))) (list (get word a)) (drop (inc (inc a)) word)) nil) ; Transposes (yuck)
        (map #(concat (take a word) (list %) (drop (inc a) word)) alphabet) ; Replaces
        (map #(concat (take a word) (list %) (drop a word)) alphabet) ; Inserts
      )
    )))

(edits1 "hi")
