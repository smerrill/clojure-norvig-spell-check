(ns norvig-checker.core)
  (use '[clojure.string :only (lower-case)])
(defn words-in [x] (re-seq #"[a-z]+" (lower-case x)))
(def NWORDS (frequencies (words-in (slurp "big.txt")))) ; @TODO: Move from slurp.
(def alphabet "abcdefghijklmnopqrstuvwxyz")

(defn deletion [a word] (list (apply str (concat (take a word) (drop (inc a) word)))))
(defn replaces [a word] (map #(apply str (concat (take a word) (list %) (drop (inc a) word))) alphabet))
(defn transposition [a word] (list (apply str (if (< a (dec (count word))) (concat (take a word) (list (get word (inc a))) (list (get word a)) (drop (inc (inc a)) word))))))
(defn inserts [a word] (map #(apply str (concat (take a word) (list %) (drop a word))) alphabet))

(defn edits1 [word]
  (remove empty? 
    (flatten 
      (for [a (range (count word))]
        (list (deletion a word) (inserts a word) (replaces a word) (transposition a word))))))

(defn known [words]
  (let [x (filter #(contains? NWORDS %) words)]
    (if (empty? x) nil x)))
(defn correct [word]
  (let [x (or (known (list word)) (known (edits1 word)) (known (flatten (map edits1 (edits1 word)))) (list word))]
    (key (first (sort (reduce merge (map #(hash-map % (get NWORDS %)) x)))))))

