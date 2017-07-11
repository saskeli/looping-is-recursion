(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc b e]
                 (if (zero? e)
                   acc
                   (recur (* acc b) b (dec e))))]
    (helper 1 base exp)))

(defn last-element [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (recur (rest a-seq))))

(defn seq= [seq1 seq2]
  (cond 
    (and (empty? seq1) (empty? seq2)) true
    (or (empty? seq1) (empty? seq2)) false
    (== (first seq1) (first seq2)) (recur (rest seq1) (rest seq2))
    :else false))

(defn find-first-index [pred a-seq]
  (loop [idx 0
         l-seq a-seq]
    (cond
      (empty? l-seq) nil
      (pred (first l-seq)) idx
      :else (recur (inc idx) (rest l-seq)))))

(defn avg [a-seq]
  (loop [l-seq a-seq
         num-elems 0
         sum-elems 0]
    (if (empty? l-seq)
      (if (zero? num-elems)
        0
        (/ sum-elems num-elems))
      (recur (rest l-seq) (inc num-elems) (+ sum-elems (first l-seq))))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (loop [l-set a-seq
         p-set #{}]
    (if (empty? l-set)
      p-set
      (recur (rest l-set) (toggle p-set (first l-set))))))

(defn fast-fibo [n]
  (if (zero? n)
    0
    (loop [fn-1 0
           fn 1
           to n]
      (if (< to 2)
        fn
        (recur fn (+ fn-1 fn) (dec to))))))

(defn cut-at-repetition [a-seq]
  (loop [l-seq a-seq
         u-set #{}
         res []]
    (cond
      (empty? l-seq) res
      (contains? u-set (first l-seq)) res
      :else (recur (rest l-seq) 
                   (conj u-set (first l-seq)) 
                   (conj res (first l-seq))))))

