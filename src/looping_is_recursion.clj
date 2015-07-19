(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc exp]
                   (cond
                         (zero? exp) acc
                         :else (recur (* acc base) (dec exp) )))]
    (helper 1 exp)))

(defn last-element [a-seq]
 (let [helper (fn [s-count h-seq]
                 (cond (== 0 s-count) nil
                       (== 1 s-count) (first h-seq)
                       :else (recur (dec s-count) (rest h-seq) )))]
    (helper (count a-seq) a-seq)))

(defn seq= [seq1 seq2]
 (let [helper (fn [s1 s2]
                 (cond (and (empty? s1) (empty? s2)) true
                       (not= (count s1) (count s2)) false
                       (== (first s1) (first s2)) (recur (rest s1) (rest s2))
                       :else false))]
    (helper seq1 seq2)))

(defn find-first-index [pred a-seq]
  (loop [i 0
         rem-seq a-seq]
    (cond
      (empty? rem-seq) nil
      (pred (first rem-seq)) i
      :else (recur (inc i) (rest rem-seq)))))

(defn avg [a-seq]
  (loop [seq a-seq
         total  0
         v    0]
         (cond (empty? seq) (/ total v)
               :else (recur (rest seq) (+ (first seq) total) (inc v)))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (loop [set #{}
         seq a-seq]
         (cond (empty? seq) set
               :else (recur (toggle set (first seq)) (rest seq)))  ) )

(defn fast-fibo [n]
  (cond (= n 0) 0
        (= n 1) 1
        :else
          (loop [a 0
                 b 1
                 n1 (dec n)]
                 (if (= n1 0)
                    b
                    (recur b (+ a b) (dec n1))) )))

(defn cut-at-repetition [a-seq]
 (loop [o-elem #{}
        seq []
        rem-seq a-seq]
    (let [next-elem (first rem-seq)]
      (cond
        (empty? rem-seq) seq
        (contains? o-elem next-elem) seq
        :else
        (recur (conj o-elem next-elem)
             (conj seq next-elem)
             (rest rem-seq))))))

