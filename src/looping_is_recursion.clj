(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc base exp]
                 (cond
                   (zero? exp) 1
                   (= exp 1) acc
                   :else (recur (* acc base) base (dec exp))))]
    (helper base base exp)))

(defn last-element [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (recur (rest a-seq))))

(defn seq= [seq1 seq2]
  (let [helper (fn [acc seq1 seq2]
                 (cond
                   (and (empty? seq1) (empty? seq2)) true
                   (or (empty? seq1) (empty? seq2)) false
                   (= (first seq1) (first seq2)) (recur true (rest seq1) (rest seq2))
                   :else false))]
    (helper false seq1 seq2)))

(defn find-first-index [pred a-seq]
  (loop [idx 0
         my-seq a-seq]
    (cond
      (empty? my-seq) nil
      (pred (first my-seq)) idx
      :else (recur (inc idx) (rest my-seq)))))

(defn avg [a-seq]
  (loop [acc 0
         total-elems 0
         my-seq a-seq]
    (if (empty? my-seq)
      (/ acc total-elems)
      (recur (+ acc (first my-seq))
             (inc total-elems)
             (rest my-seq)))))

(defn keep-if [pred coll]
  (loop [new-coll '()
         my-coll coll]
    (cond
      (empty? my-coll) new-coll
      (pred (first my-coll)) (recur (cons (first my-coll) new-coll) (rest my-coll))
      :else (recur new-coll (rest my-coll)))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (loop [my-set #{}
         my-seq a-seq]
    (if (empty? my-seq)
      my-set
      (recur (toggle my-set (first my-seq))
             (rest my-seq)))))

; f(4) => f(3) + f(2)
; f(3) => f(2) + f(1)
; f(2) => f(1) + f(0)
; f(1) => 1
; f(0) => 0

; (f 4) => ((f 2) + (f 1)) + ((f 2) + f(0))
; (f 4) => (((f 1) + 1)) + ((f 1) + ((f 0) + 0))
; (f 4) => (1 + 1) + (1 + 0)
; (f 4) => 3

(defn fast-fibo [n]
  (loop [remaining n
         curr-num 0
         next-num 1]
    (if (zero? remaining)
      curr-num
      (recur (- remaining 1)
             next-num
             (+ curr-num next-num)))))

; (ff 4)
; (recur 3 1 1)
; (recur 2 1 2)
; (recur 1 2 3)
; (recur 0 3 5)
; 3

(defn cut-at-repetition [a-seq]
  (loop [acc []
         my-seq a-seq]
    (cond
      (contains? (set acc) (first my-seq)) acc
      (nil? (first my-seq)) acc
      :else (recur (conj acc (first my-seq)) (rest my-seq)))))

