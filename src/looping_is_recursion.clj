(ns looping-is-recursion)

(defn power [base exp]
   (let [helper (fn [n exp] 
     (cond 
       (= exp 0) 1 
        (= exp 1) n 
     :else 
         (recur (* n base) (dec exp))))] 
          (helper base exp)
     ))

(defn last-element [a-seq]
  (cond
    (empty? a-seq) nil
      (= (count a-seq) 1) (first a-seq)
      :else (recur (rest a-seq))   
   ))

(defn seq= [seq1 seq2]
  (cond
    (and (empty? seq1) (empty? seq2)) true
     (or (empty? seq1) (empty? seq2)) false
       (not= (first seq1) (first seq2)) false
        :else (recur (rest seq1) (rest seq2))
   ))

(defn find-first-index [pred a-seq]
  (loop [n 0 a a-seq p pred] (
    cond (empty? a) nil
      (p (first a)) n
      :else (recur (inc n) (rest a) p) 
    )))

(defn avg [a-seq]
  (cond 
    (empty? a-seq) nil
     :else
       (loop [a a-seq sum 0 number 0]
         (if (empty? a)
           (/ sum number)
             (recur (rest a) (+ sum (first a)) (inc number)))
         )
   ))

(defn parity [a-seq]
  (loop [a a-seq m #{}] 
    (cond (empty? a) m 
       (contains? m (first a)) (recur (rest a) (disj m (first a))) 
        :else 
        (recur (rest a) (conj m (first a))))

    ))

(defn fast-fibo [n]
  (loop [f-0 0 f-1 1 m n]  
     (cond (= m 0) f-0
        :else (recur f-1 (+ f-0 f-1) (dec m))
      )
    ))

(defn cut-at-repetition [a-seq]
  (loop [new-seq [] a a-seq] (
    cond (empty? a) new-seq
      (some (partial = (first a)) new-seq) new-seq
         :else (recur (conj new-seq (first a)) (rest a))
  )))