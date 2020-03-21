(ns wonderland-number.finder)

(defn same-digits? [n1 n2] (apply = (map (comp set str) [n1 n2])) )

(defn times-1-to-6 [num] (map #(* num %) (range 1 7)))

(defn times-1-to-6-have-same-uniq-digs [num]
  (reduce #(if (same-digits? %1 %2)
             %1
             (reduced false))
          (times-1-to-6 num)))

(defn wonderland-number []
  (some times-1-to-6-have-same-uniq-digs (range 100000 166666)))