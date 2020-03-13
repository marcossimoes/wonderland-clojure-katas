(ns alphabet-cipher.coder)

(defn char->index
  "receives a capital letter in char format and returns an index from 0 (A) to 25 (Z)"
  [c]
  (- (int c) 97))

(defn index->letter
  "receives a positive integer i and returns the corresponding letter where
  0 = A, 1 = B ..., 25 = Z
  26 = A, ..."
  [non-neg-int]
  (-> non-neg-int
      (mod 26)
      (+ 97)
      (char)
      (str)))

(defn extended-kwd
  "extended keyword equal or higher in size than the message"
  [keyword message]
  (nth (iterate (partial str keyword) "")
       (inc (quot (count message)
                  (count keyword)))))

(defn encode-char
  "given a keyword char and a message char, returns the encoded-char"
  [k-char m-char]
  (index->letter
    (+ (char->index k-char)
       (char->index m-char))))

(defn encode [keyword message]
  (let [extnd-kwd (extended-kwd keyword message)]
    (apply str (map encode-char extnd-kwd message))))

(defn decode-char
  [k-char m-char]
  (index->letter
    (+ 26
       (- (char->index m-char)
          (char->index k-char)))))

(defn decode
  [keyword message]
  (let [extnd-kwd (extended-kwd keyword message)]
    (apply str (map decode-char extnd-kwd message))))

(defn decipher
  ([cipher message] (decipher cipher message 1))
  ([cipher message num-parts]
   (let [parts (partition num-parts (decode message cipher))]
     (if #nu/tap (apply = parts)
       #nu/tap (apply str (first parts))
       (decipher cipher message (inc num-parts))))))