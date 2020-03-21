(ns alphabet-cipher.coder
  (:require [clojure.string :as str]))

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

(defn all-rem-parts-eql? [rem] (apply = rem))

(defn sub-list-of? [l1 l2] (= (take (count l1) l2) l1))

(defn decompose-in-eql-parts-of-size [ext-kwd size]
  (let [[last & rem] (reverse (partition size size nil ext-kwd))]
    (when (and (all-rem-parts-eql? rem)
               (sub-list-of? last (first rem)))
      (apply str (first rem)))))

(defn decompose-in-eql-parts-of [ext-kwd]
  (some (partial decompose-in-eql-parts-of-size ext-kwd) (range 1 (count ext-kwd))))

(defn decipher [cipher message]
  (let [ext-kwd (decode message cipher)]
    (decompose-in-eql-parts-of ext-kwd)))

