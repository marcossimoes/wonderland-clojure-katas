(ns alphabet-cipher.coder-test
  (:require [clojure.test :refer :all]
            [alphabet-cipher.coder :refer :all]))

(deftest test-char->index
  (testing "returns an index ranging from 0 (A) to 25 (Z)"
    (is (= 0 (char->index \a)))
    (is (= 9 (char->index \j)))
    (is (= 10 (char->index \k)))
    (is (= 22 (char->index \w)))
    (is (= 24 (char->index \y)))
    (is (= 25 (char->index \z)))))

(deftest test-non-neg-int->letter
  (testing "returns a letter ranging from a (0 or any multiple of 26)
  to z (25 or any n that (n-25) is a multiple of 26"
    (is (= "a" (index->letter 0)))
    (is (= "b" (index->letter 1)))
    (is (= "z" (index->letter 25)))
    (is (= "a" (index->letter 26)))))

(deftest test-extended-keyword
  (testing "returns a a keyword that is equal or higher in size than the message"
    (is (= "bolabolabolabolabolabolabolabola" (extended-kwd "bola" "anticonstitucionalissimamente")))
    (is (= "bolabola" (extended-kwd "bola" "abobora")))
    (is (= "bola" (extended-kwd "bola" "boi")))))

(deftest test-encode-char
  (testing
    (is (= "e" (encode-char \m \s)))))

(deftest test-encode
  (testing "can encode a message with a secret keyword"
    (is (= "hmkbxebpxpmyllyrxiiqtoltfgzzv"
           (encode "vigilance" "meetmeontuesdayeveningatseven")))
    (is (= "egsgqwtahuiljgs"
           (encode "scones" "meetmebythetree")))))

(deftest test-decode
  (testing "can decode a message given an encoded message and a secret keyword"
    (is (= "meetmeontuesdayeveningatseven"
           (decode "vigilance" "hmkbxebpxpmyllyrxiiqtoltfgzzv")))
    (is (= "meetmebythetree"
           (decode "scones" "egsgqwtahuiljgs")))))

(deftest test-decipher
  (testing "can extract the secret keyword given an encrypted message and the original message"
    (is (= "vigilance"
           (decipher "opkyfipmfmwcvqoklyhxywgeecpvhelzg" "thequickbrownfoxjumpsoveralazydog")))
    (is (= "scones"
           (decipher "hcqxqqtqljmlzhwiivgbsapaiwcenmyu" "packmyboxwithfivedozenliquorjugs")))
    (is (= "abcabcx"
           (decipher "hfnlphoontutufa" "hellofromrussia")))))
