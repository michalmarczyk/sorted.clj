(ns sorted.core-test
  (:refer-clojure :exclude [sorted-map sorted-map-by sorted-set sorted-set-by])
  (:use clojure.test
        sorted.core)
  (:import (clojure.lang Sorted)))

(deftest sorted-map-test
  (testing "sorted-map"
    (let [m1 (sorted-map)
          c2 (comp - compare)
          m2 (sorted-map-by c2)]
      (are [m] (instance? Sorted m)
           m1
           m2)
      (are [m c] (identical? c (.comparator ^Sorted m))
           m1 compare
           m2 c2)
      (are [m] (zero? (count m))
           m1
           m2)
      (let [m1 (assoc m1 :foo 1 :bar 2 :quux 3)
            m2 (assoc m2 :foo 1 :bar 2 :quux 3)]
        (are [m] (== (count m) 3)
             m1
             m2)
        (are [m s] (= (seq m) s)
             m1 (list [:bar 2] [:foo 1] [:quux 3])
             m2 (list [:quux 3] [:foo 1] [:bar 2]))
        (are [m1 m2] (= (seq m1) (rseq m2))
             m1 m2
             m2 m1)
        (is (= (conj m1 [:wibble 4]) {:foo 1 :bar 2 :quux 3 :wibble 4}))
        (is (= (count (conj m1 [:wibble 4])) 4))
        (is (= (conj m2 [:wibble 4]) {:foo 1 :bar 2 :quux 3 :wibble 4}))
        (is (= (count (conj m2 [:wibble 4])) 4))
        (is (= (map key (assoc m1 nil 4)) (list nil :bar :foo :quux)))
        (is (= (map key (assoc m2 nil 4)) (list :quux :foo :bar nil)))))
    (let [m (->> [[0 10] [20 30] [10 20] [50 60] [30 40] [40 50]]
                 (mapcat (partial apply range))
                 (mapcat #(list % %))
                 (apply sorted-map))
          s1 (map #(vector % %) (range 60))
          s2 (map #(vector % %) (range 59 -1 -1))]
      (is (= (count m) 60))
      (is (= (seq m) s1))
      (is (= (rseq m) s2)))
    (let [m (sorted-map :foo 1 :bar 2 :quux 3)]
      (is (= (dissoc m :foo) (hash-map :bar 2 :quux 3)))
      (is (= (count (dissoc m :foo)) 2))
      (is (= (hash m) (hash (hash-map :foo 1 :bar 2 :quux 3))))
      (is (= (subseq m < :foo)  (list [:bar 2])))
      (is (= (subseq m <= :foo) (list [:bar 2] [:foo 1])))
      (is (= (subseq m > :foo)  (list [:quux 3])))
      (is (= (subseq m >= :foo) (list [:foo 1] [:quux 3])))
      (is (= (map #(reduce (fn [_ x] x) %) m) (list 2 1 3)))
      (is (= (map #(reduce (fn [x _] x) 7 %) m) (list 7 7 7))))))

(deftest sorted-set-test
  (testing "sorted-set"
    (let [s1 (sorted-set)
          c2 (comp - compare)
          s2 (sorted-set-by c2)
          c3 #(compare (quot %1 2) (quot %2 2))
          s3 (sorted-set-by c3)
          s4 (sorted-set-by <)]
      (are [s] (instance? Sorted s)
           s1
           s2)
      (is (identical? compare (.comparator ^Sorted s1)))
      (is (zero? (count s1)))
      (is (zero? (count s2)))
      (let [s1 (conj s1 1 2 3)
            s2 (conj s2 1 2 3)
            s3 (conj s3 1 2 3 7 8 9)
            s4 (conj s4 1 2 3)]
        (is (= (hash s1) (hash s2)))
        (is (= (hash s1) (hash #{1 2 3})))
        (is (= (seq s1)  (list 1 2 3)))
        (is (= (rseq s1) (list 3 2 1)))
        (is (= (seq s2)  (list 3 2 1)))
        (is (= (rseq s2) (list 1 2 3)))
        (is (= (count s1) 3))
        (is (= (count s2) 3))
        (is (= (count s3) 4))
        (is (= (get s3 0) 1))
        (is (= (subseq s3 > 5) (list 7 8)))
        (is (= (subseq s3 > 6) (list 8)))
        (is (= (subseq s3 >= 6) (list 7 8)))
        (is (= (subseq s3 >= 12) nil))
        (is (= (subseq s3 < 0) (list)))
        (is (= (subseq s3 < 5) (list 1 2)))
        (is (= (subseq s3 < 6) (list 1 2)))
        (is (= (subseq s3 <= 6) (list 1 2 7)))
        (is (= (subseq s3 >= 2 <= 6) (list 2 7)))
        (is (= (subseq s4 >= 2 < 3) (list 2)))
        (let [s1 (disj s1 2)
              s2 (disj s2 2)]
          (is (= (seq s1)  (list 1 3)))
          (is (= (rseq s1) (list 3 1)))
          (is (= (seq s2)  (list 3 1)))
          (is (= (rseq s2) (list 1 3)))
          (is (= (count s1) 2))
          (is (= (count s2) 2)))))))
