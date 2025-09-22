(ns hashids.util-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hashids.util :as util]))

(deftest test-long->hexstr
  (is (= "2a" (util/long->hexstr 42)))
  (is (= 3735928559 (util/hexstr->long "deadbeef")))
  (is (= "deadbeef" (util/long->hexstr 3735928559))))

(deftest test-hexstr->long
  (is (= 42 (util/hexstr->long "2a")))
  (is (= 3735928559 (util/hexstr->long "deadbeef"))))

(deftest test-hexstr->long-bad-input
  (is (= nil (util/hexstr->long "XYZ"))))


(deftest split-on-chars-test
  (is (= '((\p \w) (\n) (\V \M \X \3)) (util/split-on-chars "pwcnfVMX3" "cfhistuCFHISTU"))))

(deftest positions-test
  (is (= '(1 2 4) (util/positions even? '(1 20 22 17 12 19)))))

(defspec split-on-chars-never-returns-splitchars
  1000
  (prop/for-all [instr    gen/string
                 splitstr gen/string]
    (is (every? (fn [s]
                  (empty? (clojure.set/intersection (set s) (set splitstr))))
                (util/split-on-chars instr splitstr)))))

(defspec strip-whitespace-never-returns-whitespace
  ;;"consistent-shuffle returns something other than the alphabet for a non-empty string"
  1000
  (prop/for-all [s gen/string]
    (is (not-any? clojure.string/blank? (map str (util/strip-whitespace s))))))

