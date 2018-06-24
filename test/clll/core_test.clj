(ns clll.core-test
  (:require [clojure.test :refer :all]
            [clll.core :refer :all]))

(deftest assembly-test
  (testing "Generate EVM assembly"
    (is (= (assembly '(10)) '(10)))
    (is (= (assembly '(add 11 12)) '(11 12 add)))
    (is (= (assembly '(add (add 11 12) 13)) '(11 12 add 13 add)))))

(deftest bytecode-test
  (testing "Convert symbolic assembly to bytecode"
    (is (= (bytecode '(10)) '('10)) "should quote a number constant")
    (is (= (bytecode '(1 2 add)) '('1 '2 0x01)) "should convert symbolic instruction to opcode")))

(deftest number-bytesize-test
  (testing "Figure out how many bytes a number will occupy"
    (is (= (number-bytesize 0xff) 1))
    (is (= (number-bytesize 0x0) 1))
    (is (= (number-bytesize 0xaa) 1))

    (is (= (number-bytesize 0xaabb) 2))
    (is (= (number-bytesize 0xffff) 2))

    (is (= (number-bytesize 0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff) 32))))

(deftest binary-test
  (testing "Generate bytecode hexstring"
    (is (= (binary '('0x1)) '("6001")) "should convert number constant to push1")
    (is (= (binary '('0xffff)) '("61ffff")) "should convert 2 bytes constant to push2")

    (is (= (binary '('0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff))
           '("7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"))
        "should c[onvert 32 bytes constant to push32")
    (is (= (binary '('1 '2 0x1)) '("6001" "6002" "01")) "should normal opcode to hex code")))

(deftest resolve-binding-test
  (testing "lexical binding resolution"
    (is (= (resolve-binding 'a '()) nil) "resolves to nil if there is no binding")
    (is (= (resolve-binding 'a '((a))) 1))
    (is (= (resolve-binding 'a '((a b))) 1))
    (is (= (resolve-binding 'b '((a b))) 2))

    (is (= (resolve-binding 'b '((b c) (a b))) 1) "b in first scope should shadow the later scope")
    (is (= (resolve-binding 'a '((b c) (a b))) 3) "look for a in outter scope")))

(deftest rewrite-variables-test
  (testing "rewrite variables in sexp"
    (is (= (rewrite-variables '((a)) '(foo a a a)) '(foo (:dup 3) (:dup 2) (:dup 1))))
    (is (= (rewrite-variables '((a b)) '(foo b b)) '(foo (:dup 3) (:dup 2))))))

(deftest analyze-jump-destinations-test
  (testing "analyze jump destinations"
    (let [{zero :zero foo :foo bar :bar}
          (analyze-jump-destinations
           '((:jumpdest :zero) ; 0
             (:jump :foo) ; 1
             (:jump :bar) ; 4
             (:null) ; 7
             (:null) ; 8
             (:jumpdest :foo) ; 9
             (:null) ; 10
             (:null) ; 11
             ; 12
             (:jumpdest :bar)))]
      (is (= zero 0))
      (is (= foo 9))
      (is (= bar 12)))))
