(ns clll.core-test
  (:require [clojure.test :refer :all]
            [clll.core :refer :all]))

(deftest assembly-test
  (testing "Generate EVM assembly"
    (is (= (assembly 10) '(10)))
    (is (= (assembly '(add 11 12)) '(12 11 :add)))
    (is (= (assembly '(add (add 11 12) 13)) '(13 12 11 :add :add)))
    (is (= (assembly '[(add 1 2) 3 4]) '(2 1 :add 3 4)))

    (is (= (assembly '[(block :foo 1 2) 3 4]) '((:block :foo 1 2) 3 4)))
    (is (= (assembly '(blocksize :foo)) '((:blocksize :foo))))
    (is (= (assembly '(blockoffset :foo)) '((:blockoffset :foo))))

    (is (= (assembly '[(codecopy 0x0 (blockoffset :contract) (blocksize :contract))
                       (return 0x0 (blocksize :contract))

                       (block :contract
                              (:mstore 0 0xaabbcc)
                              (:return 0 32))])
            ; output
           '((:blocksize :contract) (:blockoffset :contract) 0 :codecopy (:blocksize :contract) 0 :return (:block :contract 11189196 0 :mstore 32 0 :return))))))

(deftest bytecode-test
  (testing "Convert symbolic assembly to bytecode"
    (is (= (bytecode '(0xaabb)) '("61aabb")))
    (is (= (bytecode '(1 2 :add)) '("6001" "6002" "01")))

    (is (= (bytecode '((:jump :foo) :null :null (:jumpdest :foo) :null))
           '("6005" "56" "fe" "fe" "5b" "fe")))

    (is (= (bytecode '((:block :foo :null :null)))
           '("5b" "fe" "fe")))

    (is (= (bytecode '((:blockoffset :foo) (:block :foo :null :null)))
           '("6002" "5b" "fe" "fe")))

    (is (= (bytecode '((:blocksize :foo) (:block :foo :null)))
           '("6001" "5b" "fe")))))

(deftest number-bytesize-test
  (testing "Figure out how many bytes a number will occupy"
    (is (= (number-bytesize 0xff) 1))
    (is (= (number-bytesize 0x0) 1))
    (is (= (number-bytesize 0xaa) 1))

    (is (= (number-bytesize 0x100) 2))
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
          ((analyze-jump-destinations
            '((:jumpdest :zero) ; 0
              (:jump :foo) ; 1
              (:jump :bar) ; 4
              :null ; 7
              :null ; 8
              (:jumpdest :foo) ; 9
              :null ; 10
              :null ; 11
             ; 12
              (:jumpdest :bar))) :jumpdests)]
      (is (= zero 0))
      (is (= foo 9))
      (is (= bar 12)))

    (let [{foo :foo}
          ((analyze-jump-destinations
            `((:jump :foo) ; push1 0xXX, jump  (3)
              ~@(for [x (range 126)] 0x00) ; 252
              (:jumpdest :foo))) :jumpdests)]
      (is (= foo 255)))

    (let [{foo :foo}
          ((analyze-jump-destinations
            `((:jump :foo) ; push2 0xXXXX, jump  (4)
              ~@(for [x (range 126)] 0x00) ; 252
              :null
              (:jumpdest :foo))) :jumpdests)]
      (is (= foo 257)))

    (let [{foo :foo bar :bar}
          ((analyze-jump-destinations
            '(:null (:jumpdest :foo) (:jumpdest :bar))) :jumpdests)]
      (is (= foo 1))
      (is (= bar 2)))

    (let [{foo :foo}
          ((analyze-jump-destinations
            `((:jump :foo)
              ~@(for [x (range 252)] :null)
              (:jumpdest :foo))) :jumpdests)]
      (is (= foo 255)))

    (let [{foo :foo bar :bar}
          ((analyze-jump-destinations
            `((:jumpdest :bar)
              (:jump :foo) ; push2 0x0101, jump  (4)
              ~@(for [x (range 253)] :null) ; 253
              (:jumpdest :foo))) :jumpdests)]
      (is (= foo 258))
      (is (= bar 0)))

    (let [result
          (analyze-jump-destinations
           `((:jump :foo) ; push1 0x0, jump (2)
            ; 3
             (:block :foo :null :stop)))
          {size :offset jumpdests :jumpdests blocksizes :blocksizes} result
          {foo :foo} jumpdests]
      (is (= (blocksizes :foo) 2))
      (is (= foo 3))
      (is (= size 6)))

    (let [result
          (analyze-jump-destinations
           `((:blockoffset :foo) ; push1 0x1 (2)
             (:block :foo :stop)))
          {size :offset jumpdests :jumpdests blocksizes :blocksizes} result
          {foo :foo} jumpdests]
      (is (= (blocksizes :foo) 1))
      (is (= foo 2))
      (is (= size 4)))

    (let [result
          (analyze-jump-destinations
           `((:blockoffset :foo) ; push2 0xXXXX (3)
             ~@(for [x (range 254)] :null)
             (:block :foo :stop)))
          {size :offset jumpdests :jumpdests blocksizes :blocksizes} result
          {foo :foo} jumpdests]
      (is (= foo 257)))

    (let [result
          (analyze-jump-destinations
           `((:blocksize :foo) ; push1 0xXX (3)
             (:block :foo :null)))
          {size :offset jumpdests :jumpdests blocksizes :blocksizes} result
          {foo :foo} jumpdests]
      (is (= foo 2))
      (is (= (blocksizes :foo) 1)))

    (let [result
          (analyze-jump-destinations
           `((:blocksize :foo) ; push2 0xXXXX (3)
             (:block :foo ~@(for [x (range 256)] :null))))
          {size :offset jumpdests :jumpdests blocksizes :blocksizes} result
          {foo :foo} jumpdests]
      (is (= foo 3)))

    (let [result
          (analyze-jump-destinations
           '((:blocksize :contract) ; push1 12 (2)
             (:blockoffset :contract) ; push1 12 (2)
             0 ; push1 0x0 (2)
             :codecopy
             (:blocksize :contract) ; push1 12 (2)
             0 ; push1 0x0 (2)
             :return
            ; offset 12
             (:block :contract
                     0xaabbcc ; push3 0xaabbcc (4)
                     0 ; push1 0x0 (2)
                     :mstore
                     32 ; push1 0x32 (2)
                     0  ; push1 0x32 (2)
                    ; size: 12
                     :return)))
          {size :offset jumpdests :jumpdests blocksizes :blocksizes} result
          {contract :contract} jumpdests]
      (is (= contract 12))
      (is (= (blocksizes :contract) 12)))))

