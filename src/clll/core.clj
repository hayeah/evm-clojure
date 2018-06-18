(ns clll.core
  (:gen-class))

(require '[clojure.core.match :refer [match]])

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(def invalid-opcode 0xfe)
(def opcode-push 0x60)

(def opcodes {'push1 opcode-push ; ... push32
              'add 0x01
              'invalid invalid-opcode})

; assembly: expression -> assembly (list of symbolic instuctions)
; bytecode: assembly -> bytecode (list of opcodes)
; binary: bytecode -> binary (string of hex bytes)

(defn assembly [exp]
  "Recursively expand an expression into stack operations"
  (cond
    ;; (n: number) => push n
    (number? exp) `(~exp)
    :else
    (let [inst (first exp)
          args (rest exp)]
      `(~@(mapcat assembly args) ~inst))))

(defn instruction-bytecode [inst]
  "Convert a single instruction to opcodes"
  (cond
    (number? inst) `(quote ~inst)
    (symbol? inst) (opcodes inst invalid-opcode)))

(defn bytecode [insts]
  "Take a list of assembly instructions and produce bytecode"
  (map instruction-bytecode insts))

; (defn number-bytesize [n]
;   "Number of bytes that can contain an integer"
;   (if (= n 0) 1
;       (loop [bytes 1
;              i n]
;         (let [i2 (bit-shift-right i 8)]
;           (if (= i2 0) bytes (recur (+ 1 bytes) i2))))))

(defn number-bytesize [n]
  "Number of bytes that can contain an integer"
  (max (int (/ (.bitLength (bigint n)) 8))
       1))

(defn push-bytecode [n]
  "Generate a push instruction depending on the size of the constant number"
  (let [nbytes (number-bytesize n)
        hexstr (str "%0" (* nbytes 2) "x")
        opcode (+ opcode-push (- nbytes 1))]
      ; FIXME: check within 32 bytes
    (str (format "%02x" opcode) (format hexstr (biginteger n)))))

(defn binary [opcodes]
  (map (fn [opcode]
         (match [opcode]
      ; convert constant numbers to 32 bytes hex string
           [(['quote n] :seq)] (push-bytecode n)
           :else (format "%02x" opcode)))
       opcodes))

(defn genassembly [exp]
  "Compile a program to assembly"
  (assembly exp))

(defn genbytecode [exp]
  "Compile a program to bytecode"
  (bytecode (genassembly exp)))

(defn genbinary [exp]
  "Compile a program to binary hex string"
  (apply str (binary (genbytecode exp))))

;; (hello.core/serialize '(mstore (add 1 2) 3))
