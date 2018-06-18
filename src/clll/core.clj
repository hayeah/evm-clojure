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

(defn resolve-binding [var scopes]
  "Return the stack position of a variable in lexical scopes"
  ; TODO need to record the stack offset for the start of a scope
  ; chain of lexical scopes look like '((a b) (a))
  (loop [scope (first scopes)
         rest-scopes (rest scopes)
         varname [first scope]
        ; stack depth
         i 0]
    (if (nil? varname) ; no more binding in this scope
      (if (empty? rest-scopes)
        ; nothing is found
        nil
        ; else, recurse to next scope
        (recur (first rest-scopes) (rest rest-scopes) (ffirst rest-scopes) i))
      (if (= var varname)
        i ; found the variable
        (recur (rest scope) rest-scopes (first scope) (+ i 1))))))

(defn rewrite-variables-inner [scopes exp depth-offset]
  "Walk an s-exp and rewrite symbolic variables"
  (cond
    (symbol? exp)
    (let [i (resolve-binding exp scopes)]
      (assert (not (nil? i)) (format "Cannot find variable: %s" exp))
        ;; TODO check if stack depth is exceeded...
      `(:dup ~(+ i depth-offset)))
    :else
    ;; TODO handle 'let' specially, tracking the offset at the start of a scope
    `(~(first exp)
      ;; track sub-expression offset, pushed onto the stack from right to left
      ~@(reverse (map-indexed
                  (fn [index subexp]
                    ;; each sub-expression would increase the stack by 1
                    (rewrite-variables-inner scopes subexp (+ index depth-offset)))
                  ;; reverse to treat the right sub-expression first
                  (reverse (rest exp)))))))

(defn rewrite-variables [scopes exp]
  (rewrite-variables-inner scopes exp 0))

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
