(ns clll.core
  (:gen-class))

(require '[clojure.core.match :refer [match]])

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(def invalid-opcode 0xfe)
(def opcode-push 0x60)

(def opcodes {:push1 opcode-push ; ... push32

              :stop 0x0
              :add 0x01

              :codecopy 0x39

              :mstore 0x52

              :jump 0x56
              :jumpi 0x57
              :jumpdest 0x5b

              :return 0xf3
              :invalid invalid-opcode
              :null invalid-opcode})

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

; compile: map of blocks -> map of assembly
; assembly: expression -> assembly (list of symbolic instuctions)
; bytecode: assembly -> bytecode (list of opcodes)
; binary: bytecode -> binary (string of hex bytes)

(defn assembly [exp]
  "Recursively expand an expression into stack operations"
  (cond
    ;; (n: number) => push n
    (number? exp) `(~exp)
    (vector? exp) (mapcat assembly exp)
    :else
    (let [inst (keyword (first exp))
          args (rest exp)]
      (case inst
        :block `((:block ~(second exp) ~@(mapcat assembly (drop 2 exp))))
        :blockoffset `((:blockoffset ~(second exp))) ; TODO check second is symbol
        :blocksize `((:blocksize ~(second exp))) ; TODO check second is symbol
          ; A normal instruction.
          ; TODO: verify instruction is valid
        `(~@(mapcat assembly (reverse args)) ~inst)))))

(defn instruction-bytecode [inst]
  "Convert a single instruction to opcodes"
  (cond
    (number? inst) `(quote ~inst)
    (symbol? inst) (opcodes (keyword inst) invalid-opcode)
    (keyword? inst) (opcodes inst invalid-opcode)))

; (defn number-bytesize [n]
;   "Number of bytes that can contain an integer"
;   (if (= n 0) 1
;       (loop [bytes 1
;              i n]
;         (let [i2 (bit-shift-right i 8)]
;           (if (= i2 0) bytes (recur (+ 1 bytes) i2))))))

(defn number-bytesize [n]
  "Number of bytes that can contain an integer"
  (max (int (Math/ceil  (/ (.bitLength (bigint n)) 8)))
       1))

(defn push-bytecode [n]
  "Generate a push instruction depending on the size of the constant number"
  (let [nbytes (number-bytesize n)
        hexstr (str "%0" (* nbytes 2) "x")
        opcode (+ opcode-push (- nbytes 1))]
      ; FIXME: check within 32 bytes
    (str (format "%02x" opcode) (format hexstr (biginteger n)))))

(defn analyze-jump-destinations [_instructions & {:keys [_jumps _destinations _offset]}]
  "Analyze the jump destinations offsets"
  ; Iterate through instructions, assume that jump destinations are all 1 bytes initially.
  ; Track actual jump destinations, and if any jump destination turns out to be multiple bytes,
  ; reajust assumptions and recalcuate again.
  (loop
   [instruction (first _instructions)
    instructions (rest _instructions)
    offset (or _offset 0)
    ; jumps encountered, and their assumed destination size
    jumps (or _jumps {})
    ; actual destinations
    destinations (or _destinations {})

    ; number of bytes needed to push blocksize
    assumed-blocksize-push {}

    blocksizes {}]

    ; (println offset)

    (if (nil? instruction) ; done. check result
      (do
        ; TODO check jumping to unknown destination
        ; compare each jump size assumption with actual destination size. Retry if necessary
        (let [destination-sizes (into {} (for [[k v] destinations] [k (number-bytesize v)]))
              actual-blocksize-push (into {} (for [[k v] blocksizes] [k (number-bytesize v)]))
              new-assumed-blocksize-push (merge assumed-blocksize-push actual-blocksize-push)
              new-jumps (merge jumps destination-sizes)]
            ; (println "a" jumps offset destinations destination-sizes new-jumps)
          ; (println :compare-block-push assumed-blocksize-push new-assumed-blocksize-push)
          (if (and (= jumps new-jumps) (= new-assumed-blocksize-push assumed-blocksize-push))
            {:jumpdests destinations :offset offset :jumps new-jumps :blocksizes blocksizes} ; jump size assumptions are correct. return.
            (let [instructions _instructions]
                ; jump size assumptions violated. go back and try again with new assumptions
              (recur (first instructions) (rest instructions) 0 new-jumps {} new-assumed-blocksize-push {})))))
      (cond
        (keyword? instruction) ; normal instruction considered one byte
        (recur (first instructions) (rest instructions) (+ 1 offset) jumps destinations assumed-blocksize-push blocksizes)
        (number? instruction) ; number is push. calculate push size
        (recur (first instructions) (rest instructions) (+ 1 (number-bytesize instruction) offset) jumps destinations assumed-blocksize-push blocksizes)
        :else ; special forms
        (let [head (first instruction)]
          (case head
            :block
            (let [label (second instruction) blockbody (drop 2 instruction)]
              (do
              ; (println :block label offset)
                (let [blockstart (+ offset 1)
                      result (analyze-jump-destinations blockbody :_jumps jumps :_destinations destinations :_offset blockstart)
                      {destinations :jumpdests jumps :jumps blockend :offset} result
                    ; jumpdest for the block
                      destinations (assoc destinations label offset)
                    ; block size excludes the jumpdest
                      blocksize (- blockend blockstart)
                      blocksizes (assoc blocksizes label blocksize)]
                  ; (println :block label blockbody)
                  (recur (first instructions) (rest instructions) blockend jumps destinations assumed-blocksize-push blocksizes))))
            :blockoffset
            (let [label (second instruction)
                  seen-jump (contains? jumps label)
                  dest-size (if seen-jump (jumps label) (number-bytesize offset))
                  jumps (if seen-jump jumps (assoc jumps label dest-size))
                  ; push(n)
                  push-size (+ 1 dest-size)
                  offset (+ offset push-size)]
              (recur (first instructions) (rest instructions) offset jumps destinations assumed-blocksize-push blocksizes))
            :blocksize
            (let [label (second instruction)
                  seen-block (contains? assumed-blocksize-push label)
                  block-size (if seen-block (assumed-blocksize-push label) (number-bytesize offset))
                  assumed-blocksize-push (if seen-block assumed-blocksize-push (assoc assumed-blocksize-push label block-size))
                  ; push(n)
                  push-size (+ 1 block-size)
                  offset (+ offset push-size)]
              (recur (first instructions) (rest instructions) offset jumps destinations
                     assumed-blocksize-push blocksizes))
            :jump
            (let [label (second instruction)
                  seen-jump (contains? jumps label)
                  dest-size (if seen-jump (jumps label) (number-bytesize offset))
                  jumps (if seen-jump jumps (assoc jumps label dest-size))
                  ; push(n) dest, jump
                  jump-size (+ 2 dest-size)
                  offset (+ offset jump-size)]
              (recur (first instructions) (rest instructions) offset jumps destinations assumed-blocksize-push blocksizes))
            :jumpdest
            (let [label (second instruction)]
              (do (assert (not (contains? destinations label)) "jumpdest label should be unique")
                  (let [destinations (assoc destinations label offset)
                        offset (+ offset 1)]
                    (recur (first instructions) (rest instructions) offset jumps destinations assumed-blocksize-push blocksizes))))
            ; else
            ; (let [instruction-size 1
            ;       offset (+ offset instruction-size)]
            ;   (recur (first instructions) (rest instructions) offset jumps destinations assumed-blocksize-push blocksizes))
))))))

(defn opcodehex [opcode]
  (format "%02x" (opcodes opcode invalid-opcode)))

(defn bytecode [insts]
  "Take a list of assembly instructions and produce bytecode"
  (let [{jumpdests :jumpdests blocksizes :blocksizes} (analyze-jump-destinations insts)]
    (flatten (map (fn [inst]
                    (cond
                      (number? inst) (push-bytecode inst)
                      (keyword? inst) (opcodehex inst)
                      :else
                      ; special forms
                      (case (first inst)
                        :jump
                        (let [label (second inst)]
                          (bytecode `(~(jumpdests label) :jump)))
                        :jumpdest (opcodehex :jumpdest)
                        :block `(~(opcodehex :jumpdest) ~@(bytecode (drop 2 inst)))
                        :blockoffset
                        (let [label (second inst)]
                          `(~(push-bytecode (jumpdests label))))
                        :blocksize
                        (let [label (second inst)]
                          `(~(push-bytecode (blocksizes label))))))) insts))))

(defn compile [blocks]
  "Compile map of named blocks to assembly")

(defn genassembly [exp]
  "Compile a program to assembly"
  (assembly exp))

(defn genbytecode [exp]
  "Compile a program to bytecode"
  (bytecode (genassembly exp)))

(defn genbinary [exp]
  "Compile a program to binary hex string"
  (apply str (genbytecode exp)))
