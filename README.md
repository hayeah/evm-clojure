A EVM Low Level Lisp compiler, in Clojure.

Start REPL:

```
lein repl
```

Then load the module

```
(require 'clll.core :reload)
```

Compile a minimal program:

```
(clll.core/genbinary '[
    (mstore 0 0xaabbcc)
    (return 0 32)
])

; => "62aabbcc60005260206000f3"
```

Run with EVM CLI:

```
evm  --statdump --code 62aabbcc60005260206000f3 --input aabb run
```

```
evm execution time: 119.82µs
heap objects:       10424
allocations:        1516432
total allocations:  1516432
GC calls:           0
Gas used:           18

0x0000000000000000000000000000000000000000000000000000000000aabbcc
```

# A Minimal Deployable Contract

```
(def code '[
  (codecopy 0x0 (blockoffset :contract) (blocksize :contract))
  (return 0x0 (blocksize :contract))

  (block :contract
    (mstore 0 0xaabbcc)
    (return 0 32))
])
```

Compile expression to instructions:

```
=> (clll.core/genassembly code)
((:blocksize :contract) (:blockoffset :contract) 0 :codecopy (:blocksize :contract) 0 :return (:block :contract 11189196 0 :mstore 32 0 :return))
```

Convert instructions to bytecode:

```
=> (clll.core/genbinary code)
"600c600c600039600c6000f35b62aabbcc60005260206000f3"
```
