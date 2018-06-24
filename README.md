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
