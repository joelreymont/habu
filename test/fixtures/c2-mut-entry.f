\ c2-mut-entry.f - test/c2-memory-entry.f's case for the interpreter written in
\ Habu (test/interpreter-loop-on.f first): it refuses the mutable entry, rc 70.
require src/lib/memory.f
require src/lib/c2-memory.f

8 MEM:BYTES-ALLOC-LEN 0 C2-MEM:WITH-MUT
