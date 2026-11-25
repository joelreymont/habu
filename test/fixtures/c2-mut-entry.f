\ c2-mut-entry.f - test/c2-memory-entry.f's case of a loaded file: its mutable
\ entry is rejected, rc 70.
require src/lib/memory.f
require src/lib/c2-memory.f

8 MEM:BYTES-ALLOC-LEN 0 C2-MEM:WITH-MUT
