\ Run after restoring the full image, so ENTRY must reuse its baked C entry.
1 set-tier
require src/lib/ffi-abi.f

PROCESS-SYMBOLS
FUNCTION: QSORT qsort ( ptr u8 n n n -- ) 0 32 WRITES-BYTES ;FUNCTION

create ROW 4 cells allot

: RUN ( -- )
   4 ROW ! 3 ROW cell+ ! 2 ROW 2 cells + ! 1 ROW 3 cells + !
   SAVED-COMPARE TASK:SELF-CONTEXT FFI-CB:ENTRY {: entry:n :}
   ROW BYTE-VIEW 4 CELL entry QSORT
   SAVED-COMPARE FFI-CB:UNBIND
   ROW @ 1 = ROW cell+ @ 2 = and ROW 2 cells + @ 3 = and
   ROW 3 cells + @ 4 = and 0= if 74 throw then
   s" full-image-callback-rebind: ok" type cr ;

RUN
