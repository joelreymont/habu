\ bootstrap-ptr-cell-mark-src.f - stage0 persisted-pointer definer execution.

\ PERSISTED-PTR-VARIABLE (src/core/pointer-storage.f) is the boot prefix's only
\ caller of `ptr-cell-mark`, so a stage0 generator without that primitive cannot
\ load the prefix. The seed has no snapshot address table: its ptr-cell-mark
\ consumes the cell address and registers nothing (bootstrap/cg/forth.fs
\ EMIT-MEMORY-PRIMS), which the definer's `create here ptr-cell-mark 0 ,` needs
\ exactly. A fresh slot reads null and round-trips a pointer; two slots are two
\ cells.

\ The definer's body is compiled before the check hook exists, so nothing types
\ its stack: a seed word consuming nothing leaks `here`, one consuming two
\ underflows, and neither disturbs the round-trip. PCM-DEPTH-KEPT brackets one
\ PERSISTED-PTR-VARIABLE at top level and requires the depth to come back.

variable PCM-FAILS

: PCM-FAIL ( -- )
   PCM-FAILS @ 1 + PCM-FAILS ! ;

: PCM-TRUE ( bool -- )
   0= if PCM-FAIL then ;

PTR-VARIABLE PCM-ZERO-SLOT               \ a declared slot nothing ever writes
create PCM-TARGET 0 ,
create PCM-OTHER 0 ,

PERSISTED-PTR-VARIABLE PCM-SLOT
PERSISTED-PTR-VARIABLE PCM-SLOT2

: PCM-NULL ( -- ptr n )
   PCM-ZERO-SLOT @ ;

: PCM-ADDR ( -- ptr ptr n )
   PCM-SLOT ;

: PCM-ADDR2 ( -- ptr ptr n )
   PCM-SLOT2 ;

: PCM-RUN ( -- )
   PCM-ADDR @ PCM-NULL = PCM-TRUE            \ a fresh persisted slot reads null
   PCM-ADDR2 @ PCM-NULL = PCM-TRUE
   PCM-ADDR PCM-ADDR2 = 0= PCM-TRUE          \ two definer calls, two cells
   PCM-TARGET PCM-ADDR !
   PCM-OTHER PCM-ADDR2 !
   PCM-ADDR @ PCM-TARGET = PCM-TRUE          \ what was stored is what is read
   PCM-ADDR2 @ PCM-OTHER = PCM-TRUE ;

variable PCM-DEPTH0

: PCM-DEPTH-MARK ( -- )
   depth PCM-DEPTH0 ! ;

: PCM-DEPTH-KEPT ( -- )
   depth PCM-DEPTH0 @ = PCM-TRUE ;

: PCM-REPORT ( -- )
   PCM-FAILS @ 0= if s" ok" type cr exit then
   PCM-FAILS @ . s" bootstrap-ptr-cell-mark failures" 1 die ;

PCM-DEPTH-MARK
PERSISTED-PTR-VARIABLE PCM-SLOT3
PCM-DEPTH-KEPT

PCM-RUN
PCM-REPORT
