\ bootstrap-ptr-cell-mark-src.f - stage0 persisted-pointer definer execution.

\ PERSISTED-PTR-VARIABLE (src/core/pointer-storage.f) is the boot prefix's only
\ caller of the engine primitive `ptr-cell-mark`, so a stage0 generator that does
\ not register that primitive cannot load the prefix at all and every stage0 build
\ dies naming the token. The seed has no snapshot address table, so its
\ ptr-cell-mark consumes the cell address and registers nothing
\ (bootstrap/cg/forth.fs EMIT-MEMORY-PRIMS); that is only right if it consumes
\ exactly the address, which is what the definer's `create here ptr-cell-mark 0 ,`
\ depends on. So exercise the definer, not just the token: a fresh persisted slot
\ reads null, takes a pointer and reads the same pointer back, and two slots are
\ two distinct cells.
\
\ The definer's body is compiled before the check hook exists, so nothing types
\ its stack for us; a seed word that consumed nothing would silently leak `here`
\ and a seed word that consumed two cells would underflow, in both cases without
\ disturbing the round-trip above. PCM-DEPTH-KEPT is the assertion that catches
\ that: it brackets one PERSISTED-PTR-VARIABLE at the top level and requires the
\ data stack to come back to the depth it went in at.

variable PCM-FAILS

: PCM-FAIL ( -- )
   PCM-FAILS @ 1 + PCM-FAILS ! ;

: PCM-TRUE ( bool -- )
   0= if PCM-FAIL then ;

create PCM-ZERO-CELL 0 ,
create PCM-TARGET 0 ,
create PCM-OTHER 0 ,

PERSISTED-PTR-VARIABLE PCM-SLOT
PERSISTED-PTR-VARIABLE PCM-SLOT2

: PCM-NULL ( -- ptr n )
   PCM-ZERO-CELL 0 ptr-field @ ;

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
