\ maki/batch-loader-test.f - the BTC-4 acceptance (maki/batch-loader.f). Proves the
\ loader shapes B windows of length T into the B*T-row buffer with B OUTERMOST, and
\ fails closed on capacity.
\
\ Fixture: an IDENTITY corpus (token id == position). A window starting at s then
\ reads back ids = s,s+1,...,s+T-1 and targets = s+1,...,s+T, so contiguity and the
\ one-step target shift are checkable directly against whatever start the sampler
\ picks, and the B-outermost block layout is pinned (row b*T+t == block-b start + t).
\ Also: determinism under a fixed seed, the emitted segment attribute round-trips T,
\ and the capacity/domain rejects throw named errors without writing a partial batch.
\ Names are BLT-prefixed: the maki suite shares one dictionary across every -test.f.

require lib/test.f
require maki/examples/nanogpt/batch-loader.f
require maki/segment.f

package MAKI

20 constant BLT-CLEN            \ corpus length (>= any test's T+1)
2  constant BLT-B               \ sequences
3  constant BLT-T               \ tokens per sequence
4  constant BLT-DIM             \ embedding width (arena factor)
6  constant BLT-ROWS            \ B*T
7  constant BLT-SEED            \ fixed sampler seed

create BLT-CORPUS  BLT-CLEN cells allot
create BLT-IDS0    BLT-ROWS cells allot   \ determinism snapshot: ids
create BLT-TGT0    BLT-ROWS cells allot   \ determinism snapshot: targets

: BLT-FILL-CORPUS ( -- )  BLT-CLEN 0 ?do  i s>f  BLT-CORPUS i T-SET  loop ;
: BLT-LOAD ( -- )  BLT-CORPUS BLT-CLEN BLT-B BLT-T BLT-DIM BLT-SEED  BL-LOAD ;

\ exact bit compare of n cells (targets/ids are small integers held as floats)
: BLT-EQ? ( ptr r ptr r n -- bool ) {: a:ptr b:ptr n:n :}
   n 0 ?do  a i T-GET  b i T-GET  f= 0= if false unloop exit then  loop  true ;

\ contiguity + B-outermost layout + one-step target shift, all at once: for every
\ row i = b*T+t, block b's first id (BL-IDS[b*T]) is its window start, so BL-IDS[i]
\ must equal that start + t and BL-TGT[i] the token after it.
: BLT-WINDOWS-OK? ( -- bool )
   BLT-ROWS 0 ?do
      i BLT-T /    {: b:n :}                         \ window index of row i
      i BLT-T mod  {: t:n :}                         \ position within the window
      BL-IDS  b BLT-T *  T-GET  {: start:r :}         \ block b's window start
      BL-IDS  i  T-GET   start t s>f f+        f= 0= if false unloop exit then
      BL-TGT  i  T-GET   start t s>f f+ 1.0 f+ f= 0= if false unloop exit then
   loop  true ;

\ same seed reproduces the exact batch (compare re-load against the snapshot)
: BLT-DETERMINISTIC? ( -- bool )
   BLT-LOAD
   BLT-ROWS 0 ?do  BL-IDS i T-GET BLT-IDS0 i T-SET  BL-TGT i T-GET BLT-TGT0 i T-SET  loop
   BLT-LOAD
   BL-IDS BLT-IDS0 BLT-ROWS BLT-EQ?
   BL-TGT BLT-TGT0 BLT-ROWS BLT-EQ?  and ;

\ capacity/domain rejects (each must throw before any row is written)
: BLT-TRY-ROWS  ( -- )  BLT-CORPUS BLT-CLEN 13 10 4   BLT-SEED BL-LOAD ;   \ B*T=130 > 128 row cap
: BLT-TRY-ARENA ( -- )  BLT-CORPUS BLT-CLEN 8 16 300  BLT-SEED BL-LOAD ;   \ B*T=128 ok, 128*300 > 32768 arena
: BLT-TRY-SHORT ( -- )  BLT-CORPUS 4        2 10 4    BLT-SEED BL-LOAD ;   \ corpus 4 < T+1=11
: BLT-TRY-ZEROT ( -- )  BLT-CORPUS BLT-CLEN 2 0  4    BLT-SEED BL-LOAD ;   \ T=0 is not a window

T-RESET
BLT-FILL-CORPUS

\ ---- window extraction + B-outermost layout + one-step target shift ----------
BLT-LOAD
BLT-WINDOWS-OK? TTRUE

\ ---- determinism under a fixed seed ------------------------------------------
BLT-DETERMINISTIC? TTRUE

\ ---- emitted segment attribute + loaded-shape accessors -----------------------
BLT-LOAD
true  BL-SEG-ATTR SEG-T@      BLT-T T=       \ block width round-trips through the codec
true  BL-SEG-ATTR SEG-CAUSAL@ TTRUE          \ caller's causal flag is carried
false BL-SEG-ATTR SEG-CAUSAL@ TFALSE
BL-T@   BLT-T    T=
BL-ROWS BLT-ROWS T=

\ ---- fail-closed capacity + domain rejects (never truncates) -----------------
' BLT-TRY-ROWS   E-BL-CAP    TTHROWS
' BLT-TRY-ARENA  E-BL-CAP    TTHROWS
' BLT-TRY-SHORT  E-BL-CORPUS TTHROWS
' BLT-TRY-ZEROT  E-BL-CORPUS TTHROWS

T-REPORT

;package
