split-str
\ util.f — shared helpers for the checked core. Load before src/core/checker.f.

\ First source-prefix record. Its dictionary index is resolved
\ by CORE-PREFIX in the running engine: a captured ndict value would describe
\ the build host, whose primitive/helper record count can differ from ours.
\ This identifies the prefix; earlier DATA allocations can still be live.
package CORE-PREFIX
variable IMK-NDICT0
;package

\ >U8, >U16 and >U32 answer the low 8, 16 or 32 bits of an n; >I64 answers the
\ n declared an i64. The raw casts are retired in src/core/int-convert-retire.f,
\ because src/habu/xref.f first defines undefine.
CAST: >U8-RAW ( n -- u8 )
CAST: >U16-RAW ( n -- u16 )
CAST: >U32-RAW ( n -- u32 )

: >U8 ( n -- u8 ) $FF and >U8-RAW ;
: >U16 ( n -- u16 ) $FFFF and >U16-RAW ;
: >U32 ( n -- u32 ) $FFFFFFFF and >U32-RAW ;

CAST: >I64 ( n -- i64 )

\ The cell of a dictionary record (src/habu/layout.f DREC) that holds the
\ wordlist it was published into, 0 for the global one. src/habu/xref.f reads
\ records by it, and src/core/checker.f, which loads before xref.f, maps the
\ record the engine's lookup binds to a scope by it.
5 constant DICT-WORDLIST-SLOT

\ The scope-find flag bit set when two used publics hold the tail.
2 constant SCOPE-FIND-AMBIGUOUS

: CORE-STR= {: a:^ u:n b:^ v:n :}   \ ( ^u8 n ^u8 n -- bool ) byte-wise string equality
   u v <> IF 0 0= 0= EXIT THEN
   u 0 ?do  a i + c@  b i + c@  <> if 0 0= 0= unloop exit then  loop 0 0= ;

\ ASCII case fold + case-insensitive equality (declaration keyword matching).
: CORE-FOLD-C ( n -- n ) {: c:n :}
   c $41 < IF c EXIT THEN
   c $5A > IF c EXIT THEN
   c $20 or ;

: CORE-STR=CI {: a:^ u:n b:^ v:n :}   \ ( ^u8 n ^u8 n -- bool ) folded equality
   u v <> IF 0 0= 0= EXIT THEN
   u 0 ?do  a i + c@ CORE-FOLD-C  b i + c@ CORE-FOLD-C  <> if 0 0= 0= unloop exit then  loop 0 0= ;

variable RDP

: RD32 {: p:^ :} ( ^u8 -- n )
   p c@  p 1 + c@ 8 lshift or  p 2 + c@ 16 lshift or  p 3 + c@ 24 lshift or ;

\ The words that build the engine's error messages: the line a position is on,
\ a number's digits, and the write to the error output.
package ERROR-TEXT

private

20 constant DIGITS-CAP
create DIGITS DIGITS-CAP allot
variable DIGIT-AT

: DIGIT+ ( n -- ) {: c:n :}
   DIGIT-AT @ 1 - DIGIT-AT !
   c DIGITS DIGIT-AT @ + c! ;

public

\ A number's digits without its sign, from the end of DIGITS: taken on the
\ non-positive side, so MIN-N has them too, as / and mod truncate toward zero.
: DIGITS$ ( n -- str )
   DIGITS-CAP DIGIT-AT !
   dup 0 > if negate then
   begin dup 10 mod negate $30 + DIGIT+ 10 / dup 0= until drop
   DIGITS DIGIT-AT @ +  DIGITS-CAP DIGIT-AT @ - ;

\ Descriptor 2; a failed write has nowhere further to be reported.
: TYPE ( ^u8 n -- ) {: a:^ u:n :}
   2 a u write drop ;

;package

package KERNEL-SEARCH
public
\ The first index of [lo, hi) where q answers false, hi when none does: q answers
\ true on a leading run of the range and false after it.
\ The kernel's copy: the engine loads src/lib/sort.f after the kernel.
: PARTITION-POINT ( n n [ n -- bool ] -- n )
   {: q :}
   begin 2dup < while
      {: lo:n hi:n :}
      lo hi + 2 / {: mid:n :}
      mid q execute if mid 1 + hi else lo mid then
   repeat
   drop ;
;package
