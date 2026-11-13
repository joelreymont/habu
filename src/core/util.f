\ util.f — shared helpers for the checked core. Load before src/core/checker.f.

\ First source-prefix record. Its dictionary index is resolved
\ by CORE-PREFIX in the running engine: a captured ndict value would describe
\ the build host, whose primitive/helper record count can differ from ours.
\ This identifies the prefix; earlier DATA allocations can still be live.
variable IMK-NDICT0

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

\ A dictionary record's cell at a slot. No record is NULL-PTR (src/habu/xref.f
\ XREF-NULL), whose cells would be read from the page at address 0, so it is
\ refused by name; a caller that may hold no record tests for it first.
\ src/habu/xref.f XREF-CELL@ and src/core/checker.f CK-REC-WID read through this
\ word, and CHECKER-RESOLVE:CK-REC-NAME$ reads its flags through it before any
\ other cell, so this one guard covers all three.
7231 constant E-XREF-NULL
: DICT-CELL@ ( ptr n n -- n )
   over NULL-PTR = IF E-XREF-NULL throw THEN
   cells + @ ;

\ The scope-find flag bit set when two used publics hold the tail.
2 constant SCOPE-FIND-AMBIGUOUS

: CORE-STR= {: a:ptr u:n b:ptr v:n :}   \ ( ptr u8 n ptr u8 n -- bool ) byte-wise string equality
   u v <> IF 0 0= 0= EXIT THEN
   u 0 ?do  a i + c@  b i + c@  <> if 0 0= 0= unloop exit then  loop 0 0= ;

\ ASCII case fold + case-insensitive equality (declaration keyword matching).
: CORE-FOLD-C ( n -- n ) {: c:n :}
   c $41 < IF c EXIT THEN
   c $5A > IF c EXIT THEN
   c $20 or ;

: CORE-STR=CI {: a:ptr u:n b:ptr v:n :}   \ ( ptr u8 n ptr u8 n -- bool ) folded equality
   u v <> IF 0 0= 0= EXIT THEN
   u 0 ?do  a i + c@ CORE-FOLD-C  b i + c@ CORE-FOLD-C  <> if 0 0= 0= unloop exit then  loop 0 0= ;

variable RDP

: RD32 {: p:ptr :} ( ptr u8 -- n )
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

: LINE ( ptr u8 ptr u8 -- n )
   {: b:ptr p:ptr :}
   1 p b - 0 ?do b i + c@ 10 = if 1 + then loop ;

\ A number's digits without its sign, from the end of DIGITS: taken on the
\ non-positive side, so MIN-N has them too, as / and mod truncate toward zero.
: DIGITS$ ( n -- ptr u8 n )
   DIGITS-CAP DIGIT-AT !
   dup 0 > if negate then
   begin dup 10 mod negate $30 + DIGIT+ 10 / dup 0= until drop
   DIGITS DIGIT-AT @ +  DIGITS-CAP DIGIT-AT @ - ;

\ Descriptor 2; a failed write has nowhere further to be reported.
: TYPE ( ptr u8 n -- ) {: a:ptr u:n :}
   2 a u write drop ;

: PREFIX ( ptr u8 n ptr u8 n n -- )
   {: c:ptr cu:n p:ptr pu:n ln:n :}
   c cu TYPE  s"  habu: " TYPE
   pu 0= if exit then
   p pu TYPE
   ln 0 > if s" :" TYPE  ln DIGITS$ TYPE then
   s" : " TYPE ;
;package

\ A loader refusal's exit status, always after a line naming the refusal: an
\ uncaught throw of a code below 256 ends the process with no word at all.
$4A constant INCLUDE-IO-RC

\ `[if]` and `[then]` choose text at a file's top level, as a target's files
\ are chosen: `HB-TARGET-MACOS? [if] require src/lib/power/macos.f [then]`. A
\ false answer skips the tokens through the matching `[then]`, counting the
\ `[if]`s nested there, those in comments and strings too. Immediate, as
\ `require` is, so a body cannot hold either. Defined here so that the checker,
\ which loads before the source loader, can choose its own text.
: [if] ( bool -- )
   if exit then
   1 begin dup 0 > while
      parse-name dup 0= if
         S\" hb: source ended inside [if]\n" ERROR-TEXT:TYPE INCLUDE-IO-RC throw
      then
      2dup s" [if]" CORE-STR=CI if 2drop 1 + else
         s" [then]" CORE-STR=CI if 1 - then
      then
   repeat drop ;
immediate

: [then] ( -- ) ;
immediate
