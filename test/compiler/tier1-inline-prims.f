\ tier1-inline-prims.f - tier 1 LOWERS the trivial engine primitives instead of
\ calling them.
\
\ `ptr-field`, `cell+`, `cell-view`, `byte-view`, `tuck`, `max` and `mod` were
\ compiled as calls into the engine's own primitive bodies, and 22 per cent of
\ the self-build's samples were inside bodies a BL had entered (LESSONS.md, the
\ 2026-09-16 profile). src/compiler/native/hir-word.f now carries a row for each
\ of them, so this file pins the property those rows exist for: a tier-1 word
\ that uses one of these ops contains no call into that primitive's body.
\
\ WHERE THE TARGET ADDRESSES COME FROM. Each primitive's span is read from the
\ DICTIONARY RECORD its `FPRIM-L` registration made (src/habu/habu1.f), through
\ src/habu/xref.f. That is the same record the compiler resolves a callable
\ against, so the address a call WOULD go to and the address compared here are
\ one fact and not two that can drift apart. `ENGINE-PRIMS` is the build-side
\ table and is empty in a running engine, which is why it is not read here.
\
\ WHY THERE ARE POSITIVE CONTROLS. "No BL into these spans" is worth nothing if
\ the decoder cannot see a call at all, so the same decoder is asked two
\ questions it must answer YES to: an ordinary call between two definitions of
\ this file is found, and `!` is still found calling the engine's guarded store.
\ The second is not only a control - `!` keeps its call BY DESIGN, because the
\ primitive's body performs the PROT-GUARD write-span check and inlining the
\ store alone would drop it.
\
\ Values are asserted as well as shapes: an op lowered to the wrong arithmetic
\ would pass a call census and fail here. The two byte-order rows read a cell
\ written by this file back through the other view, which is a little-endian
\ fact and the only target this compiler emits for.

require lib/test.f
require src/habu/xref.f

1 set-tier

package TIER1-INLINE-PRIMS
private

\ ---- reading a word's own emitted code ---------------------------------------
\ XREF-N>U8 is src/habu/xref.f's own boundary between an engine address and a
\ readable pointer; this file adds none of its own.
: CODE@ ( n -- n ) {: at:n :}
   at XREF-N>U8 c@
   at 1+ XREF-N>U8 c@ 8 lshift or
   at 2 + XREF-N>U8 c@ 16 lshift or
   at 3 + XREF-N>U8 c@ 24 lshift or ;

\ AArch64 BL has top six bits $25 and a signed 26-bit instruction displacement.
: BL? ( n -- bool ) CODE@ 26 rshift $25 = ;

: BL-TARGET ( n -- n ) {: at:n :}
   at CODE@ $3FFFFFF and {: d:n :}
   d $2000000 >= if d $4000000 - else d then
   2 lshift at + ;

\ ---- the spans ---------------------------------------------------------------
variable MISSING                     \ names the dictionary did not answer for

: SPAN ( ptr u8 n -- n n ) {: na:ptr nu:n :}
   na nu XREF-FIND {: rec:ptr :}
   rec XREF-FOUND? 0= if
      1 MISSING +!
      s" tier1-inline-prims: no record for " type na nu type cr
      0 0 exit
   then
   rec XREF-START dup rec XREF-CODE-BYTES + ;

: SPAN-BYTES ( ptr u8 n -- n )
   SPAN {: lo:n hi:n :} hi lo - ;

8 constant PRIM-MAX
create PRIM-LO PRIM-MAX cells allot
create PRIM-HI PRIM-MAX cells allot
variable PRIM-N

: PRIM+ ( ptr u8 n -- ) {: na:ptr nu:n :}
   PRIM-N @ PRIM-MAX >= if E-CIE-ROW throw then
   na nu SPAN {: lo:n hi:n :}
   lo PRIM-N @ cells PRIM-LO + !
   hi PRIM-N @ cells PRIM-HI + !
   PRIM-N @ 1+ PRIM-N ! ;

\ Whether any recorded primitive body holds this address. A zero-length span
\ matches nothing, so a name the dictionary could not answer for cannot make a
\ census read clean; TEST-SETUP refuses that case outright.
: IN-PRIM? ( n -- bool ) {: at:n :}
   false
   PRIM-N @ 0 ?do
      at i cells PRIM-LO + @ >=
      at i cells PRIM-HI + @ < and if drop true leave then
   loop ;

\ How many calls the named word makes into ANY recorded primitive body.
: CALLS-PRIMS ( ptr u8 n -- n ) {: na:ptr nu:n :}
   na nu SPAN {: lo:n hi:n :}
   0
   hi lo ?do
      i BL? if i BL-TARGET IN-PRIM? if 1+ then then
   4 +loop ;

\ How many calls the named word makes into one other named word.
: CALLS-WORD ( ptr u8 n ptr u8 n -- n ) {: na:ptr nu:n ta:ptr tu:n :}
   ta tu SPAN {: tlo:n thi:n :}
   na nu SPAN {: lo:n hi:n :}
   0
   hi lo ?do
      i BL? if
         i BL-TARGET {: t:n :}
         t tlo >= t thi < and if 1+ then
      then
   4 +loop ;

\ ---- the subjects ------------------------------------------------------------
\ Real checked definitions, each doing work whose answer is asserted below.
create TIP-CELLS 8 cells allot       \ plain cells
create TIP-TAB-STORE 8 cells allot   \ the block the pointer table lives in
TYPED-VARIABLE TIP-TAB ptr n         \ and the declared cell that names its base
TIP-TAB-STORE TIP-TAB !

public

\ `ptr-field` indexes the pointer table; the pointer it holds is then read. It
\ indexes a base whose pointee is DECLARED, which is why the table is reached
\ through TIP-TAB rather than through a raw cell named directly.
: TIP-INDEXED ( n -- n ) {: k:n :}
   TIP-TAB @ k ptr-field @ @ ;

: TIP-INDEXED! ( ptr n n -- ) {: p:ptr k:n :}
   p TIP-TAB @ k ptr-field ! ;

\ `cell+` steps one cell past the cell an index names.
: TIP-NEXT-CELL ( n -- n ) {: k:n :}
   TIP-CELLS k cells + cell+ @ ;

\ `cell-view` and `byte-view` change the pointee and nothing else, so each is
\ paired with the same access written without the view.
: TIP-CELL-VIEW ( ptr u8 -- n ) cell-view @ ;
: TIP-PLAIN-CELL ( ptr n -- n ) @ ;
: TIP-BYTE-VIEW ( ptr n -- n ) byte-view c@ ;
: TIP-PLAIN-BYTE ( ptr u8 -- n ) c@ ;

: TIP-TUCK ( n n -- n n n ) tuck ;
: TIP-MAX ( n n -- n ) max ;
: TIP-MOD ( n n -- n ) mod ;

\ The guarded store, which keeps its call: see the header.
: TIP-STORE ( n ptr n -- ) ! ;

\ The decoder's positive control: an ordinary call, not in tail position.
: TIP-CALLEE ( n -- n ) 1 lshift ;
: TIP-CALLER ( n -- n ) TIP-CALLEE 1+ ;

private

: RECORD-PRIMS ( -- )
   0 PRIM-N !
   s" ptr-field" PRIM+
   s" cell+" PRIM+
   s" cell-view" PRIM+
   s" byte-view" PRIM+
   s" tuck" PRIM+
   s" max" PRIM+
   s" mod" PRIM+ ;

: NO-PRIM-CALLS ( ptr u8 n -- ) {: na:ptr nu:n :}
   na nu SPAN-BYTES 0 > TTRUE
   na nu CALLS-PRIMS 0 T= ;

: TEST-SETUP ( -- )
   s" every primitive this suite names is in the dictionary" T-LABEL
   RECORD-PRIMS
   MISSING @ 0 T=
   PRIM-N @ 7 T=
   PRIM-N @ 0 ?do
      i cells PRIM-HI + @  i cells PRIM-LO + @  > TTRUE
   loop ;

: TEST-DECODER ( -- )
   s" the call decoder finds an ordinary call between two definitions" T-LABEL
   s" TIER1-INLINE-PRIMS:TIP-CALLER" s" TIER1-INLINE-PRIMS:TIP-CALLEE"
   CALLS-WORD 1 T=
   s" a store still calls the engine's guarded store primitive" T-LABEL
   s" TIER1-INLINE-PRIMS:TIP-STORE" s" !" CALLS-WORD 1 T= ;

: TEST-NO-CALLS ( -- )
   s" no tier-1 subject calls one of the lowered primitives" T-LABEL
   s" TIER1-INLINE-PRIMS:TIP-INDEXED" NO-PRIM-CALLS
   s" TIER1-INLINE-PRIMS:TIP-INDEXED!" NO-PRIM-CALLS
   s" TIER1-INLINE-PRIMS:TIP-NEXT-CELL" NO-PRIM-CALLS
   s" TIER1-INLINE-PRIMS:TIP-CELL-VIEW" NO-PRIM-CALLS
   s" TIER1-INLINE-PRIMS:TIP-BYTE-VIEW" NO-PRIM-CALLS
   s" TIER1-INLINE-PRIMS:TIP-TUCK" NO-PRIM-CALLS
   s" TIER1-INLINE-PRIMS:TIP-MAX" NO-PRIM-CALLS
   s" TIER1-INLINE-PRIMS:TIP-MOD" NO-PRIM-CALLS ;

\ An identity view is a rename, so it stages nothing: a word that takes one is
\ the same instructions as the same access written without it.
: TEST-VIEWS-EMIT-NOTHING ( -- )
   s" an identity view costs no instruction" T-LABEL
   s" TIER1-INLINE-PRIMS:TIP-CELL-VIEW" SPAN-BYTES
   s" TIER1-INLINE-PRIMS:TIP-PLAIN-CELL" SPAN-BYTES T=
   s" TIER1-INLINE-PRIMS:TIP-BYTE-VIEW" SPAN-BYTES
   s" TIER1-INLINE-PRIMS:TIP-PLAIN-BYTE" SPAN-BYTES T= ;

: TEST-ADDRESS-VALUES ( -- )
   s" the lowered address arithmetic answers what the primitives answered" T-LABEL
   $55AA TIP-CELLS 3 cells + !
   2 TIP-NEXT-CELL $55AA T=
   TIP-CELLS 3 cells + 0 TIP-INDEXED!
   0 TIP-INDEXED $55AA T=
   TIP-CELLS 3 cells + 7 TIP-INDEXED!
   7 TIP-INDEXED $55AA T=
   $1234 TIP-CELLS !
   TIP-CELLS byte-view TIP-CELL-VIEW $1234 T=
   TIP-CELLS TIP-BYTE-VIEW $34 T= ;

: TEST-ARITHMETIC-VALUES ( -- )
   s" the lowered stack and arithmetic words answer what they answered" T-LABEL
   3 4 TIP-TUCK {: x:n y:n z:n :}
   x 4 T=  y 3 T=  z 4 T=
   3 4 TIP-MAX 4 T=
   4 3 TIP-MAX 4 T=
   -5 -9 TIP-MAX -5 T=
   -9 -5 TIP-MAX -5 T=
   7 7 TIP-MAX 7 T=
   17 5 TIP-MOD 2 T=
   -17 5 TIP-MOD -2 T=
   17 -5 TIP-MOD 2 T=
   4 4 TIP-MOD 0 T=
   3 5 TIP-MOD 3 T= ;

public

: RUN ( -- )
   T-RESET
   0 MISSING !
   TEST-SETUP
   TEST-DECODER
   TEST-NO-CALLS
   TEST-VIEWS-EMIT-NOTHING
   TEST-ADDRESS-VALUES
   TEST-ARITHMETIC-VALUES
   T-REPORT ;

;package

TIER1-INLINE-PRIMS:RUN
