\ tier1-inline-prims.f - tier 1 LOWERS the trivial engine primitives instead of
\ calling them.
\
\ `ptr-field`, `cell+`, `cell-view`, `byte-view`, `tuck`, `max` and `mod` were
\ compiled as calls into the engine's own primitive bodies, and 22 per cent of
\ the self-build's samples were inside bodies a BL had entered.
\ src/compiler/native/hir-word.f now carries a row for each
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
\ this file is found, and an eligible checked store calls the sealed span guard.
\
\ Values are asserted as well as shapes: an op lowered to the wrong arithmetic
\ would pass a call census and fail here. The two byte-order rows read a cell
\ written by this file back through the other view, which is a little-endian
\ fact and the only target this compiler emits for.

require src/lib/test.f
require src/lib/fs.f
require src/habu/xref.f
require src/habu/code-bytes.f
require src/compiler/native/dict.f
require src/arch/x86-64/disasm.f

1 set-tier

package TIER1-INLINE-PRIMS
private

\ ---- reading a word's own emitted code ---------------------------------------
\ CODE-BYTES:AT is the bounded view between an engine code address and a
\ readable pointer; this file adds none of its own.
: CODE@ ( n -- n )
   4 CODE-BYTES:AT drop {: p:ptr :}
   p c@
   p 1+ c@ 8 lshift or
   p 2 + c@ 16 lshift or
   p 3 + c@ 24 lshift or ;

\ AArch64 BL has top six bits $25 and a signed 26-bit instruction displacement.
: BL? ( n -- bool ) CODE@ 26 rshift $25 = ;

: BL-TARGET ( n -- n ) {: at:n :}
   at CODE@ $3FFFFFF and {: d:n :}
   d $2000000 >= if d $4000000 - else d then
   2 lshift at + ;

\ Decode complete x86 instructions: an E8 byte inside an immediate is not a
\ call, and a direct call's displacement is relative to its instruction end.
: X-STEP ( n n -- n n n ) {: at:n hi:n :}
   at hi at - CODE-BYTES:AT at X64DIS:STEP
   {: bytes:n kind:n target:n :}
   at bytes + kind target ;

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
   HB-TARGET-LINUX-X86-64? if
      lo 0
      begin over hi < while
         swap hi X-STEP {: next:n kind:n target:n :}
         kind X64DIS:FLOW-CALL = target IN-PRIM? and if 1+ then
         next swap
      repeat
      nip exit
   then
   0
   hi lo ?do
      i BL? if i BL-TARGET IN-PRIM? if 1+ then then
   4 +loop ;

\ How many calls the named word makes into one other named word.
: CALLS-WORD ( ptr u8 n ptr u8 n -- n ) {: na:ptr nu:n ta:ptr tu:n :}
   ta tu SPAN {: tlo:n thi:n :}
   na nu SPAN {: lo:n hi:n :}
   HB-TARGET-LINUX-X86-64? if
      lo 0
      begin over hi < while
         swap hi X-STEP {: next:n kind:n target:n :}
         kind X64DIS:FLOW-CALL =
            target tlo >= target thi < and and if 1+ then
         next swap
      repeat
      nip exit
   then
   0
   hi lo ?do
      i BL? if
         i BL-TARGET {: t:n :}
         t tlo >= t thi < and if 1+ then
      then
   4 +loop ;

: CALLS-ENTRY ( ptr u8 n n -- n ) {: na:ptr nu:n target:n :}
   na nu SPAN {: lo:n hi:n :}
   HB-TARGET-LINUX-X86-64? if
      lo 0
      begin over hi < while
         swap hi X-STEP {: next:n kind:n at:n :}
         kind X64DIS:FLOW-CALL = at target = and if 1+ then
         next swap
      repeat
      nip exit
   then
   0
   hi lo ?do
      i BL? if i BL-TARGET target = if 1+ then then
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

\ A guarded store whose span check is folded into its caller.
: TIP-STORE ( n ptr n -- n ) ! 7 ;
: TIP-BSTORE ( n ptr u8 -- n ) c! 7 ;

\ Division fixes its results in the registers the span guard overwrites.
: TIP-DIV-STORE ( n n ptr n -- n )
   {: a:n b:n p:ptr :}
   a b / p ! 7 ;


: TIP-MOD-STORE ( n n ptr n -- n )
   {: a:n b:n p:ptr :}
   a b mod p ! 7 ;


: TIP-DIV-BSTORE ( n n ptr u8 -- n )
   {: a:n b:n p:ptr :}
   a b / p c! 7 ;

\ Keep the store address live over a real call before the guarded write.
: TIP-CLOSE-CELL ( ptr n -- )
   {: p:ptr :}
   p @ close -1 p ! ;

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
   s" an eligible store calls the sealed span guard" T-LABEL
   s" (PROT-SPAN)" NDICT:HELPER-TARGET {: guard:n :}
   guard 0 > TTRUE
   s" TIER1-INLINE-PRIMS:TIP-STORE" guard CALLS-ENTRY 1 T=
   s" TIER1-INLINE-PRIMS:TIP-STORE" s" !" CALLS-WORD 0 T=
   s" TIER1-INLINE-PRIMS:TIP-CLOSE-CELL" guard CALLS-ENTRY 1 T=
   s" TIER1-INLINE-PRIMS:TIP-CLOSE-CELL" s" close" CALLS-WORD 1 T=
   s" TIER1-INLINE-PRIMS:TIP-CLOSE-CELL" s" !" CALLS-WORD 0 T=
   s" a byte store calls the sealed span guard" T-LABEL
   s" TIER1-INLINE-PRIMS:TIP-BSTORE" guard CALLS-ENTRY 1 T=
   s" TIER1-INLINE-PRIMS:TIP-BSTORE" s" c!" CALLS-WORD 0 T= ;

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
   TIP-CELLS TIP-BYTE-VIEW $34 T=
   77 TIP-CELLS TIP-STORE 7 T=
   TIP-CELLS @ 77 T=
   91 TIP-CELLS byte-view TIP-BSTORE 7 T=
   TIP-CELLS byte-view c@ 91 T=
   100 3 TIP-CELLS TIP-DIV-STORE 7 T=
   TIP-CELLS @ 33 T=
   100 7 TIP-CELLS TIP-MOD-STORE 7 T=
   TIP-CELLS @ 2 T=
   100 3 TIP-CELLS byte-view TIP-DIV-BSTORE 7 T=
   TIP-CELLS byte-view c@ 33 T= ;

: TEST-STORE-AFTER-CALL ( -- )
   s" a guarded store keeps its address through a call" T-LABEL
   s" /dev/null" FS-PATHZ open-rd {: fd:n :}
   fd 0 >= TTRUE
   fd TIP-CELLS !
   TIP-CELLS byte-view cell-view TIP-CLOSE-CELL
   TIP-CELLS @ -1 T=
   fd close-rc -1 T= ;

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
   TEST-STORE-AFTER-CALL
   TEST-ARITHMETIC-VALUES
   T-REPORT ;

;package

TIER1-INLINE-PRIMS:RUN
