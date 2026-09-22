\ select-x64.f - instruction selection for x86-64: read a frozen HIR module and
\ build the frozen X64IR module its operations select to. It is to
\ src/compiler/native/x64ir.f what src/compiler/native/select.f is to a64ir.f,
\ and it is a SEPARATE pass and not a parameterisation of that one, for the
\ reason docs/x86-64.md gives for the emitter: what the two passes share is the
\ walk, and what they do not share is every rule in it.
\
\ WHAT THIS PASS LOWERS, AND WHAT IT LEAVES TO THE NEXT SLICE. The straight-line
\ and branching core: literals, the two-address arithmetic and its immediate
\ forms, the shifts whose count is a literal, the addressed loads and stores,
\ the comparisons, the two branches, the data-stack boundary of a routine, a
\ call, a tail call, the frame and the return. It is CORRECT AND UNFUSED: a
\ comparison that feeds a branch selects to `x64.cmpset` and then `x64.brz`,
\ never to `x64.cmpbr`, and no selection becomes a `x64.cmpsel`. Fusing and
\ if-conversion are a rewrite of what this pass builds and not a condition of
\ its correctness, so they are the next slice; the ARM64 pass does them inline
\ only because it grew that way.
\
\ THE THREE THINGS THIS MACHINE MAKES THE SELECTOR DO DIFFERENTLY.
\
\ ONE. IT INSERTS THE COPY EVERY TWO-ADDRESS FORM NEEDS. `add rd, rs` destroys
\ rd, and the dialect says so with a schema TIE. The allocator READS that tie as
\ a must-share constraint and REFUSES a pair whose ends cannot share a register
\ (regalloc.f MB-TIE1 throws E-A64RA-TIE); it inserts nothing. Making every tie
\ satisfiable is therefore this pass's work: operand 0 is copied into a fresh
\ value with `x64.mov` first whenever it is still LIVE AFTER the operation. That
\ is a question about the CONTROL FLOW GRAPH and not about how many operands
\ name the value: a value defined above a loop and read ONCE inside it is read
\ again by the next pass through the header, and no operand of the backedge
\ names it. So this pass computes live-in and live-out over the function's
\ blocks before it walks them ("which values are live where" below) and the tie
\ decision reads them. The allocator coalesces the copies it can (MB-COPY?), so
\ a copy that was not needed costs nothing after allocation.
\
\ TWO. A LITERAL IN THE SECOND OPERAND IS PART OF THE INSTRUCTION. x86-64 ALU
\ and compare forms carry a signed imm32, so `8 +` is one instruction and not
\ two. The literal's own `x64.movi` is left out only when EVERY use of it is one
\ of these, which is counted over the whole function and not guessed at.
\
\ THREE. TWO FORMS NAME A REGISTER, AND THE COPY IS HOW THIS PASS ASKS FOR IT.
\ `shl r64, cl` reads its count from rcx and `idiv r64` divides rdx:rax into rax
\ and rdx. The schema states those obligations as the forms' own fixed operand
\ and result registers (x64ir.f DEF-SHIFT-CL and DEF-IDIV) and the allocator
\ places them from there, so making them SATISFIABLE is this pass's work, as it
\ is for a tie: the count of a variable shift and the dividend of a division are
\ copied into fresh values with `x64.mov` first, and a count or dividend the rest
\ of the function reads - or two counts live over one interval - is repaired by
\ the copy instead of refused. A shift by a literal keeps the immediate form and
\ carries no constraint at all. A division's quotient is result 0 and the source
\ value's; its remainder is a second result nothing reads; `cqo` and the branch
\ over the zero-divisor refusal belong to the form's RENDER, which is why the
\ operation carries the runtime `throw` entry as its own attribute the way
\ a64.sdiv does, and why a target dictionary without `throw` is refused
\ (E-X64SEL-TRAP).
\
\ WHAT THE EMITTER STILL OWES. emit-x64.f renders none of `x64.shl`, `x64.shr`
\ and `x64.idiv` and refuses each by name (E-X64EMIT-FORM), so a variable shift
\ and a division are lowered, placed and validated here but are not yet bytes.
\ The render owes two answers this pass cannot give it: the branch that hands a
\ zero divisor to the entry above, and `MIN-N / -1`, which raises #DE on this
\ machine where Habu's `/` wraps to MIN-N.
\
\ AND ONE THE DIALECT MAKES: THERE IS NO FLOATING FORM AT ALL. x64ir declares no
\ SSE operation, so every floating source operation - including the four
\ conversions - is refused with E-X64SEL-FLOAT instead of being lowered wrongly.
\
\ WHERE THE POINTER STANDS. The data-stack pointer is a register and stands at
\ ONE place for the whole body. This pass does not survey the body for the
\ cheapest place the way the ARM64 one does: it stands at the routine's entry
\ base, which costs one adjustment at each end and none in between, except in a
\ routine that leaves through its callee, where the tail branch carries no
\ adjustment at all and the pointer therefore has to stand where it was entered.
\ A cell already pushed is then at a NEGATIVE displacement, which this machine
\ addresses as readily as a positive one (x64ir.f DSLOT).
\
\ That policy is not this file's private habit: x64ir.f VOCABULARY states it as
\ the dialect's `stand` field (NDIALECT:dstand `entry-base`, src/compiler/
\ native/dialect.f), and regalloc-verify.f VDPLACE-CK checks a module against
\ the stated policy rather than re-deriving the ARM64 survey.

require lib/prelude.f
require lib/errors.f
require src/compiler/ir/id.f
require src/compiler/ir/context.f
require src/compiler/ir/symbol.f
require src/compiler/ir/type.f
require src/compiler/ir/source.f
require src/compiler/ir/schema.f
require src/compiler/ir/fun.f
require src/compiler/ir/build.f
require src/compiler/native-effect.f
require src/compiler/native/machine.f
require src/compiler/native/hir.f
require src/compiler/native/x64ir.f
require src/compiler/native/frozen.f
require src/compiler/native/dict.f
require src/compiler/native/trap.f
require src/arch/x86-64/machine.f

package X64SEL
using NFROZEN
private

\ ---- the scratch this pass retains -------------------------------------------
variable SCRATCH-VALUES
variable SCRATCH-BLOCKS
: VMAX ( -- n ) SCRATCH-VALUES @ ;
: BMAX ( -- n ) SCRATCH-BLOCKS @ ;

: SCRATCH-SIZES! ( -- )
   NFROZEN:VALUE-COUNT 1 max SCRATCH-VALUES !
   NFROZEN:TOTAL-BLOCKS 1 max SCRATCH-BLOCKS ! ;

DYNAMIC-BUFFER VMAP IR-ID:ir-value-id
DYNAMIC-BUFFER VSET-BUF n
: VSET ( -- ptr n ) 0 VSET-BUF ;
DYNAMIC-BUFFER D-ORDER IR-ID:ir-value-id
DYNAMIC-BUFFER D-ORDER-SET-BUF n
: D-ORDER-SET ( -- ptr n ) 0 D-ORDER-SET-BUF ;

\ ---- what the data-stack residency pass answers ------------------------------
\ A slot of the caller's data stack holds a value of the source module, and this
\ is the window that fact is tracked over: past it, never resident. The pass is
\ A64SEL's (select.f, "which slot holds which value"), COPIED and not shared:
\ the ARM64 chain requires select.f alone and this file is its own package with
\ its own buffers, so moving the machinery into one place would move the ARM64
\ selector, which nothing here is allowed to do.
64 constant DSLOT-MAX                \ slots one routine's residency is tracked over
-1 constant DNONE                    \ this slot holds nothing this pass can name
-2 constant DANY                     \ nothing has been said about this slot yet
63 constant DELIDE-MAX               \ store positions one run's elision mask holds

DYNAMIC-BUFFER D-IN-BUF n
: D-IN ( -- ptr n ) 0 D-IN-BUF ;
DYNAMIC-BUFFER D-OUT-BUF n
: D-OUT ( -- ptr n ) 0 D-OUT-BUF ;
DYNAMIC-BUFFER D-NEED-BUF n
: D-NEED ( -- ptr n ) 0 D-NEED-BUF ;
here CELL 1- and CELL swap - CELL 1- and allot
create D-CUR DSLOT-MAX cells allot   \ the running answer inside one block
create D-MEET DSLOT-MAX cells allot  \ the meet of one block's predecessors
variable D-MOVED                     \ a fixpoint round changed something

\ The liveness planes: one bit per value, per block, in each of the four sets
\ the backward dataflow keeps, and one set of the same width for the values a
\ block has already defined while it is being read. A block's set is only as
\ wide as the FUNCTION's own value span (LWORDS), so what each function clears
\ and iterates is its own size; the reservation is the module's worst case,
\ because nothing tells the walk which function is the widest before it meets
\ it.
64 constant SET-BITS
0 constant P-IN
1 constant P-OUT
2 constant P-USE
3 constant P-DEF
4 constant PLANES
: SETC ( -- n ) VMAX SET-BITS 1- + SET-BITS / ;
DYNAMIC-BUFFER LIVE-BUF n
: LIVE-SETS ( -- ptr n ) 0 LIVE-BUF ;
DYNAMIC-BUFFER DEFSET-BUF n
: DEFSET ( -- ptr n ) 0 DEFSET-BUF ;

: RESERVE-SCRATCH ( -- )
   SCRATCH-SIZES!
   VMAX VMAP-RESERVE
   VMAX VSET-BUF-RESERVE
   BMAX D-ORDER-RESERVE
   BMAX D-ORDER-SET-BUF-RESERVE
   BMAX DSLOT-MAX * D-IN-BUF-RESERVE
   BMAX DSLOT-MAX * D-OUT-BUF-RESERVE
   VMAX D-NEED-BUF-RESERVE
   PLANES BMAX * SETC * LIVE-BUF-RESERVE
   SETC DEFSET-BUF-RESERVE ;

\ ---- the bound source dialect ------------------------------------------------
HIR-OPCODE:CONST    HIR:ORD constant O-CONST
HIR-OPCODE:RETURN   HIR:ORD constant O-RETURN
HIR-OPCODE:ADD      HIR:ORD constant O-ADD
HIR-OPCODE:SUB      HIR:ORD constant O-SUB
HIR-OPCODE:AND      HIR:ORD constant O-AND
HIR-OPCODE:OR       HIR:ORD constant O-OR
HIR-OPCODE:XOR      HIR:ORD constant O-XOR
HIR-OPCODE:LSHIFT   HIR:ORD constant O-LSHIFT
HIR-OPCODE:RSHIFT   HIR:ORD constant O-RSHIFT
HIR-OPCODE:LT       HIR:ORD constant O-LT
HIR-OPCODE:LE       HIR:ORD constant O-LE
HIR-OPCODE:GT       HIR:ORD constant O-GT
HIR-OPCODE:GE       HIR:ORD constant O-GE
HIR-OPCODE:EQUAL    HIR:ORD constant O-EQUAL
HIR-OPCODE:NE       HIR:ORD constant O-NE
HIR-OPCODE:BR       HIR:ORD constant O-BR
HIR-OPCODE:BRZ      HIR:ORD constant O-BRZ
HIR-OPCODE:CALL     HIR:ORD constant O-CALL
HIR-OPCODE:WORDCALL HIR:ORD constant O-WORDCALL
HIR-OPCODE:TRAP     HIR:ORD constant O-TRAP

0 constant BOUND-NO
1 constant BOUND-YES

128 constant NAME-CAP
3 constant TRAP-CELLS                \ the address, the length and the exit code
64 constant EDGE-MAX                 \ operands one edge may carry

here CELL 1- and CELL swap - CELL 1- and allot
variable BND-MODE
BOUND-NO BND-MODE !

1 TYPED-BUFFER BND-MOD IR-ID:ir-module-id
HIR:OPCODES TYPED-BUFFER BND-OP IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-VAL IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-ADDR IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-FUN IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-ENTRY IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-IN IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-OUT IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-MEM IR-ID:ir-type-id

\ ---- what one selection run is working on ------------------------------------
1 TYPED-BUFFER S-CTX IR-CTX:ctx
1 TYPED-BUFFER S-BLD IR-BUILD:builder
1 TYPED-BUFFER S-SID IR-ID:ir-source-id
1 TYPED-BUFFER S-ACC IR-ID:ir-value-id
1 TYPED-BUFFER S-TOK IR-ID:ir-value-id
1 TYPED-BUFFER S-ARGS NEFF:placeseq
1 TYPED-BUFFER S-OUTS NEFF:placeseq
1 TYPED-BUFFER S-TRT NEFF:traits
1 TYPED-BUFFER S-FTOK IR-ID:ir-value-id
1 TYPED-BUFFER S-FUN IR-ID:ir-fun-id
1 TYPED-BUFFER S-BLK IR-ID:ir-block-id
EDGE-MAX TYPED-BUFFER EDGE-V IR-ID:ir-value-id
variable S-FRAME                     \ the frame the contract declares, in bytes
variable S-DECL-IN                   \ how many values the contract declares the emission takes
variable S-DECL-OUT                  \ and how many it declares it leaves
variable S-TAIL                      \ whether control leaves through a callee
variable S-DSTACK                    \ whether the data-stack convention is declared
variable N-CALLS
variable N-TAILS
variable TAIL-SITE                   \ the operation this function leaves through, or -1
variable D-POS                       \ where the body's data-stack pointer stands
variable V-BASE                      \ first value owned by the function being selected
variable V-LIMIT                     \ one past its last value
variable R-BASE                      \ where this function's blocks start in the module
variable R-NEWBASE                   \ and where they start in the module being built
variable LIVE-WORDS                  \ cells in one block's value set, this function
variable LIVE-CHANGED                \ whether the last dataflow pass moved a set

here CELL 1- and CELL swap - CELL 1- and allot
create NAMEBUF NAME-CAP allot

\ ---- the slots, read back ----------------------------------------------------
: CTX ( -- IR-CTX:ctx )              0 S-CTX @ ;
: BLD ( -- IR-BUILD:builder )        0 S-BLD @ ;
: SID ( -- IR-ID:ir-source-id )      0 S-SID @ ;
: ACC ( -- IR-ID:ir-value-id )       0 S-ACC @ ;
: ACC! ( IR-ID:ir-value-id -- )      0 S-ACC ! ;
: TOK ( -- IR-ID:ir-value-id )       0 S-TOK @ ;
: TOK! ( IR-ID:ir-value-id -- )      0 S-TOK ! ;
: ARGS ( -- NEFF:placeseq )          0 S-ARGS @ ;
: OUTS ( -- NEFF:placeseq )          0 S-OUTS @ ;
: TRAITS ( -- NEFF:traits )          0 S-TRT @ ;
: TAIL? ( -- bool )                  S-TAIL @ 0<> ;
: FRAME ( -- n )                     S-FRAME @ ;
: FTOK ( -- IR-ID:ir-value-id )      0 S-FTOK @ ;
: FTOK! ( IR-ID:ir-value-id -- )     0 S-FTOK ! ;
: FUN ( -- IR-ID:ir-fun-id )         0 S-FUN @ ;
: BLK ( -- IR-ID:ir-block-id )       0 S-BLK @ ;

\ ---- the source dialect's opcode family --------------------------------------
\ A symbol naming no member is an operation this pass has no rule for.
: OPCODE-SLOT ( IR-ID:ir-symbol-id -- n )
   {: sym:IR-ID:ir-symbol-id :}
   -1
   HIR:OPCODES 0 ?do
      sym i BND-OP @ SAME-SYM? if drop i leave then
   loop
   dup 0 < if E-X64SEL-OPCODE throw then ;

: OP-SLOT ( IR-ID:ir-op-id -- n )
   OPCODE-AT OPCODE-SLOT ;

\ ---- the value map -----------------------------------------------------------
\ Which value of the NEW module a value of the source module selected to.
: V-RANGE1 ( IR-ID:ir-value-id -- )
   IR-ID:VALUE-LOCAL {: k:n :}
   k V-BASE @ min V-BASE !
   k 1+ V-LIMIT @ max V-LIMIT ! ;

: V-RANGE-OP ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id RESULTS-OF 0 ?do id i RESULT-AT V-RANGE1 loop ;

: V-RANGE-BLOCK ( IR-ID:ir-block-id -- )
   {: bk:IR-ID:ir-block-id :}
   bk ARG-COUNT 0 ?do bk i ARG-AT V-RANGE1 loop
   bk OP-COUNT 0 ?do bk i OP-AT V-RANGE-OP loop ;

: V-RANGE! ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   $7FFFFFFF V-BASE !
   -1 V-LIMIT !
   f BLOCK-COUNT 0 ?do f i BLOCK-AT V-RANGE-BLOCK loop
   V-LIMIT @ 0< if 0 V-BASE ! 0 V-LIMIT ! exit then
   V-LIMIT @ V-BASE @ - VMAX > if E-X64SEL-CAP throw then ;

: VCLEAR ( -- )
   VMAX 0 ?do 0 i cells VSET + ! loop ;

: VSLOT ( IR-ID:ir-value-id -- n )
   IR-ID:VALUE-LOCAL V-BASE @ -
   dup 0 < over VMAX >= or if E-X64SEL-CAP throw then ;

: VBIND ( IR-ID:ir-value-id IR-ID:ir-value-id -- )
   {: src:IR-ID:ir-value-id new:IR-ID:ir-value-id :}
   src VSLOT {: k:n :}
   new k VMAP !
   1 k cells VSET + ! ;

: VOF ( IR-ID:ir-value-id -- IR-ID:ir-value-id )
   VSLOT {: k:n :}
   k cells VSET + @ 0= if E-X64SEL-SHAPE throw then
   k VMAP @ ;

\ ---- reading the frozen module -----------------------------------------------
: SRC-CK ( IR-ID:ir-source-id -- )
   IR-ID:SOURCE-LOCAL 0<> if E-X64SEL-SHAPE throw then ;

: OP-SPAN ( IR-ID:ir-op-id -- IR-SOURCE:span )
   {: id:IR-ID:ir-op-id :}
   id SPAN-AT IR--SOURCE-SPAN:UNMAKE
   {: src:IR-ID:ir-source-id st:n ln:n :}
   src SRC-CK
   BLD SID st ln IR-BUILD:ADD-SPAN ;

: FUN-SPAN ( IR-ID:ir-fun-id -- IR-SOURCE:span )
   {: f:IR-ID:ir-fun-id :}
   V-FUNR VW MKEY f IR-FUN:FSPAN@ IR--SOURCE-SPAN:UNMAKE
   {: src:IR-ID:ir-source-id st:n ln:n :}
   src SRC-CK
   BLD SID st ln IR-BUILD:ADD-SPAN ;

: BLOCK-SPAN ( IR-ID:ir-block-id -- IR-SOURCE:span )
   {: bk:IR-ID:ir-block-id :}
   V-BLKR VW MKEY bk IR-FUN:FBLOCK-SPAN@ IR--SOURCE-SPAN:UNMAKE
   {: src:IR-ID:ir-source-id st:n ln:n :}
   src SRC-CK
   BLD SID st ln IR-BUILD:ADD-SPAN ;

: OPERAND ( IR-ID:ir-op-id n -- IR-ID:ir-value-id )
   OPERAND-AT VOF ;

\ The TYPE the source module gives it, held against the identity this pass
\ interned - never a position or an opcode.
: TOKEN? ( IR-ID:ir-value-id -- bool )
   VALUE-TYPE-AT  0 BND-MEM @  SAME-TYPE? ;

\ ---- the residency answers, read back ----------------------------------------
: DSLOT-CK ( n -- n )
   dup 0 < if E-X64SEL-CAP throw then ;

: DIN-WINDOW? ( n -- bool )
   DSLOT-CK DSLOT-MAX < ;

: DRES@ ( n -- n )
   dup DIN-WINDOW? 0= if drop DNONE exit then
   cells D-CUR + @ ;

: DRES! ( IR-ID:ir-value-id n -- )
   {: v:IR-ID:ir-value-id s:n :}
   s DIN-WINDOW? 0= if exit then
   v VSLOT s cells D-CUR + ! ;

\ Whether the cell already held it, which is the whole of what makes a store
\ droppable.
: DPUT? ( IR-ID:ir-value-id n -- bool )
   {: v:IR-ID:ir-value-id s:n :}
   s DRES@  v VSLOT =  {: had:bool :}
   v s DRES!
   had ;

: DKILL ( -- )
   DSLOT-MAX 0 ?do DNONE i cells D-CUR + ! loop ;

\ A memory order holds no register and is never dropped.
: DNEED? ( IR-ID:ir-value-id -- bool )
   dup TOKEN? if drop true exit then
   VSLOT cells D-NEED + @ 0<> ;

: DNEED+ ( IR-ID:ir-value-id -- )
   dup DNEED? if drop exit then
   VSLOT cells D-NEED + 1 swap !
   1 D-MOVED ! ;

\ ---- staging one machine operation -------------------------------------------
\ Every machine operation carries the span of the source operation it selects
\ from, so a diagnostic points at the source and not at the machine.
: OPEN ( IR-ID:ir-op-id X64IR:opcode -- )
   {: id:IR-ID:ir-op-id o:X64IR:opcode :}
   CTX BLD  CTX BLD o X64IR:ENSURE-OP  IR-BUILD:BEGIN-OP
   CTX BLD  id OP-SPAN  IR-BUILD:SET-OP-SPAN ;

: RESULT+ ( -- )
   CTX BLD  CTX BLD X64IR:GPR-TYPE  IR-BUILD:ADD-RESULT ;

: TOKEN+ ( -- )
   CTX BLD  CTX BLD X64IR:MEM-TYPE  IR-BUILD:ADD-RESULT ;

: OPERAND+ ( IR-ID:ir-value-id -- )
   CTX BLD rot IR-BUILD:ADD-OPERAND ;

: CLOSE-VALUE ( -- )
   CTX BLD IR-BUILD:END-OP {: id:IR-ID:ir-op-id :}
   CTX BLD id 0 IR-BUILD:OP-RESULT@ ACC! ;

\ One value moved into a fresh one, carrying the span of the operation the move
\ was made for. Both places that need a value somewhere else are this: the
\ destination an edge carries its argument into, and the operand a two-address
\ form is about to destroy. The opcode is `x64.mov`, which ties nothing and is
\ what an allocator coalesces away when the two ends can share a register.
: EMIT-COPY ( IR-ID:ir-op-id IR-ID:ir-value-id -- IR-ID:ir-value-id )
   {: at:IR-ID:ir-op-id v:IR-ID:ir-value-id :}
   at X64IR-OPCODE:MOV OPEN
   CTX BLD v IR-BUILD:ADD-OPERAND
   RESULT+
   CLOSE-VALUE
   ACC ;

\ ---- the attributes an operation of the source carries ------------------------
: ATTR-SLOT-OF ( IR-ID:ir-op-id IR-ID:ir-symbol-id -- n )
   {: id:IR-ID:ir-op-id want:IR-ID:ir-symbol-id :}
   -1
   id ATTRS-OF 0 ?do
      id i ATTR-KEY-AT want SAME-SYM? if drop i leave then
   loop
   dup 0 < if E-X64SEL-ATTR throw then ;

: ATTR-INT-OF ( IR-ID:ir-op-id IR-ID:ir-symbol-id -- n )
   {: id:IR-ID:ir-op-id want:IR-ID:ir-symbol-id :}
   id  id want ATTR-SLOT-OF  ATTR-INT-AT ;

: CONST-VALUE ( IR-ID:ir-op-id -- n )
   0 BND-VAL @ ATTR-INT-OF ;

: CONST-ADDR ( IR-ID:ir-op-id -- n )
   0 BND-ADDR @ ATTR-INT-OF ;

: WORD-ENTRY ( IR-ID:ir-op-id -- n )
   0 BND-ENTRY @ ATTR-INT-OF ;

\ ---- the routine's convention, read once -------------------------------------
: SLOT-POSITIONS ( NEFF:placeseq -- n )
   {: s:NEFF:placeseq :}
   s NEFF:SEQ-SLOTS {: sl:n :}
   sl 0= if 0 exit then
   sl s NEFF:SEQ-LEN <> if E-X64SEL-PLACE throw then
   sl ;

\ THE CONTRACT SAYS SO and this pass does not work it out.
: DSTACK? ( -- bool )
   S-DSTACK @ 0<> ;

: CALLS? ( -- bool )
   TRAITS NEFF:T-CALL NEFF:TRAITS-HAS? ;

\ A frame is the ALLOCATOR's spill room on this machine and nothing else: the
\ return address is on the machine stack, so a call costs this routine no slot.
: FRAMED? ( -- bool )
   FRAME 0<> ;

\ A call site hands the callee its arguments through the caller's data stack, so
\ a contract that declares a call has to declare that convention.
: CONTRACT-CK ( -- )
   CALLS? 0= if exit then
   DSTACK? 0= if E-X64SEL-CALL throw then ;

\ ---- each function's own boundary --------------------------------------------
\ The CONTRACT describes the published word and the MODULE describes every
\ function, so only function zero is held against the contract.
: FUN-SLOTS ( n -- NEFF:placeseq )
   NEFF:SEQ-DSTACK ;

: DECL-CK ( n n -- )
   {: in:n out:n :}
   in S-DECL-IN @ <> if E-X64SEL-PLACE throw then
   out S-DECL-OUT @ <> if E-X64SEL-PLACE throw then ;

: FUN-PLACES! ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id ord:n :}
   DSTACK? 0= if exit then
   f NFROZEN:FUN-ARITY {: in:n out:n :}
   ord 0= if in out DECL-CK then
   in FUN-SLOTS 0 S-ARGS !
   out FUN-SLOTS 0 S-OUTS ! ;

: CALLED-CK ( -- )
   CALLS? if
      N-CALLS @ 0= if E-X64SEL-CALL throw then exit
   then
   N-CALLS @ 0<> if E-X64SEL-CALL throw then ;

: TAILED-CK ( -- )
   TAIL? if
      N-TAILS @ 1 <> if E-X64SEL-TAIL throw then exit
   then
   N-TAILS @ 0<> if E-X64SEL-TAIL throw then ;

\ ---- the four data-stack operations ------------------------------------------
\ Every offset is written in the place the body's pointer stands, which is where
\ the machine addresses it from.
: DPLACED ( n -- n )
   D-POS @ - ;

: DSLOT-ATTR+ ( n -- )
   {: off:n :}
   CTX BLD  CTX BLD X64IR:KEY-DSLOT  CTX BLD off DPLACED X64IR:DSLOT-ATTR
   IR-BUILD:ADD-ATTR ;

\ The mover's own number, already placed by the caller: a move is a distance and
\ not an address.
: DBYTES-AT+ ( n -- )
   {: d:n :}
   CTX BLD  CTX BLD X64IR:KEY-DBYTES  CTX BLD d X64IR:DBYTES-ATTR
   IR-BUILD:ADD-ATTR ;

: DBACK-AT+ ( n -- )
   {: d:n :}
   CTX BLD  CTX BLD X64IR:KEY-DBACK  CTX BLD d X64IR:DBACK-ATTR
   IR-BUILD:ADD-ATTR ;

\ Where the routine's data-stack order is minted. It moves the pointer down over
\ the arguments the caller left, except in a routine that leaves through its
\ callee, where it moves nothing and only mints the order.
: EMIT-DTAKE ( IR-ID:ir-op-id n -- )
   {: at:IR-ID:ir-op-id d:n :}
   at X64IR-OPCODE:DTAKE OPEN
   TOKEN+
   d DBYTES-AT+
   CTX BLD IR-BUILD:END-OP {: id:IR-ID:ir-op-id :}
   CTX BLD id 0 IR-BUILD:OP-RESULT@ TOK! ;

: EMIT-DLOAD ( IR-ID:ir-op-id n -- IR-ID:ir-value-id )
   {: at:IR-ID:ir-op-id off:n :}
   at X64IR-OPCODE:DLOAD OPEN
   TOK OPERAND+
   RESULT+
   TOKEN+
   off DSLOT-ATTR+
   CTX BLD IR-BUILD:END-OP {: id:IR-ID:ir-op-id :}
   CTX BLD id 1 IR-BUILD:OP-RESULT@ TOK!
   CTX BLD id 0 IR-BUILD:OP-RESULT@ ;

: EMIT-DSTORE ( IR-ID:ir-op-id IR-ID:ir-value-id n -- )
   {: at:IR-ID:ir-op-id v:IR-ID:ir-value-id off:n :}
   at X64IR-OPCODE:DSTORE OPEN
   v OPERAND+
   TOK OPERAND+
   TOKEN+
   off DSLOT-ATTR+
   CTX BLD IR-BUILD:END-OP {: id:IR-ID:ir-op-id :}
   CTX BLD id 0 IR-BUILD:OP-RESULT@ TOK! ;

\ The pointer is left one past the results, which is the moment they become the
\ caller's. x86-64 has no write-back addressing, so this is its own instruction
\ where ARM64 rides it on the last store.
: EMIT-DPUBLISH ( IR-ID:ir-op-id n -- )
   {: at:IR-ID:ir-op-id d:n :}
   at X64IR-OPCODE:DPUBLISH OPEN
   TOK OPERAND+
   d DBYTES-AT+
   CTX BLD IR-BUILD:END-OP drop ;

\ ---- the routine's frame -----------------------------------------------------
\ There is no link save and no link load: `call` pushed the return address on
\ the machine stack and `ret` pops it, so the prologue is the reserve alone.
: FRAME-ATTR+ ( n -- )
   {: size:n :}
   CTX BLD  CTX BLD X64IR:KEY-FRAME  CTX BLD size X64IR:FRAME-ATTR
   IR-BUILD:ADD-ATTR ;

: EMIT-RESERVE ( IR-ID:ir-op-id -- )
   {: at:IR-ID:ir-op-id :}
   at X64IR-OPCODE:RESERVE OPEN
   TOKEN+
   FRAME FRAME-ATTR+
   CTX BLD IR-BUILD:END-OP {: id:IR-ID:ir-op-id :}
   CTX BLD id 0 IR-BUILD:OP-RESULT@ FTOK! ;

: EMIT-RELEASE ( IR-ID:ir-op-id -- )
   {: at:IR-ID:ir-op-id :}
   at X64IR-OPCODE:RELEASE OPEN
   FTOK OPERAND+
   FRAME FRAME-ATTR+
   CTX BLD IR-BUILD:END-OP drop ;

: PROLOGUE ( IR-ID:ir-op-id -- )
   {: at:IR-ID:ir-op-id :}
   FRAMED? 0= if exit then
   at EMIT-RESERVE ;

: EPILOGUE ( IR-ID:ir-op-id -- )
   {: at:IR-ID:ir-op-id :}
   FRAMED? 0= if exit then
   at EMIT-RELEASE ;

\ ---- which operation this function leaves through ----------------------------
: TAIL-OP? ( IR-ID:ir-op-id -- bool )
   {: id:IR-ID:ir-op-id :}
   TAIL-SITE @ 0 < if false exit then
   id IR-ID:OP-LOCAL TAIL-SITE @ = ;

: TAIL-HERE? ( -- bool )
   TAIL-SITE @ 0 < if false exit then
   BLK OP-COUNT {: n:n :}
   n 2 < if false exit then
   BLK n 2 - OP-AT TAIL-OP? ;

\ ---- the shape of a call site ------------------------------------------------
\ How many values are live ACROSS the call, held against both lists: the operands
\ are the order, the live values and then the arguments, and the results are the
\ order, the same live values and then the answers.
: CALL-LIVE ( IR-ID:ir-op-id n n -- n )
   {: id:IR-ID:ir-op-id a:n r:n :}
   id OPERANDS-OF 1- a - {: k:n :}
   k 0 < if E-X64SEL-CALL throw then
   id RESULTS-OF 1- r - k <> if E-X64SEL-CALL throw then
   k ;

\ A self-call is RECURSE, which names the DEFINITION and not the body the token
\ stands in, so its shape is the CONTRACT's declaration.
: SELF-SHAPE ( IR-ID:ir-op-id -- n n n n )
   {: id:IR-ID:ir-op-id :}
   S-DECL-IN @ {: a:n :}
   S-DECL-OUT @ {: r:n :}
   id a r CALL-LIVE {: k:n :}
   a r k 0 ;

: WORD-SHAPE ( IR-ID:ir-op-id -- n n n n )
   {: id:IR-ID:ir-op-id :}
   id 0 BND-IN @ ATTR-INT-OF {: a:n :}
   id 0 BND-OUT @ ATTR-INT-OF {: r:n :}
   id a r CALL-LIVE {: k:n :}
   a r k 0 ;

\ The shape of the SITE is not the shape of the operation at the one operation a
\ routine leaves through: nothing is live across a branch it never returns from.
: SITE-SHAPE ( IR-ID:ir-op-id -- n n n n )
   {: id:IR-ID:ir-op-id :}
   id WORD-SHAPE {: a:n r:n kk:n m:n :}
   id TAIL-OP? 0= if a r kk m exit then
   a r 0  kk m + ;

\ Which value goes into which cell is written down once and three readers stand
\ on it: the live values first, then the arguments.
: DSAVE-VAL ( IR-ID:ir-op-id n n n -- IR-ID:ir-value-id )
   {: id:IR-ID:ir-op-id kk:n m:n i:n :}
   i kk < if id i 1+ OPERAND-AT exit then
   id m i + 1+ OPERAND-AT ;

: DBACK-VAL ( IR-ID:ir-op-id n n n -- IR-ID:ir-value-id )
   {: id:IR-ID:ir-op-id kk:n m:n i:n :}
   i kk < if id i 1+ RESULT-AT exit then
   id m i + 1+ RESULT-AT ;

\ ---- the residency of one call site ------------------------------------------
\ One bit per position of the store run, set where the cell already held it.
: DBIT? ( n n -- bool )
   {: mask:n i:n :}
   i DELIDE-MAX >= if false exit then
   mask 1 i lshift and 0<> ;

\ ---- selecting a call --------------------------------------------------------
: EMIT-CALL-OP ( IR-ID:ir-op-id n n -- )
   {: at:IR-ID:ir-op-id give:n back:n :}
   at X64IR-OPCODE:CALL OPEN
   TOK OPERAND+
   TOKEN+
   give DBYTES-AT+
   back DBACK-AT+
   CTX BLD IR-BUILD:END-OP {: id:IR-ID:ir-op-id :}
   CTX BLD id 0 IR-BUILD:OP-RESULT@ TOK! ;

: EMIT-WORDCALL-OP ( IR-ID:ir-op-id n n n -- )
   {: at:IR-ID:ir-op-id give:n back:n entry:n :}
   at X64IR-OPCODE:WORDCALL OPEN
   TOK OPERAND+
   TOKEN+
   give DBYTES-AT+
   back DBACK-AT+
   CTX BLD  CTX BLD X64IR:KEY-ENTRY  CTX BLD entry X64IR:ENTRY-ATTR
   IR-BUILD:ADD-ATTR
   CTX BLD IR-BUILD:END-OP {: id:IR-ID:ir-op-id :}
   CTX BLD id 0 IR-BUILD:OP-RESULT@ TOK! ;

\ Every live value and every argument is written to its cell, EXCEPT the ones the
\ cell already holds. The mask is the residency pass's answer for this site, one
\ bit per position of the store run (DSAVE-XFER below, handed over by RULE), and
\ a store of a value its cell still holds is not an optimisation to leave out: it
\ is what regalloc-verify.f VDSTORE-CK refuses by name (E-A64RAV-DKEEP).
: CALL-SAVE ( IR-ID:ir-op-id n n n n -- )
   {: id:IR-ID:ir-op-id kk:n m:n a:n mask:n :}
   kk a + {: n:n :}
   n 0 ?do
      mask i DBIT? 0= if
         id  id kk m i DSAVE-VAL VOF  i X64IR:SLOT-WIDTH *  EMIT-DSTORE
      then
   loop ;

\ And only the values something reads out of a register are taken back: a load
\ whose result has no use is the other half of the same refusal.
: CALL-RESTORE ( IR-ID:ir-op-id n n n -- )
   {: id:IR-ID:ir-op-id kk:n m:n r:n :}
   kk r + {: n:n :}
   n 0 ?do
      id kk m i DBACK-VAL {: v:IR-ID:ir-value-id :}
      v DNEED? if
         v  id  i X64IR:SLOT-WIDTH *  EMIT-DLOAD  VBIND
      then
   loop
   id 0 RESULT-AT  TOK  VBIND
   N-CALLS @ 1+ N-CALLS ! ;

: EMIT-CALL ( IR-ID:ir-op-id n -- )
   {: id:IR-ID:ir-op-id mask:n :}
   DSTACK? 0= if E-X64SEL-CALL throw then
   id SELF-SHAPE {: a:n r:n kk:n m:n :}
   id 0 OPERAND TOK!
   id kk m a mask CALL-SAVE
   id  kk a + X64IR:SLOT-WIDTH * DPLACED
       kk r + X64IR:SLOT-WIDTH * DPLACED  EMIT-CALL-OP
   id kk m r CALL-RESTORE ;

: EMIT-WORD-CALL ( IR-ID:ir-op-id n -- )
   {: id:IR-ID:ir-op-id mask:n :}
   DSTACK? 0= if E-X64SEL-CALL throw then
   id SITE-SHAPE {: a:n r:n kk:n m:n :}
   id 0 OPERAND TOK!
   id kk m a mask CALL-SAVE
   id  kk a + X64IR:SLOT-WIDTH * DPLACED
       kk r + X64IR:SLOT-WIDTH * DPLACED
   id WORD-ENTRY EMIT-WORDCALL-OP
   id kk m r CALL-RESTORE ;

\ ---- leaving through the callee ----------------------------------------------
\ A tail call is not a call site with the end cut off: the pointer must already
\ stand at the callee's entry base, and the branch carries no adjustment.
: TAIL-CK ( n n -- )
   {: a:n r:n :}
   a ARGS SLOT-POSITIONS <> if E-X64SEL-TAIL throw then
   r OUTS SLOT-POSITIONS <> if E-X64SEL-TAIL throw then
   a X64IR:SLOT-WIDTH * DPLACED 0<> if E-X64SEL-TAIL throw then
   r X64IR:SLOT-WIDTH * DPLACED 0<> if E-X64SEL-TAIL throw then ;

: EMIT-TAIL-BR ( IR-ID:ir-op-id n -- )
   {: at:IR-ID:ir-op-id entry:n :}
   at X64IR-OPCODE:TAILCALL OPEN
   TOK OPERAND+
   CTX BLD  CTX BLD X64IR:KEY-ENTRY  CTX BLD entry X64IR:ENTRY-ATTR
   IR-BUILD:ADD-ATTR
   CTX BLD IR-BUILD:END-OP drop ;

\ The epilogue stands in FRONT of the branch and not after it.
: EMIT-TAIL-CALL ( IR-ID:ir-op-id n -- )
   {: id:IR-ID:ir-op-id mask:n :}
   DSTACK? 0= if E-X64SEL-CALL throw then
   id SITE-SHAPE {: a:n r:n kk:n m:n :}
   a r TAIL-CK
   id 0 OPERAND TOK!
   id kk m a mask CALL-SAVE
   id EPILOGUE
   id  id WORD-ENTRY  EMIT-TAIL-BR
   N-TAILS @ 1+ N-TAILS ! ;

: EMIT-CALL-OR-TAIL ( IR-ID:ir-op-id n -- )
   {: id:IR-ID:ir-op-id mask:n :}
   id TAIL-OP? if id mask EMIT-TAIL-CALL exit then
   id mask EMIT-WORD-CALL ;

\ ---- leaving through the routine that ends the process -----------------------
\ The trap registry resolves diagnostics at compile time; only die is needed in
\ the target dictionary.
: TRAP-ENTRY ( -- n )
   NTRAP:ROUTINE$ NDICT:CALL-TARGET {: e:n :}
   e 0= if E-X64SEL-TRAP throw then
   e ;

: EMIT-TRAP-BR ( IR-ID:ir-op-id n n -- )
   {: at:IR-ID:ir-op-id give:n entry:n :}
   at X64IR-OPCODE:TRAP OPEN
   TOK OPERAND+
   CTX BLD  CTX BLD X64IR:KEY-TRAP-ENTRY  CTX BLD entry X64IR:ENTRY-ATTR
   IR-BUILD:ADD-ATTR
   give DBYTES-AT+
   CTX BLD IR-BUILD:END-OP drop ;

\ A call site with nothing to take back: the address, the length and the exit
\ code go into the cells die reads them out of.
: EMIT-TRAP ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   DSTACK? 0= if E-X64SEL-TRAP throw then
   TRAP-CELLS 0 ?do
      id  id i OPERAND  i X64IR:SLOT-WIDTH *  EMIT-DSTORE
   loop
   id  TRAP-CELLS X64IR:SLOT-WIDTH * DPLACED  TRAP-ENTRY  EMIT-TRAP-BR ;

\ ---- selecting a constant ----------------------------------------------------
\ One instruction whatever the cell holds: `mov r64, imm64`, with `x64.addr`
\ saying whether a relocation pass has to find it again.
: EMIT-CONST ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id X64IR-OPCODE:MOVI OPEN
   RESULT+
   CTX BLD  CTX BLD X64IR:KEY-IMM
   CTX BLD  id CONST-VALUE  X64IR:IMM-ATTR  IR-BUILD:ADD-ATTR
   CTX BLD  CTX BLD X64IR:KEY-ADDR
   CTX BLD  id CONST-ADDR  X64IR:ADDR-ATTR  IR-BUILD:ADD-ATTR
   CLOSE-VALUE
   id 0 RESULT-AT  ACC  VBIND ;

\ ---- the literal an instruction carries instead of a register -----------------
\ Only a plain number: an address literal opens a relocation site the emitter
\ has to find, and a site folded into an ALU immediate is a site with no
\ instruction of its own.
: LIT-OP? ( IR-ID:ir-value-id -- bool )
   {: v:IR-ID:ir-value-id :}
   v VALUE-FROM-OP? 0= if false exit then
   v DEF-OP OP-SLOT O-CONST <> if false exit then
   v DEF-OP CONST-ADDR HIR:ADDR-NONE = ;

: LIT-VALUE ( IR-ID:ir-value-id -- n )
   DEF-OP CONST-VALUE ;

: IMM-FOLD? ( IR-ID:ir-value-id -- bool )
   {: v:IR-ID:ir-value-id :}
   v LIT-OP? 0= if false exit then
   v LIT-VALUE {: k:n :}
   k X64IR:IMM-LIMIT negate <  k X64IR:IMM-LIMIT 1- >  or 0= ;

: SHIFT-FOLD? ( IR-ID:ir-value-id -- bool )
   {: v:IR-ID:ir-value-id :}
   v LIT-OP? 0= if false exit then
   v LIT-VALUE {: k:n :}
   k 0 >=  k X64IR:SHIFT-LIMIT <  and ;

\ ---- which values are live where ---------------------------------------------
\ The copy a two-address form needs turns on whether its operand is live after
\ the operation, so the function's liveness is computed before its blocks are
\ walked. It is the ordinary backward dataflow:
\
\    live-in(B)  = reads(B) ∪ (live-out(B) − defs(B))
\    live-out(B) = ∪ live-in(S) over B's successors
\
\ where defs(B) is B's block arguments together with the results of its
\ operations, and reads(B) is the operands it reads before defining them. A
\ terminator is an operation of its own block, so the values an edge carries are
\ reads of the block the branch is IN and not of the block it goes to. Blocks
\ are visited backwards because that is the direction the answers flow, and the
\ iteration stops when no set has moved: the sets only grow and there are
\ finitely many values and blocks, so it terminates.
\
\ The allocator computes the same thing over the MACHINE module (regalloc.f
\ MB-LIVENESS) and cannot be asked for it here: it runs after this pass, over
\ the module this decision has already shaped.
: BLOCK-ORD-CK ( n -- n )
   dup 0 < over BMAX >= or if E-X64SEL-CAP throw then ;

\ The blocks of the source function map one for one, so a block of the MODULE
\ becomes an ordinal of the FUNCTION by taking its base off.
: BLOCK-ORD ( IR-ID:ir-block-id -- n )
   IR-ID:BLOCK-LOCAL  R-BASE @ -  BLOCK-ORD-CK ;

: LWORDS ( -- n ) LIVE-WORDS @ ;

: LWORDS! ( -- )
   V-LIMIT @ V-BASE @ -  SET-BITS 1- +  SET-BITS /  1 max  LIVE-WORDS ! ;

: BIT-CELL ( n -- n )    SET-BITS / ;
: BIT-MASK ( n -- n )    SET-BITS mod 1 swap lshift ;

: LIVE-IX ( n n n -- n )
   {: pl:n b:n w:n :}
   pl BMAX * b +  LWORDS *  w + ;

: LIVE@ ( n n n -- n )   LIVE-IX cells LIVE-SETS + @ ;

: LIVE! ( n n n n -- )
   {: val:n pl:n b:n w:n :}
   val  pl b w LIVE-IX cells LIVE-SETS + ! ;

: LIVE-HAS? ( n n n -- bool )
   {: pl:n b:n v:n :}
   pl b v BIT-CELL LIVE@  v BIT-MASK and 0<> ;

: LIVE-SET ( n n n -- )
   {: pl:n b:n v:n :}
   pl b v BIT-CELL LIVE@  v BIT-MASK or  pl b v BIT-CELL LIVE! ;

\ What the block being read has defined so far: a use under a definition in the
\ same block is not a read of what arrives at the block.
: DEFS-CLEAR ( -- )
   LWORDS 0 ?do 0 i cells DEFSET + ! loop ;

: DEFS-HAS? ( n -- bool )
   {: v:n :}
   v BIT-CELL cells DEFSET + @  v BIT-MASK and 0<> ;

: DEFS-SET ( n -- )
   {: v:n :}
   v BIT-CELL cells DEFSET + @  v BIT-MASK or
   v BIT-CELL cells DEFSET + ! ;

: LIVE-CLEAR1 ( n n -- )
   {: pl:n b:n :}
   LWORDS 0 ?do 0 pl b i LIVE! loop ;

: LIVE-CLEAR ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   f BLOCK-COUNT 0 ?do
      P-IN i LIVE-CLEAR1   P-OUT i LIVE-CLEAR1
      P-USE i LIVE-CLEAR1  P-DEF i LIVE-CLEAR1
   loop ;

: LIVE-USE1 ( n IR-ID:ir-value-id -- )
   {: b:n v:IR-ID:ir-value-id :}
   v VSLOT {: k:n :}
   k DEFS-HAS? if exit then
   P-USE b k LIVE-SET ;

: LIVE-DEF1 ( n IR-ID:ir-value-id -- )
   {: b:n v:IR-ID:ir-value-id :}
   v VSLOT {: k:n :}
   P-DEF b k LIVE-SET
   k DEFS-SET ;

: LIVE-OP-UD ( n IR-ID:ir-op-id -- )
   {: b:n id:IR-ID:ir-op-id :}
   id OPERANDS-OF 0 ?do b  id i OPERAND-AT  LIVE-USE1 loop
   id RESULTS-OF 0 ?do  b  id i RESULT-AT   LIVE-DEF1 loop ;

: LIVE-BLOCK-UD ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id b:n :}
   f b BLOCK-AT {: bk:IR-ID:ir-block-id :}
   DEFS-CLEAR
   bk ARG-COUNT 0 ?do b  bk i ARG-AT  LIVE-DEF1 loop
   bk OP-COUNT 0 ?do  b  bk i OP-AT   LIVE-OP-UD loop ;

: LIVE-OUT-ADD ( n n -- )
   {: b:n s:n :}
   LWORDS 0 ?do
      P-OUT b i LIVE@  P-IN s i LIVE@ or  P-OUT b i LIVE!
   loop ;

\ A repeated edge repeats a successor; a meet over sets is idempotent over that.
: LIVE-OUT! ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id b:n :}
   LWORDS 0 ?do 0 P-OUT b i LIVE! loop
   f b BLOCK-AT TERM-AT {: t:IR-ID:ir-op-id :}
   t SUCCS-OF 0 ?do
      b  t i SUCC-AT BLOCK-ORD  LIVE-OUT-ADD
   loop ;

: LIVE-IN1 ( n n -- bool )
   {: b:n w:n :}
   P-USE b w LIVE@   P-OUT b w LIVE@  P-DEF b w LIVE@ invert and   or {: nv:n :}
   nv  P-IN b w LIVE@ = if false exit then
   nv P-IN b w LIVE!
   true ;

: LIVE-IN! ( n -- bool )
   {: b:n :}
   false
   LWORDS 0 ?do b i LIVE-IN1 or loop ;

: LIVE-PASS1 ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id b:n :}
   f b LIVE-OUT!
   b LIVE-IN! if 1 LIVE-CHANGED ! then ;

: LIVENESS! ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   LWORDS!
   f LIVE-CLEAR
   f BLOCK-COUNT 0 ?do f i LIVE-BLOCK-UD loop
   begin
      0 LIVE-CHANGED !
      f BLOCK-COUNT 0 ?do
         f  f BLOCK-COUNT 1- i -  LIVE-PASS1
      loop
      LIVE-CHANGED @ 0=
   until ;

\ Whether the value leaves the block being selected alive.
: LIVE-OUT? ( IR-ID:ir-value-id -- bool )
   {: v:IR-ID:ir-value-id :}
   P-OUT  BLK BLOCK-ORD  v VSLOT  LIVE-HAS? ;

\ ---- counting what reads a value ---------------------------------------------
\ How many operands of the whole function name a value. Two answers read this: a
\ literal becomes no instruction of its own only when every use of it folds into
\ one, and a tail site may carry nothing anything reads again. A use later in
\ this block and a use in any other block are one answer here: nothing is
\ computed about where a value dies, only whether a use exists that is not the
\ operand being asked about.
: OP-USES ( IR-ID:ir-value-id IR-ID:ir-op-id -- n )
   {: v:IR-ID:ir-value-id id:IR-ID:ir-op-id :}
   0
   id OPERANDS-OF 0 ?do
      id i OPERAND-AT v SAME-VALUE? if 1+ then
   loop ;

: BLOCK-USES ( IR-ID:ir-value-id IR-ID:ir-block-id -- n )
   {: v:IR-ID:ir-value-id bk:IR-ID:ir-block-id :}
   0
   bk OP-COUNT 0 ?do  v bk i OP-AT OP-USES  +  loop ;

: VALUE-USES ( IR-ID:ir-value-id -- n )
   {: v:IR-ID:ir-value-id :}
   0
   FUN BLOCK-COUNT 0 ?do  v FUN i BLOCK-AT BLOCK-USES  +  loop ;

\ ---- selecting the arithmetic ------------------------------------------------
\ Operand 0 is the one the instruction overwrites and the schema ties the result
\ to it. The allocator refuses a tie whose ends cannot share a register
\ (regalloc.f MB-TIE1) instead of repairing it, so the operand a form is about
\ to destroy is copied here whenever it is LIVE AFTER this operation, which is
\ three questions in one: another operand of this operation names it (`dup +`),
\ a later operation of this block reads it, or it is live out of this block. The
\ last of the three is the one a count of uses cannot answer - a value read once
\ inside a loop is read again when the backedge brings control round, and no
\ operand of that branch names it. The copy is `x64.mov` into a fresh value, and
\ the allocator coalesces the ones whose ends can share a register after all.

\ From operand k on, so that the destroyed operand can ask about the others.
: OP-READS-FROM? ( IR-ID:ir-value-id IR-ID:ir-op-id n -- bool )
   {: v:IR-ID:ir-value-id id:IR-ID:ir-op-id k:n :}
   false
   id OPERANDS-OF k ?do
      id i OPERAND-AT v SAME-VALUE? if drop true leave then
   loop ;

\ Where the operation being selected stands in the block being selected. The
\ walk is at this operation, so the block really holds it.
: OP-POSITION ( IR-ID:ir-op-id -- n )
   {: id:IR-ID:ir-op-id :}
   -1
   BLK OP-COUNT 0 ?do
      BLK i OP-AT IR-ID:OP-LOCAL  id IR-ID:OP-LOCAL = if drop i leave then
   loop
   dup 0 < if E-X64SEL-SHAPE throw then ;

: READ-BELOW? ( IR-ID:ir-value-id IR-ID:ir-op-id -- bool )
   {: v:IR-ID:ir-value-id id:IR-ID:ir-op-id :}
   false
   BLK OP-COUNT  id OP-POSITION 1+ ?do
      v  BLK i OP-AT  0 OP-READS-FROM? if drop true leave then
   loop ;

: LIVE-AFTER? ( IR-ID:ir-value-id IR-ID:ir-op-id -- bool )
   {: v:IR-ID:ir-value-id id:IR-ID:ir-op-id :}
   v id 1 OP-READS-FROM? if true exit then
   v id READ-BELOW? if true exit then
   v LIVE-OUT? ;

: TIED-OPERAND ( IR-ID:ir-op-id -- IR-ID:ir-value-id )
   {: id:IR-ID:ir-op-id :}
   id 0 OPERAND-AT  id LIVE-AFTER? if  id  id 0 OPERAND  EMIT-COPY exit then
   id 0 OPERAND ;

: EMIT-BINARY ( IR-ID:ir-op-id X64IR:opcode -- )
   {: id:IR-ID:ir-op-id o:X64IR:opcode :}
   id TIED-OPERAND {: dst:IR-ID:ir-value-id :}
   id o OPEN
   CTX BLD  dst  IR-BUILD:ADD-OPERAND
   CTX BLD  id 1 OPERAND  IR-BUILD:ADD-OPERAND
   RESULT+
   CLOSE-VALUE
   id 0 RESULT-AT  ACC  VBIND ;

: EMIT-BINARY-IMM ( IR-ID:ir-op-id X64IR:opcode n -- )
   {: id:IR-ID:ir-op-id o:X64IR:opcode imm:n :}
   id TIED-OPERAND {: dst:IR-ID:ir-value-id :}
   id o OPEN
   CTX BLD  dst  IR-BUILD:ADD-OPERAND
   RESULT+
   CTX BLD  CTX BLD X64IR:KEY-IMM
   CTX BLD  imm X64IR:IMM32-ATTR  IR-BUILD:ADD-ATTR
   CLOSE-VALUE
   id 0 RESULT-AT  ACC  VBIND ;

: EMIT-SHIFT-IMM ( IR-ID:ir-op-id X64IR:opcode n -- )
   {: id:IR-ID:ir-op-id o:X64IR:opcode count:n :}
   id TIED-OPERAND {: dst:IR-ID:ir-value-id :}
   id o OPEN
   CTX BLD  dst  IR-BUILD:ADD-OPERAND
   RESULT+
   CTX BLD  CTX BLD X64IR:KEY-SHIFT
   CTX BLD  count X64IR:SHIFT-ATTR  IR-BUILD:ADD-ATTR
   CLOSE-VALUE
   id 0 RESULT-AT  ACC  VBIND ;

: EMIT-UNARY ( IR-ID:ir-op-id X64IR:opcode -- )
   {: id:IR-ID:ir-op-id o:X64IR:opcode :}
   id TIED-OPERAND {: dst:IR-ID:ir-value-id :}
   id o OPEN
   CTX BLD  dst  IR-BUILD:ADD-OPERAND
   RESULT+
   CLOSE-VALUE
   id 0 RESULT-AT  ACC  VBIND ;

\ The register form and the immediate form are one rule, because which one the
\ machine has is decided by what the second operand IS.
: BINARY-RULE ( IR-ID:ir-op-id X64IR:opcode X64IR:opcode -- )
   {: id:IR-ID:ir-op-id o:X64IR:opcode oi:X64IR:opcode :}
   id 1 OPERAND-AT IMM-FOLD? if
      id oi  id 1 OPERAND-AT LIT-VALUE  EMIT-BINARY-IMM exit
   then
   id o EMIT-BINARY ;

\ `shl r64, cl` reads its count from rcx and the schema fixes operand 1 there, so
\ the count is COPIED into a fresh value and the copy is the operand: a count
\ another operation reads, and two counts live over one interval, are then two
\ classes the allocator can place in one register one after the other. The copy
\ that was not needed is coalesced away (regalloc.f MB-COALESCE1).
: EMIT-SHIFT-CL ( IR-ID:ir-op-id X64IR:opcode -- )
   {: id:IR-ID:ir-op-id o:X64IR:opcode :}
   id TIED-OPERAND {: dst:IR-ID:ir-value-id :}
   id  id 1 OPERAND  EMIT-COPY {: cnt:IR-ID:ir-value-id :}
   id o OPEN
   CTX BLD  dst  IR-BUILD:ADD-OPERAND
   CTX BLD  cnt  IR-BUILD:ADD-OPERAND
   RESULT+
   CLOSE-VALUE
   id 0 RESULT-AT  ACC  VBIND ;

\ The register form and the immediate form are one rule, as they are for a binary
\ operation, because which one the machine has is decided by what the count IS.
: SHIFT-RULE ( IR-ID:ir-op-id X64IR:opcode X64IR:opcode -- )
   {: id:IR-ID:ir-op-id o:X64IR:opcode oi:X64IR:opcode :}
   id 1 OPERAND-AT SHIFT-FOLD? if
      id oi  id 1 OPERAND-AT LIT-VALUE  EMIT-SHIFT-IMM exit
   then
   id o EMIT-SHIFT-CL ;

\ ---- selecting the division --------------------------------------------------
\ A zero divisor is a CALLER error, so the divide's cold side hands the code to
\ the runtime's `throw` (src/habu/habu1.f BTHROW) exactly as the engine's own `/`
\ does. The entry is asked for here, where the dictionary is readable, and
\ carried to the emitter as the operation's own attribute; the name E-X64SEL-TRAP
\ names is the routine a compiled refusal branches to missing from the target
\ dictionary, which is the same refusal the ARM64 selector makes.
: THROW-ENTRY ( -- n )
   s" throw" NDICT:CALL-TARGET {: e:n :}
   e 0= if E-X64SEL-TRAP throw then
   e ;

\ `idiv r64` takes its dividend in rax and writes rax and rdx, so the dividend is
\ COPIED into a fresh value the allocator can place in rax - the source value
\ itself may be read after the division, and the instruction destroys what it
\ divides. The divisor is the free operand. Two results leave the form because
\ one instruction leaves both: the quotient is what `/` computes and the
\ remainder is the result this lowering does not read.
: EMIT-DIV ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id  id 0 OPERAND  EMIT-COPY {: num:IR-ID:ir-value-id :}
   id X64IR-OPCODE:IDIV OPEN
   CTX BLD  num  IR-BUILD:ADD-OPERAND
   CTX BLD  id 1 OPERAND  IR-BUILD:ADD-OPERAND
   RESULT+
   RESULT+
   CTX BLD  CTX BLD X64IR:KEY-THROW-ENTRY
   CTX BLD  THROW-ENTRY X64IR:ENTRY-ATTR  IR-BUILD:ADD-ATTR
   CLOSE-VALUE
   id 0 RESULT-AT  ACC  VBIND ;

\ ---- selecting a comparison --------------------------------------------------
\ One source relation is one machine condition, and the comparison of two Habu
\ cells is SIGNED, so every one of them is an L/G condition.
: COMPARE-COND ( IR-ID:ir-op-id -- X64IR:cond )
   OP-SLOT
   case
      O-LT    of X64IR-COND:LT    endof
      O-LE    of X64IR-COND:LE    endof
      O-GT    of X64IR-COND:GT    endof
      O-GE    of X64IR-COND:GE    endof
      O-EQUAL of X64IR-COND:EQUAL endof
      O-NE    of X64IR-COND:NE    endof
      E-X64SEL-OPCODE throw
   endcase ;

: COND-ATTR+ ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   CTX BLD  CTX BLD X64IR:KEY-COND
   CTX BLD  id COMPARE-COND  X64IR:COND-ATTR  IR-BUILD:ADD-ATTR ;

\ Compare and set a boolean: ONE operation and three instructions, because the
\ flags between them are a single architectural resource no value may stand for.
\ It is not fused into the branch below it in this slice.
: EMIT-CMPSET ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id X64IR-OPCODE:CMPSET OPEN
   CTX BLD  id 0 OPERAND  IR-BUILD:ADD-OPERAND
   CTX BLD  id 1 OPERAND  IR-BUILD:ADD-OPERAND
   RESULT+
   id COND-ATTR+
   CLOSE-VALUE
   id 0 RESULT-AT  ACC  VBIND ;

\ The operand is the LEFT-hand side and the immediate the right, which is what
\ makes `0=` and `0<` one instruction against the literal zero.
: EMIT-CMPSETI ( IR-ID:ir-op-id n -- )
   {: id:IR-ID:ir-op-id imm:n :}
   id X64IR-OPCODE:CMPSETI OPEN
   CTX BLD  id 0 OPERAND  IR-BUILD:ADD-OPERAND
   RESULT+
   id COND-ATTR+
   CTX BLD  CTX BLD X64IR:KEY-IMM
   CTX BLD  imm X64IR:IMM32-ATTR  IR-BUILD:ADD-ATTR
   CLOSE-VALUE
   id 0 RESULT-AT  ACC  VBIND ;

: COMPARE-RULE ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id 1 OPERAND-AT IMM-FOLD? if
      id  id 1 OPERAND-AT LIT-VALUE  EMIT-CMPSETI exit
   then
   id EMIT-CMPSET ;

\ ---- selecting the memory operations -----------------------------------------
\ The source order and the machine order are ONE order.
: EMIT-MEM ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   DSTACK? 0= if E-X64SEL-MEM throw then
   id 0 RESULT-AT  TOK  VBIND ;

: EMIT-ALOAD ( IR-ID:ir-op-id X64IR:opcode -- )
   {: id:IR-ID:ir-op-id o:X64IR:opcode :}
   id o OPEN
   CTX BLD  id 0 OPERAND  IR-BUILD:ADD-OPERAND
   CTX BLD  id 1 OPERAND  IR-BUILD:ADD-OPERAND
   RESULT+
   TOKEN+
   CTX BLD IR-BUILD:END-OP {: nid:IR-ID:ir-op-id :}
   CTX BLD nid 1 IR-BUILD:OP-RESULT@ {: tk:IR-ID:ir-value-id :}
   tk TOK!
   id 1 RESULT-AT tk VBIND
   id 0 RESULT-AT  CTX BLD nid 0 IR-BUILD:OP-RESULT@  VBIND ;

: EMIT-ASTORE ( IR-ID:ir-op-id X64IR:opcode -- )
   {: id:IR-ID:ir-op-id o:X64IR:opcode :}
   id o OPEN
   CTX BLD  id 0 OPERAND  IR-BUILD:ADD-OPERAND
   CTX BLD  id 1 OPERAND  IR-BUILD:ADD-OPERAND
   CTX BLD  id 2 OPERAND  IR-BUILD:ADD-OPERAND
   TOKEN+
   CTX BLD IR-BUILD:END-OP {: nid:IR-ID:ir-op-id :}
   CTX BLD nid 0 IR-BUILD:OP-RESULT@ {: tk:IR-ID:ir-value-id :}
   tk TOK!
   id 0 RESULT-AT tk VBIND ;

\ ---- selecting the address of another function of this emission ---------------
: QUOT-FUN ( IR-ID:ir-op-id -- n )
   {: id:IR-ID:ir-op-id :}
   id ATTRS-OF 1 <> if E-X64SEL-ATTR throw then
   id 0 ATTR-KEY-AT  0 BND-FUN @  SAME-SYM?
   0= if E-X64SEL-ATTR throw then
   id 0 ATTR-INT-AT ;

: EMIT-QUOT ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id X64IR-OPCODE:CODEADDR OPEN
   RESULT+
   CTX BLD  CTX BLD X64IR:KEY-FUN
   CTX BLD  id QUOT-FUN  X64IR:FUN-ATTR  IR-BUILD:ADD-ATTR
   CLOSE-VALUE
   id 0 RESULT-AT  ACC  VBIND ;

\ ---- selecting the return ----------------------------------------------------
\ Under the data-stack convention every result is stored to its cell and the
\ pointer published; under a register convention the values still live where
\ control leaves become the return's own operands.
: EMIT-EXIT ( IR-ID:ir-op-id n -- )
   {: id:IR-ID:ir-op-id mask:n :}
   OUTS SLOT-POSITIONS {: r:n :}
   id OPERANDS-OF r <> if E-X64SEL-PLACE throw then
   r 0 ?do
      mask i DBIT? 0= if
         id  id i OPERAND  OUTS i NEFF:SEQ-SLOT@ X64IR:SLOT-WIDTH *  EMIT-DSTORE
      then
   loop
   id  r X64IR:SLOT-WIDTH * DPLACED  EMIT-DPUBLISH ;

: EMIT-RETURN ( IR-ID:ir-op-id n -- )
   {: id:IR-ID:ir-op-id mask:n :}
   DSTACK? if id mask EMIT-EXIT then
   id EPILOGUE
   id X64IR-OPCODE:RET OPEN
   DSTACK? 0= if
      id OPERANDS-OF {: k:n :}
      k 0 ?do
         CTX BLD  id i OPERAND  IR-BUILD:ADD-OPERAND
      loop
   then
   CTX BLD IR-BUILD:END-OP drop ;

: EMIT-RETURN-OR-TAILED ( IR-ID:ir-op-id n -- )
   {: id:IR-ID:ir-op-id mask:n :}
   TAIL-HERE? if exit then
   id mask EMIT-RETURN ;

\ ---- the order a block is entered with ---------------------------------------
\ Every data-stack access threads one order, so a block takes the order its
\ edges stated. The machine two-way branch carries no operands, so an order two
\ paths differ on cannot be handed over as an argument here.
: ORDER-ARG? ( n -- bool )
   {: b:n :}
   false
   FUN b BLOCK-AT ARG-COUNT 0 ?do
      FUN b BLOCK-AT i ARG-AT TOKEN? if drop true leave then
   loop ;

: ORDER-SET? ( n -- bool )
   BLOCK-ORD-CK cells D-ORDER-SET + @ 0<> ;

: ORDER@ ( n -- IR-ID:ir-value-id )
   BLOCK-ORD-CK D-ORDER @ ;

: ORDER! ( n -- )
   {: b:n :}
   TOK b BLOCK-ORD-CK D-ORDER !
   1 b BLOCK-ORD-CK cells D-ORDER-SET + ! ;

: ORDER-SAME? ( n -- bool )
   ORDER@ IR-ID:VALUE-LOCAL  TOK IR-ID:VALUE-LOCAL  = ;

: ORDER-CLEAR ( -- )
   BMAX 0 ?do 0 i cells D-ORDER-SET + ! loop ;

: ORDER-EDGE! ( n -- )
   {: b:n :}
   DSTACK? 0= if exit then
   b ORDER-ARG? if exit then
   b ORDER-SET? 0= if b ORDER! exit then
   b ORDER-SAME? 0= if E-X64SEL-ORDER throw then ;

\ A block no edge reached is one control cannot arrive at.
: ORDER-ENTER ( n -- )
   {: b:n :}
   DSTACK? 0= if exit then
   b ORDER-ARG? if exit then
   b ORDER-SET? 0= if E-X64SEL-ORDER throw then
   b ORDER@ TOK! ;

\ ---- selecting the branches --------------------------------------------------
\ The blocks of the source function map one for one, so a successor's ordinal
\ rides across and only the module base changes.
: SUCC-IDX ( IR-ID:ir-op-id n -- n )
   SUCC-AT BLOCK-ORD ;

: SUCCESSOR-ORD+ ( n -- )
   {: b:n :}
   b ORDER-EDGE!
   CTX BLD
   BLD IR-BUILD:MODULE-KEY  R-NEWBASE @ b +  IR-ID:PACK-BLOCK
   IR-BUILD:ADD-SUCCESSOR ;

: SUCCESSOR+ ( IR-ID:ir-op-id n -- )
   {: id:IR-ID:ir-op-id i:n :}
   id i SUCC-IDX SUCCESSOR-ORD+ ;

\ ---- splitting the edges that carry values -----------------------------------
\ Edge destinations must share the successor arguments' registers. Snapshot all
\ sources before those destination copies: a backedge can permute live header
\ values, and writing one destination must not destroy a later source.
\
\ An unchanged header argument already occupies its destination. Copying it
\ would create another value tied to that register while the original can still
\ be live on the loop's exit path.
: EDGE-STAYS? ( IR-ID:ir-op-id n -- bool )
   {: id:IR-ID:ir-op-id i:n :}
   id i OPERAND-AT  id 0 SUCC-AT i ARG-AT  SAME-VALUE? ;

: EDGE-VALUE ( IR-ID:ir-op-id n -- IR-ID:ir-value-id )
   {: id:IR-ID:ir-op-id i:n :}
   id i OPERAND-AT TOKEN?  id i EDGE-STAYS? or if id i OPERAND exit then
   id  id i OPERAND  EMIT-COPY ;

: EDGE-DESTINATION ( IR-ID:ir-op-id n -- )
   {: id:IR-ID:ir-op-id i:n :}
   id i OPERAND-AT TOKEN?  id i EDGE-STAYS? or if exit then
   id  i EDGE-V @  EMIT-COPY  i EDGE-V ! ;

: EMIT-BR ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id OPERANDS-OF {: k:n :}
   k EDGE-MAX > if E-X64SEL-CAP throw then
   k 0 ?do  id i EDGE-VALUE  i EDGE-V !  loop
   k 0 ?do  id i EDGE-DESTINATION  loop
   id X64IR-OPCODE:BR OPEN
   k 0 ?do
      CTX BLD  i EDGE-V @  IR-BUILD:ADD-OPERAND
   loop
   id 0 SUCCESSOR+
   CTX BLD IR-BUILD:END-OP drop ;

\ `test rv, rv` and a jump on the zero flag, with the successors in the source's
\ own order: hir.brz goes to its FIRST successor on zero and so does this form.
: EMIT-BRZ ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id X64IR-OPCODE:BRZ OPEN
   CTX BLD  id 0 OPERAND  IR-BUILD:ADD-OPERAND
   id 0 SUCCESSOR+
   id 1 SUCCESSOR+
   CTX BLD IR-BUILD:END-OP drop ;

\ ---- which slot holds which value, over the whole routine ---------------------
\ A slot of the caller's data stack holds a value of the source module. The
\ answer is a FIXPOINT over the control-flow graph and not a block-local memory,
\ because that is what the validator holds the emission to: regalloc-verify.f
\ keeps the same map over the same graph (VDRES-FIX) and refuses a store into a
\ cell that still holds the value stored (VDSTORE-CK, DKEEP-SAME) and a load
\ nothing reads (VDLOAD-CK, DKEEP-DEAD). A rule that elided fewer stores than
\ the fixpoint sees as redundant would be REFUSED and not merely slower.
: DIN-AT ( n n -- n )
   {: b:n s:n :}
   s DIN-WINDOW? 0= if DNONE exit then
   b BLOCK-ORD-CK DSLOT-MAX * s + cells D-IN + @ ;

: DIN-AT! ( n n n -- )
   {: v:n b:n s:n :}
   s DIN-WINDOW? 0= if exit then
   v  b BLOCK-ORD-CK DSLOT-MAX * s + cells D-IN + ! ;

: DOUT-AT ( n n -- n )
   {: b:n s:n :}
   s DIN-WINDOW? 0= if DNONE exit then
   b BLOCK-ORD-CK DSLOT-MAX * s + cells D-OUT + @ ;

: DOUT-AT! ( n n n -- )
   {: v:n b:n s:n :}
   s DIN-WINDOW? 0= if exit then
   v  b BLOCK-ORD-CK DSLOT-MAX * s + cells D-OUT + ! ;

: DCUR<IN ( n -- )
   {: b:n :}
   DSLOT-MAX 0 ?do  b i DIN-AT  i cells D-CUR + !  loop ;

: DOUT<CUR ( n -- )
   {: b:n :}
   DSLOT-MAX 0 ?do  i cells D-CUR + @  b i DOUT-AT!  loop ;

\ ---- the effect of one source operation on the map ---------------------------
\ Every walk over a block's operations goes through DOP-XFER, so the fixpoint,
\ the need pass and the emission read one transfer.
: DSAVE-XFER ( IR-ID:ir-op-id n n n -- n )
   {: id:IR-ID:ir-op-id kk:n m:n a:n :}
   0
   kk a + 0 ?do
      id kk m i DSAVE-VAL  i  DPUT? if
         i DELIDE-MAX < if 1 i lshift or then
      then
   loop ;

\ Every slot the callee could have written stops holding anything this routine
\ can name.
: DBACK-XFER ( IR-ID:ir-op-id n n n -- )
   {: id:IR-ID:ir-op-id kk:n m:n r:n :}
   DKILL
   kk r + 0 ?do
      id kk m i DBACK-VAL  i  DRES!
   loop ;

: DEXIT-XFER ( IR-ID:ir-op-id n -- n )
   {: id:IR-ID:ir-op-id r:n :}
   id OPERANDS-OF r <> if E-X64SEL-PLACE throw then
   0
   r 0 ?do
      id i OPERAND-AT  OUTS i NEFF:SEQ-SLOT@  DPUT? if
         i DELIDE-MAX < if 1 i lshift or then
      then
   loop ;

: DCALL-XFER ( IR-ID:ir-op-id n n n n -- n )
   {: id:IR-ID:ir-op-id a:n r:n kk:n m:n :}
   id kk m a DSAVE-XFER {: mask:n :}
   id kk m r DBACK-XFER
   mask ;

\ An addressed store has no arm: it destroys nothing this map holds. A trap
\ writes the cells die reads and is a TERMINATOR (hir.f), so nothing that could
\ read the map follows it.
: DOP-XFER ( IR-ID:ir-op-id -- n )
   {: id:IR-ID:ir-op-id :}
   id OP-SLOT {: s:n :}
   s O-CALL = if id  id SELF-SHAPE  DCALL-XFER exit then
   s O-WORDCALL = if id  id SITE-SHAPE  DCALL-XFER exit then
   s O-RETURN = if
      DSTACK? 0= if 0 exit then
      id  OUTS SLOT-POSITIONS  DEXIT-XFER exit
   then
   0 ;

: DXFER-BLOCK ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id b:n :}
   b DCUR<IN
   f b BLOCK-AT {: bk:IR-ID:ir-block-id :}
   bk OP-COUNT 0 ?do  bk i OP-AT DOP-XFER drop  loop
   b DOUT<CUR ;

\ ---- the meet, and the translation that makes it exact -----------------------
: DMEET1 ( n n -- n )
   {: a:n b:n :}
   a DANY = if b exit then
   b DANY = if a exit then
   a b = if a exit then
   DNONE ;

\ Function inputs retain their value across every loop turn. Other names need
\ an edge argument to identify the value carried into the next turn.
: DENTRY-VALUE? ( n -- bool ) {: v:n :}
   false
   ARGS SLOT-POSITIONS 0 ?do
      0 ARGS i NEFF:SEQ-SLOT@ DIN-AT v = if drop true leave then
   loop ;

: DXLATE ( IR-ID:ir-op-id IR-ID:ir-block-id bool n -- n )
   {: t:IR-ID:ir-op-id tb:IR-ID:ir-block-id back:bool v:n :}
   v 0 < if v exit then
   tb ARG-COUNT {: k:n :}
   DANY
   t OP-SLOT O-BR = if
      t OPERANDS-OF k = if
         k 0 ?do
            t i OPERAND-AT VSLOT v = if
               drop  tb i ARG-AT VSLOT  leave
            then
         loop
      then
   then
   dup DANY <> if exit then
   drop
   back if v DENTRY-VALUE? 0= if DNONE exit then then
   v ;

: DMEET-EDGE ( IR-ID:ir-op-id IR-ID:ir-block-id n n -- )
   {: t:IR-ID:ir-op-id tb:IR-ID:ir-block-id p:n b:n :}
   b p <= {: back:bool :}
   DSLOT-MAX 0 ?do
      i cells D-MEET + @
      t tb  back  p i DOUT-AT  DXLATE
      DMEET1
      i cells D-MEET + !
   loop ;

: DEDGE? ( IR-ID:ir-op-id n -- bool )
   {: t:IR-ID:ir-op-id b:n :}
   false
   t SUCCS-OF 0 ?do
      t i SUCC-IDX b = if drop true leave then
   loop ;

: DMEET-FROM ( IR-ID:ir-fun-id n n -- )
   {: f:IR-ID:ir-fun-id p:n b:n :}
   f p BLOCK-AT TERM-AT {: t:IR-ID:ir-op-id :}
   t b DEDGE? 0= if exit then
   t  f b BLOCK-AT  p b DMEET-EDGE ;

: DIN-SET? ( n n n -- bool )
   {: v:n b:n s:n :}
   b s DIN-AT v = if false exit then
   v b s DIN-AT!
   true ;

: DMEET-BLOCK ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id b:n :}
   DSLOT-MAX 0 ?do DANY i cells D-MEET + ! loop
   f b BLOCK-AT {: bk:IR-ID:ir-block-id :}
   f BLOCK-COUNT {: n:n :}
   bk PRED-COUNT 0 ?do
      bk i PRED-AT IR-ID:BLOCK-LOCAL R-BASE @ - {: p:n :}
      p 0 >= p n < and if f p b DMEET-FROM then
   loop
   DSLOT-MAX 0 ?do
      i cells D-MEET + @  b i DIN-SET? if 1 D-MOVED ! then
   loop ;

\ ---- the entry map, and the fixpoint over the rest ---------------------------
: DIN-ANY ( n -- )
   {: b:n :}
   DSLOT-MAX 0 ?do DANY b i DIN-AT! loop ;

: DENTRY-IN ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   DSLOT-MAX 0 ?do DNONE 0 i DIN-AT! loop
   DSTACK? 0= if exit then
   f 0 BLOCK-AT {: bk:IR-ID:ir-block-id :}
   ARGS SLOT-POSITIONS {: a:n :}
   bk ARG-COUNT a <> if E-X64SEL-PLACE throw then
   a 0 ?do
      bk i ARG-AT VSLOT  0  ARGS i NEFF:SEQ-SLOT@  DIN-AT!
   loop ;

: DIN-INIT ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   f BLOCK-COUNT 1 ?do i DIN-ANY loop
   f DENTRY-IN ;

: DRES-ROUND ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   \ The entry is fixed. Let later blocks read this round's earlier outputs.
   f BLOCK-COUNT 1 ?do  f i DMEET-BLOCK  f i DXFER-BLOCK  loop ;

\ Every cell starts at "nothing said", may name one value, and may then fall to
\ "nothing", so the descent has a bounded number of rounds.
: DRES-ROUNDS ( -- n ) BMAX DSLOT-MAX * 2 * 2 + ;

: DRES-FIX ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   f DIN-INIT
   f BLOCK-COUNT 0 ?do f i DXFER-BLOCK loop
   0
   begin
      1 D-MOVED !
      dup DRES-ROUNDS >= if E-X64SEL-CAP throw then
      0 D-MOVED !
      f DRES-ROUND
      1+
      D-MOVED @ 0=
   until
   drop ;

\ ---- which values reach a register -------------------------------------------
: DNEED-CLEAR ( -- )
   VMAX 0 ?do 0 i cells D-NEED + ! loop ;

\ The routine's own interface is not this pass's to change.
: DNEED-ENTRY ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   DSTACK? if exit then
   f 0 BLOCK-AT {: bk:IR-ID:ir-block-id :}
   bk ARG-COUNT 0 ?do bk i ARG-AT DNEED+ loop ;

: DNEED-OPERANDS ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id OPERANDS-OF 0 ?do id i OPERAND-AT DNEED+ loop ;

\ A value the site writes down needs a register to be written out of; one whose
\ store was elided does not. The values the site takes BACK are named by whoever
\ reads them, which is the ordinary operand rule.
: DNEED-CALL ( IR-ID:ir-op-id n n n n n -- )
   {: id:IR-ID:ir-op-id mask:n a:n r:n kk:n m:n :}
   id 0 OPERAND-AT DNEED+
   kk a + 0 ?do
      mask i DBIT? 0= if id kk m i DSAVE-VAL DNEED+ then
   loop ;

: DNEED-EXIT ( IR-ID:ir-op-id n -- )
   {: id:IR-ID:ir-op-id mask:n :}
   DSTACK? 0= if id DNEED-OPERANDS exit then
   OUTS SLOT-POSITIONS 0 ?do
      mask i DBIT? 0= if id i OPERAND-AT DNEED+ then
   loop ;

\ A branch's operands are read like any other: this machine builds EVERY block
\ argument (OPEN-ARGS), so an edge carries every value it names.
: DNEED-OP ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id DOP-XFER {: mask:n :}
   id OP-SLOT {: s:n :}
   s O-CALL = if id mask  id SELF-SHAPE  DNEED-CALL exit then
   s O-WORDCALL = if id mask  id SITE-SHAPE  DNEED-CALL exit then
   s O-RETURN = if id mask DNEED-EXIT exit then
   id DNEED-OPERANDS ;

: DNEED-BLOCK ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id b:n :}
   b DCUR<IN
   f b BLOCK-AT {: bk:IR-ID:ir-block-id :}
   bk OP-COUNT 0 ?do  bk i OP-AT DNEED-OP  loop ;

: DNEED-FIX ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   DNEED-CLEAR
   f DNEED-ENTRY
   begin
      0 D-MOVED !
      f BLOCK-COUNT 0 ?do f i DNEED-BLOCK loop
      D-MOVED @ 0=
   until ;

: DRESIDENCY ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   f DRES-FIX
   f DNEED-FIX ;

\ ---- the selection table -----------------------------------------------------
\ The selector may not lower a may-trap source operation to a form that does not
\ reproduce the trap. x86-64's add, sub and imul wrap exactly as ARM64's do, so
\ a trapping compilation unit is refused here too.
: TRAP-PRESERVED? ( HIR:opcode -- bool )
   MATCH HIR:opcode
      const  OF false ENDOF
      add    OF false ENDOF
      sub    OF false ENDOF
      mul    OF false ENDOF
      div    OF true  ENDOF
      lt     OF false ENDOF
      le     OF false ENDOF
      gt     OF false ENDOF
      ge     OF false ENDOF
      equal  OF false ENDOF
      ne     OF false ENDOF
      and    OF false ENDOF
      or     OF false ENDOF
      xor    OF false ENDOF
      lshift OF false ENDOF
      rshift OF false ENDOF
      invert OF false ENDOF
      mem    OF false ENDOF
      load   OF false ENDOF
      store  OF false ENDOF
      bload  OF false ENDOF
      bstore OF false ENDOF
      br     OF false ENDOF
      brz    OF false ENDOF
      call   OF true  ENDOF
      wordcall OF true ENDOF
      return OF false ENDOF
      trap   OF true  ENDOF
      fconst   OF false ENDOF
      fadd     OF false ENDOF
      fsub     OF false ENDOF
      fmul     OF false ENDOF
      fdiv     OF false ENDOF
      fneg     OF false ENDOF
      fabs     OF false ENDOF
      fsqrt    OF false ENDOF
      intreal  OF false ENDOF
      realint  OF false ENDOF
      bitsreal OF false ENDOF
      realbits OF false ENDOF
      flt      OF false ENDOF
      fgt      OF false ENDOF
      feq      OF false ENDOF
      fltz     OF false ENDOF
      feqz     OF false ENDOF
      quot     OF false ENDOF
   ;MATCH ;

: TRAP-CK ( HIR:opcode IR-ID:ir-symbol-id -- HIR:opcode )
   {: o:HIR:opcode sym:IR-ID:ir-symbol-id :}
   V-SCHR VW sym IR-SCHEMA:FTRAPS? if
      o TRAP-PRESERVED? 0= if E-X64SEL-TRAP throw then
   then
   o ;

\ ---- one source operation, lowered -------------------------------------------
\ The transfer is applied HERE and once, and the mask it answers is this site's
\ residency: the boundary rules are the only ones that read it.
: RULE ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id OPCODE-AT {: sym:IR-ID:ir-symbol-id :}
   sym OPCODE-SLOT HIR:NTH  sym TRAP-CK
   id DOP-XFER {: mask:n :}
   MATCH HIR:opcode
      const  OF id EMIT-CONST ENDOF
      add    OF id X64IR-OPCODE:ADD X64IR-OPCODE:ADDI BINARY-RULE ENDOF
      sub    OF id X64IR-OPCODE:SUB X64IR-OPCODE:SUBI BINARY-RULE ENDOF
      mul    OF id X64IR-OPCODE:IMUL EMIT-BINARY ENDOF
      div    OF id EMIT-DIV ENDOF
      lt     OF id COMPARE-RULE ENDOF
      le     OF id COMPARE-RULE ENDOF
      gt     OF id COMPARE-RULE ENDOF
      ge     OF id COMPARE-RULE ENDOF
      equal  OF id COMPARE-RULE ENDOF
      ne     OF id COMPARE-RULE ENDOF
      and    OF id X64IR-OPCODE:AND X64IR-OPCODE:ANDI BINARY-RULE ENDOF
      or     OF id X64IR-OPCODE:OR X64IR-OPCODE:ORI BINARY-RULE ENDOF
      xor    OF id X64IR-OPCODE:XOR X64IR-OPCODE:XORI BINARY-RULE ENDOF
      lshift OF id X64IR-OPCODE:SHL X64IR-OPCODE:SHLI SHIFT-RULE ENDOF
      rshift OF id X64IR-OPCODE:SHR X64IR-OPCODE:SHRI SHIFT-RULE ENDOF
      invert OF id X64IR-OPCODE:NOT EMIT-UNARY ENDOF
      mem    OF id EMIT-MEM ENDOF
      load   OF id X64IR-OPCODE:ALOAD EMIT-ALOAD ENDOF
      store  OF id X64IR-OPCODE:ASTORE EMIT-ASTORE ENDOF
      bload  OF id X64IR-OPCODE:ABLOAD EMIT-ALOAD ENDOF
      bstore OF id X64IR-OPCODE:ABSTORE EMIT-ASTORE ENDOF
      br     OF id EMIT-BR ENDOF
      brz    OF id EMIT-BRZ ENDOF
      call   OF id mask EMIT-CALL ENDOF
      wordcall OF id mask EMIT-CALL-OR-TAIL ENDOF
      return OF id mask EMIT-RETURN-OR-TAILED ENDOF
      trap   OF id EMIT-TRAP ENDOF
      fconst   OF E-X64SEL-FLOAT throw ENDOF
      fadd     OF E-X64SEL-FLOAT throw ENDOF
      fsub     OF E-X64SEL-FLOAT throw ENDOF
      fmul     OF E-X64SEL-FLOAT throw ENDOF
      fdiv     OF E-X64SEL-FLOAT throw ENDOF
      fneg     OF E-X64SEL-FLOAT throw ENDOF
      fabs     OF E-X64SEL-FLOAT throw ENDOF
      fsqrt    OF E-X64SEL-FLOAT throw ENDOF
      flt      OF E-X64SEL-FLOAT throw ENDOF
      fgt      OF E-X64SEL-FLOAT throw ENDOF
      feq      OF E-X64SEL-FLOAT throw ENDOF
      fltz     OF E-X64SEL-FLOAT throw ENDOF
      feqz     OF E-X64SEL-FLOAT throw ENDOF
      intreal  OF E-X64SEL-FLOAT throw ENDOF
      realint  OF E-X64SEL-FLOAT throw ENDOF
      bitsreal OF E-X64SEL-FLOAT throw ENDOF
      realbits OF E-X64SEL-FLOAT throw ENDOF
      quot     OF id EMIT-QUOT ENDOF
   ;MATCH ;

\ ---- the literal that never becomes an instruction ---------------------------
\ A literal folded into every instruction that reads it needs no `x64.movi` of
\ its own. Whether that is so is COUNTED over the whole function: one use that
\ is not a fold - an operand the form has no immediate for, a branch argument, a
\ second reader - and the literal is materialised exactly as it always was.
: FOLDS-OPERAND1? ( IR-ID:ir-op-id -- bool )
   {: id:IR-ID:ir-op-id :}
   id OPERANDS-OF 2 < if false exit then
   id OP-SLOT {: s:n :}
   s O-LSHIFT =  s O-RSHIFT = or if id 1 OPERAND-AT SHIFT-FOLD? exit then
   s O-ADD = s O-SUB = or s O-AND = or s O-OR = or s O-XOR = or
   s O-LT = or s O-LE = or s O-GT = or s O-GE = or
   s O-EQUAL = or s O-NE = or
   0= if false exit then
   id 1 OPERAND-AT IMM-FOLD? ;

: OP-FOLDS ( IR-ID:ir-value-id IR-ID:ir-op-id -- n )
   {: v:IR-ID:ir-value-id id:IR-ID:ir-op-id :}
   id FOLDS-OPERAND1? 0= if 0 exit then
   id 1 OPERAND-AT v SAME-VALUE? if 1 else 0 then ;

: BLOCK-FOLDS ( IR-ID:ir-value-id IR-ID:ir-block-id -- n )
   {: v:IR-ID:ir-value-id bk:IR-ID:ir-block-id :}
   0
   bk OP-COUNT 0 ?do  v bk i OP-AT OP-FOLDS  +  loop ;

: VALUE-FOLDS ( IR-ID:ir-value-id -- n )
   {: v:IR-ID:ir-value-id :}
   0
   FUN BLOCK-COUNT 0 ?do  v FUN i BLOCK-AT BLOCK-FOLDS  +  loop ;

: LIT-FOLDED-AWAY? ( IR-ID:ir-op-id -- bool )
   {: id:IR-ID:ir-op-id :}
   id OP-SLOT O-CONST <> if false exit then
   id RESULTS-OF 1 <> if false exit then
   id 0 RESULT-AT {: v:IR-ID:ir-value-id :}
   v VALUE-USES {: u:n :}
   u 0= if false exit then
   v VALUE-FOLDS u = ;

\ ---- opening the selected function -------------------------------------------
\ The two modules number their symbols separately, so the name is copied out of
\ the source module's interner and interned into the new one.
: FUN-NAME ( IR-ID:ir-fun-id -- IR-ID:ir-symbol-id )
   {: f:IR-ID:ir-fun-id :}
   V-SYMP VW V-SYMR VW  V-FUNR VW MKEY f IR-FUN:FSYMBOL@  NAMEBUF NAME-CAP
   IR-SYM:FCOPY {: u:n :}
   CTX BLD NAMEBUF u IR-BUILD:INTERN-SYMBOL ;

: FUN-SIG ( IR-ID:ir-fun-id -- IR-ID:ir-type-id )
   {: f:IR-ID:ir-fun-id :}
   V-TYPR VW  V-FUNR VW MKEY f IR-FUN:FSIGNATURE@  IR-TYPE:FARITY@
   {: in:n out:n :}
   CTX BLD X64IR:GPR-TYPE {: t:IR-ID:ir-type-id :}
   IR-TYPE:FN-BEGIN
   in 0 ?do t IR-TYPE:FN-PARAM loop
   out 0 ?do t IR-TYPE:FN-RESULT loop
   CTX BLD IR-BUILD:INTERN-CODE-REF ;

: OPEN-FUN ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   CTX BLD f FUN-NAME IR-BUILD:BEGIN-FUN
   CTX BLD f FUN-SIG IR-BUILD:SET-SIGNATURE
   CTX BLD  V-FUNR VW f IR-FUN:FLINKAGE@  IR-BUILD:SET-LINKAGE
   CTX BLD  V-FUNR VW f IR-FUN:FVISIBILITY@  IR-BUILD:SET-VISIBILITY
   CTX BLD  V-FUNR VW f IR-FUN:FCONVENTION@  IR-BUILD:SET-CONVENTION
   CTX BLD f FUN-SPAN IR-BUILD:SET-FUN-SPAN ;

\ At the class the source argument's TYPE says. A block argument of the
\ memory-order type is the ORDER arriving.
: OPEN-ARG1 ( IR-ID:ir-value-id -- )
   {: a:IR-ID:ir-value-id :}
   a TOKEN? if
      a  CTX BLD  CTX BLD X64IR:MEM-TYPE  IR-BUILD:ADD-BLOCK-ARG
      dup TOK!  VBIND
      exit
   then
   a  CTX BLD  CTX BLD X64IR:GPR-TYPE  IR-BUILD:ADD-BLOCK-ARG  VBIND ;

: OPEN-ARGS ( IR-ID:ir-block-id -- )
   {: bk:IR-ID:ir-block-id :}
   bk ARG-COUNT 0 ?do bk i ARG-AT OPEN-ARG1 loop ;

\ Under the data-stack convention the entry block takes no argument at all,
\ because nothing arrives in a register: the pointer is taken and every
\ argument something reads out of a register is loaded out of its cell. An
\ argument nothing reads is not loaded: the validator refuses a load whose
\ result has no use as readily as a redundant store.
: OPEN-DARGS ( IR-ID:ir-block-id -- )
   {: bk:IR-ID:ir-block-id :}
   ARGS SLOT-POSITIONS {: a:n :}
   bk ARG-COUNT a <> if E-X64SEL-PLACE throw then
   bk 0 OP-AT {: at:IR-ID:ir-op-id :}
   at PROLOGUE
   at  a X64IR:SLOT-WIDTH * DPLACED  EMIT-DTAKE
   0 ORDER-EDGE!
   a 0 ?do
      bk i ARG-AT {: v:IR-ID:ir-value-id :}
      v DNEED? if
         v  at  ARGS i NEFF:SEQ-SLOT@ X64IR:SLOT-WIDTH *  EMIT-DLOAD  VBIND
      then
   loop ;

\ Only the entry block carries the routine's interface.
: OPEN-BLOCK ( IR-ID:ir-block-id n -- )
   {: bk:IR-ID:ir-block-id ord:n :}
   CTX BLD IR-BUILD:BEGIN-BLOCK
   CTX BLD bk BLOCK-SPAN IR-BUILD:SET-BLOCK-SPAN
   DSTACK? ord 0= and if bk OPEN-DARGS exit then
   bk OPEN-ARGS
   ord ORDER-ENTER ;

\ ---- which operation this function leaves through, decided once --------------
\ The operation immediately in front of the terminator of a block whose
\ terminator is the return.
: TAIL-AT? ( IR-ID:ir-block-id n -- bool )
   {: bk:IR-ID:ir-block-id at:n :}
   TAIL? 0= if false exit then
   bk OP-COUNT {: n:n :}
   n 2 < if false exit then
   at n 2 - <> if false exit then
   bk at OP-AT OP-SLOT O-WORDCALL <> if false exit then
   bk n 1- OP-AT OP-SLOT O-RETURN = ;

\ Nothing the site would carry is read again, which is the one thing a tail
\ branch needs that the source operation does not say.
: TAIL-DEAD-CK ( IR-ID:ir-op-id n -- )
   {: id:IR-ID:ir-op-id k:n :}
   k 0 ?do
      id i 1+ RESULT-AT VALUE-USES 0<> if E-X64SEL-TAIL throw then
   loop ;

: TAIL-POS ( IR-ID:ir-block-id -- n )
   {: bk:IR-ID:ir-block-id :}
   bk OP-COUNT {: n:n :}
   n 2 < if -1 exit then
   bk n 2 - TAIL-AT? 0= if -1 exit then
   n 2 - ;

: SITE-CROSSED ( IR-ID:ir-op-id -- n )
   WORD-SHAPE {: kk:n m:n :}
   2drop
   kk m + ;

\ A SECOND site in one function is refused rather than one of them chosen.
: TAIL-SITE-TRY ( IR-ID:ir-block-id -- )
   {: bk:IR-ID:ir-block-id :}
   bk TAIL-POS {: at:n :}
   at 0 < if exit then
   TAIL-SITE @ 0 >= if E-X64SEL-TAIL throw then
   bk at OP-AT {: id:IR-ID:ir-op-id :}
   id IR-ID:OP-LOCAL TAIL-SITE !
   id  id SITE-CROSSED  TAIL-DEAD-CK ;

: TAIL-SITE! ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id ord:n :}
   -1 TAIL-SITE !
   TAIL? 0= if exit then
   ord 0<> if exit then
   f BLOCK-COUNT 0 ?do  f i BLOCK-AT TAIL-SITE-TRY  loop ;

\ ---- where this function's blocks and its pointer stand ----------------------
: R-BASE! ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   f 0 BLOCK-AT IR-ID:BLOCK-LOCAL R-BASE !
   f BLOCK-COUNT 0 ?do
      f i BLOCK-AT IR-ID:BLOCK-LOCAL  R-BASE @ -  i <>
      if E-X64SEL-SHAPE throw then
   loop ;

\ The entry base, except in a routine that leaves through its callee: there the
\ tail branch carries no adjustment, so the pointer never moves at all.
: DPLACE ( -- )
   0 D-POS !
   DSTACK? 0= if exit then
   TAIL? 0= if exit then
   ARGS SLOT-POSITIONS X64IR:SLOT-WIDTH * D-POS ! ;

\ ---- the walk ----------------------------------------------------------------
\ The value map is NOT cleared between blocks: a value defined in one block and
\ read in another is ordinary.
: WALK-BLOCK ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id ord:n :}
   ord DCUR<IN
   f ord BLOCK-AT {: bk:IR-ID:ir-block-id :}
   bk ord OPEN-BLOCK
   bk 0 S-BLK !
   bk OP-COUNT {: n:n :}
   n 0 ?do
      bk i OP-AT {: id:IR-ID:ir-op-id :}
      id LIT-FOLDED-AWAY? 0= if id RULE then
   loop
   CTX BLD IR-BUILD:END-BLOCK drop ;

\ Block by block in the order the module records them, which is the order every
\ later pass reads too.
: WALK-FUN ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id ord:n :}
   f BLOCK-COUNT {: n:n :}
   n 1 < if E-X64SEL-SHAPE throw then
   n BMAX > if E-X64SEL-CAP throw then
   f 0 S-FUN !
   f ord FUN-PLACES!
   f ord TAIL-SITE!
   f OPEN-FUN
   f V-RANGE!
   VCLEAR
   ORDER-CLEAR
   f R-BASE!
   f LIVENESS!
   f DRESIDENCY
   DPLACE
   n 0 ?do
      f i WALK-BLOCK
   loop
   R-NEWBASE @ n + R-NEWBASE !
   CTX BLD IR-BUILD:END-FUN drop ;

\ ---- what one selection run is told ------------------------------------------
\ The new module gets the same source the old one has, carried row and all.
: SOURCE! ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   V-SRC VW IR-SOURCE:FSOURCES 1 <> if E-X64SEL-SHAPE throw then
   c b  V-SRC VW  MKEY 0 IR-ID:PACK-SOURCE  IR-BUILD:CARRY-SOURCE 0 S-SID ! ;

: BND-TAKE ( -- )
   BND-MODE @ {: have:n :}
   BOUND-NO BND-MODE !
   have BOUND-YES <> if E-X64SEL-BIND throw then ;

: BND-MODULE-CK ( IR-BUILD:module -- )
   IR-BUILD:FMODULE  0 BND-MOD @  IR-ID:MODULE-SAME?
   0= if E-X64SEL-SOURCE throw then ;

: HIR-CK ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b  c b IR-BUILD:DIALECT@  HIR:NAME IR-BUILD:SYMBOL-IS?
   0= if E-X64SEL-SOURCE throw then
   c b IR-BUILD:SCHEMA-MAJOR@ HIR:MAJOR <> if E-X64SEL-SOURCE throw then
   c b IR-BUILD:SCHEMA-MINOR@ HIR:MINOR <> if E-X64SEL-SOURCE throw then ;

\ The machine a contract is written against is the contract's own field, and a
\ backend that lowered for another machine's would be reading numbers that mean
\ something else.
: MACHINE-CK ( NMACH:mach -- )
   X64M:MACHINE NMACH-MACH:EQ 0= if E-X64SEL-MACHINE throw then ;

public

\ ---- binding the source dialect ----------------------------------------------
\ The only moment a module can be asked its opcode identities, because its
\ symbols are its own ordinals.
: BIND-SOURCE ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   BND-MODE @ BOUND-YES = if E-X64SEL-BIND throw then
   c b HIR-CK
   b IR-BUILD:MODULE@ 0 BND-MOD !
   HIR:OPCODES 0 ?do
      c b i HIR:BIND i BND-OP !
   loop
   c b HIR:KEY-VALUE 0 BND-VAL !
   c b HIR:KEY-ADDR  0 BND-ADDR !
   c b HIR:KEY-FUN   0 BND-FUN !
   c b HIR:KEY-ENTRY 0 BND-ENTRY !
   c b HIR:KEY-IN    0 BND-IN !
   c b HIR:KEY-OUT   0 BND-OUT !
   c b HIR:MEM-TYPE 0 BND-MEM !
   BOUND-YES BND-MODE ! ;

\ Each pass answers for itself, because a caller cleaning up after a refusal
\ cannot know how far the run got.
: BOUND? ( -- bool )
   BND-MODE @ BOUND-YES = ;

: RELEASE ( -- )
   BND-TAKE ;

\ ---- the pass ----------------------------------------------------------------
: SELECT ( IR-CTX:ctx IR-BUILD:module IR-BUILD:builder NEFF:routine -- IR-BUILD:module )
   NEFF:VALIDATE NEFF-ROUTINE:UNMAKE
   {: cv:NEFF:conv gi:NEFF:placeseq gr:NEFF:placeseq gc:NEFF:gprs
      fi:NEFF:fprs fr:NEFF:fprs fc:NEFF:fprs
      z:NEFF:nzcv l:NEFF:link ct:NEFF:control
      t:NEFF:traits size:n delta:n mch:NMACH:mach :}
   {: c:IR-CTX:ctx m:IR-BUILD:module b:IR-BUILD:builder :}
   c X64IR:CHECK-TARGET
   mch MACHINE-CK
   BND-TAKE
   m BND-MODULE-CK
   gi 0 S-ARGS !
   gr 0 S-OUTS !
   gi NEFF:SEQ-LEN S-DECL-IN !
   gr NEFF:SEQ-LEN S-DECL-OUT !
   t 0 S-TRT !
   size S-FRAME !
   ct NEFF-CONTROL:TAIL-CALL NEFF-CONTROL:EQ if 1 else 0 then S-TAIL !
   cv NEFF-CONV:DSTACK NEFF-CONV:EQ if 1 else 0 then S-DSTACK !
   0 N-CALLS !
   0 N-TAILS !
   -1 TAIL-SITE !
   0 R-NEWBASE !
   CONTRACT-CK
   c 0 S-CTX !
   b 0 S-BLD !
   m VIEWS!
   RESERVE-SCRATCH
   c b SOURCE!
   FUN-COUNT {: n:n :}
   n 0 ?do
      MKEY i IR-ID:PACK-FUN i WALK-FUN
   loop
   CALLED-CK
   TAILED-CK
   c b IR-BUILD:FREEZE ;

: RESET-SCRATCH ( -- )
   0 SCRATCH-VALUES ! 0 SCRATCH-BLOCKS ! ;

private
get-current prot-wid-add

public
get-current prot-wid-add

;using
;package
