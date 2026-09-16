\ native-trap.f - checked tests for the terminator that does not return.

require lib/test.f
require lib/string.f
require lib/process.f
require lib/process-argv.f
require lib/engine-candidate.f
require test/compiler/native-chain-fixture.f
require src/compiler/native/publish.f
require src/compiler/native/compiler.f

package NTRAP-TEST
private

\ `evaluate` is the metaprogramming boundary the checker does not model.
TRUSTED: EV ( ptr u8 n -- )
   evaluate ;

: REC-LEN ( ptr u8 n -- n )
   XREF-FIND
   dup XREF-FOUND? 0= if s" native-trap: record not found" 76 die then
   XREF-LEN ;

\ ---- the module a shape is built into ----------------------------------------
create TXT
   58 c, 32 c, 84 c, 82 c, 80 c, 32 c, 100 c, 117 c,
   112 c, 32 c, 42 c, 32 c, 59 c, 32 c, 32 c, 32 c,
16 constant TXT-N

2 constant NAME-ST
3 constant NAME-LN
0 constant OPEN-ST
1 constant OPEN-LN
6 constant BODY-ST
3 constant BODY-LN
12 constant CLOSE-ST
1 constant CLOSE-LN

1 TYPED-BUFFER W-CTX IR-CTX:ctx
1 TYPED-BUFFER W-BLD IR-BUILD:builder
1 TYPED-BUFFER W-SRC IR-ID:ir-source-id

: CC ( -- IR-CTX:ctx )               0 W-CTX @ ;
: BB ( -- IR-BUILD:builder )         0 W-BLD @ ;
: SS ( -- IR-ID:ir-source-id )       0 W-SRC @ ;

: SPN ( n n -- IR-SOURCE:span )
   {: st:n ln:n :}
   BB SS st ln IR-BUILD:ADD-SPAN ;

: CELLT ( -- IR-ID:ir-type-id )
   CC BB IR--TYPE-WIDTH:W64 IR--TYPE-SIGN:SIGNED IR-BUILD:INTERN-INT ;

: SIGN ( n n -- IR-ID:ir-type-id )
   {: in:n out:n :}
   CELLT {: t:IR-ID:ir-type-id :}
   IR-TYPE:FN-BEGIN
   in 0 ?do t IR-TYPE:FN-PARAM loop
   out 0 ?do t IR-TYPE:FN-RESULT loop
   CC BB IR-BUILD:INTERN-CODE-REF ;

: OPEN-FUN ( ptr u8 n n n -- )
   {: p u:n in:n out:n :}
   CC BB  CC BB p u IR-BUILD:INTERN-SYMBOL  IR-BUILD:BEGIN-FUN
   CC BB  in out SIGN  IR-BUILD:SET-SIGNATURE
   CC BB IR--FUN-LINKAGE:DEFINED IR-BUILD:SET-LINKAGE
   CC BB IR--FUN-VISIBILITY:EXPORTED IR-BUILD:SET-VISIBILITY
   CC BB IR--FUN-CONVENTION:HABU IR-BUILD:SET-CONVENTION
   CC BB  NAME-ST NAME-LN SPN  IR-BUILD:SET-FUN-SPAN
   CC BB IR-BUILD:BEGIN-BLOCK
   CC BB  OPEN-ST OPEN-LN SPN  IR-BUILD:SET-BLOCK-SPAN ;

: ARG+ ( -- IR-ID:ir-value-id )
   CC BB CELLT IR-BUILD:ADD-BLOCK-ARG ;

: CLOSE-FUN ( -- )
   CC BB IR-BUILD:END-BLOCK drop
   CC BB IR-BUILD:END-FUN drop ;

: HIR-MOD ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   IR-BUILD:PLAN-BEGIN
   IR-BUILD:PLAN-DEFAULT
   c HIR:NEW-BUILDER {: b:IR-BUILD:builder :}
   c b HIR:REGISTER
   c 0 W-CTX !
   b 0 W-BLD !
   c b TXT TXT-N IR-BUILD:ADD-SOURCE 0 W-SRC ! ;

: OPEN-OP ( HIR:opcode n n -- )
   {: o:HIR:opcode st:n ln:n :}
   CC BB  CC BB o HIR:OPCODE  IR-BUILD:BEGIN-OP
   CC BB  st ln SPN  IR-BUILD:SET-OP-SPAN ;

: BLOCK-ID ( n -- IR-ID:ir-block-id )
   {: k:n :}
   BB IR-BUILD:MODULE-KEY k IR-ID:PACK-BLOCK ;

: BLOCK+ ( -- )
   CC BB IR-BUILD:END-BLOCK drop
   CC BB IR-BUILD:BEGIN-BLOCK
   CC BB  OPEN-ST OPEN-LN SPN  IR-BUILD:SET-BLOCK-SPAN ;

: KINDOP ( n n -- IR-ID:ir-value-id )
   {: v:n kind:n :}
   HIR-OPCODE:CONST BODY-ST BODY-LN OPEN-OP
   CC BB CELLT IR-BUILD:ADD-RESULT
   CC BB  CC BB HIR:KEY-VALUE  CC BB v IR-BUILD:INTERN-INT-ATTR
   IR-BUILD:ADD-ATTR
   CC BB  CC BB HIR:KEY-ADDR  CC BB kind HIR:ADDR-ATTR
   IR-BUILD:ADD-ATTR
   CC BB IR-BUILD:END-OP {: id:IR-ID:ir-op-id :}
   CC BB id 0 IR-BUILD:OP-RESULT@ ;

: CONSTOP ( n -- IR-ID:ir-value-id )
   HIR:ADDR-NONE KINDOP ;

: DOUBLED ( IR-ID:ir-value-id -- IR-ID:ir-value-id )
   {: a:IR-ID:ir-value-id :}
   HIR-OPCODE:ADD BODY-ST BODY-LN OPEN-OP
   CC BB a IR-BUILD:ADD-OPERAND
   CC BB a IR-BUILD:ADD-OPERAND
   CC BB CELLT IR-BUILD:ADD-RESULT
   CC BB IR-BUILD:END-OP {: id:IR-ID:ir-op-id :}
   CC BB id 0 IR-BUILD:OP-RESULT@ ;

: BRZ2 ( IR-ID:ir-value-id n n -- )
   {: v:IR-ID:ir-value-id z:n o:n :}
   HIR-OPCODE:BRZ BODY-ST BODY-LN OPEN-OP
   CC BB v IR-BUILD:ADD-OPERAND
   CC BB z BLOCK-ID IR-BUILD:ADD-SUCCESSOR
   CC BB o BLOCK-ID IR-BUILD:ADD-SUCCESSOR
   CC BB IR-BUILD:END-OP drop ;

: RET1 ( IR-ID:ir-value-id -- )
   {: v:IR-ID:ir-value-id :}
   HIR-OPCODE:RETURN CLOSE-ST CLOSE-LN OPEN-OP
   CC BB v IR-BUILD:ADD-OPERAND
   CC BB IR-BUILD:END-OP drop ;

: TRAP-VALUES ( n -- IR-ID:ir-value-id IR-ID:ir-value-id IR-ID:ir-value-id )
   NTRAP:MESSAGE {: a:ptr u:n rc:n :}
   a u NSTR:INTERN HIR:ADDR-DATA KINDOP
   u CONSTOP rc CONSTOP ;

: TRAP-OPERANDS ( IR-ID:ir-value-id IR-ID:ir-value-id IR-ID:ir-value-id -- )
   {: a:IR-ID:ir-value-id u:IR-ID:ir-value-id rc:IR-ID:ir-value-id :}
   CC BB a IR-BUILD:ADD-OPERAND
   CC BB u IR-BUILD:ADD-OPERAND
   CC BB rc IR-BUILD:ADD-OPERAND ;

\ The terminator carries the diagnostic address, length and code; no successor.
: TRAP1 ( n -- )
   TRAP-VALUES
   HIR-OPCODE:TRAP CLOSE-ST CLOSE-LN OPEN-OP
   TRAP-OPERANDS
   CC BB IR-BUILD:END-OP drop ;

\ The same with a successor named, which the closed operation model has to refuse
\ because the schema declares none.
: TRAP-SUCC ( n n -- )
   {: ord:n t:n :}
   ord TRAP-VALUES
   HIR-OPCODE:TRAP CLOSE-ST CLOSE-LN OPEN-OP
   TRAP-OPERANDS
   CC BB t BLOCK-ID IR-BUILD:ADD-SUCCESSOR
   CC BB IR-BUILD:END-OP drop ;

\ ---- the shapes --------------------------------------------------------------
\ One argument tested; the zero arm returns twice the argument and the other arm
\ traps. This is the shape the compiler refused before hir.trap existed - a routine
\ with a second terminator that names no successor - and the whole point of the
\ exit-block rule is that it now goes through.
: BUILD-MIXED-NAMED ( ptr u8 n n -- )
   {: p u:n ord:n :}
   p u 1 1 OPEN-FUN
   ARG+ {: a:IR-ID:ir-value-id :}
   a 1 2 BRZ2
   BLOCK+
   a DOUBLED RET1
   BLOCK+
   ord TRAP1
   CLOSE-FUN ;

: BUILD-MIXED ( n -- )
   {: ord:n :}
   s" TRP" ord BUILD-MIXED-NAMED ;

\ THE SAME TWO ARMS THE OTHER WAY ROUND, which is the shape that says the fix is
\ a rule and not an ordering. The compiler walks blocks in module order, so here the
\ TRAP is selected first and the RETURN second: whatever the trap block leaves
\ behind is what the return block would inherit if anything were inherited across
\ that boundary. Both orders have to go through, and they are the same routine.
: BUILD-SWAPPED ( n -- )
   {: ord:n :}
   s" TRW" 1 1 OPEN-FUN
   ARG+ {: a:IR-ID:ir-value-id :}
   a 1 2 BRZ2
   BLOCK+
   ord TRAP1
   BLOCK+
   a DOUBLED RET1
   CLOSE-FUN ;

\ Every path traps: the routine has NO block that hands its caller anything, and
\ it still has to allocate, accept and emit. This is the dead-path lane's
\ foundation and the fixture that pins the shape.
: BUILD-ALL-DEAD ( n -- )
   {: ord:n :}
   s" TRD" 1 1 OPEN-FUN
   ARG+ {: a:IR-ID:ir-value-id :}
   a 1 2 BRZ2
   BLOCK+
   ord TRAP1
   BLOCK+
   ord TRAP1
   CLOSE-FUN ;

\ The smallest routine that never returns: one block, one terminator, and it is
\ the trap. It is the shape the forge below publishes and calls, and the shape
\ the emitted bytes are read off, because its LAST instruction is the branch with
\ nothing after it to account for.
: BUILD-TRAP-ONLY ( ptr u8 n n -- )
   {: p u:n ord:n :}
   p u 1 1 OPEN-FUN
   ARG+ drop
   ord TRAP1
   CLOSE-FUN ;

\ A routine of no arguments and no results whose two arms both trap. It is here
\ for the PLACEMENT survey and not for the terminator: the pointer stands at one
\ place for the whole routine, the entry wants it where the caller left it and
\ each trap site wants it one cell up, so the two trap sites outvote the entry
\ and the routine pays ONE adjustment instead of two. A survey that did not count
\ trap sites would pick the other place and emit the other number, which is what
\ the case below measures.
: BUILD-DEAD-VOID ( n -- )
   {: ord:n :}
   s" TRV" 0 0 OPEN-FUN
   0 CONSTOP 1 2 BRZ2
   BLOCK+
   ord TRAP1
   BLOCK+
   ord TRAP1
   CLOSE-FUN ;

: BUILD-TRAP-SUCC ( n -- )
   {: ord:n :}
   s" TRS" 1 1 OPEN-FUN
   ARG+ {: a:IR-ID:ir-value-id :}
   a 1 2 BRZ2
   BLOCK+
   a DOUBLED RET1
   BLOCK+
   ord 1 TRAP-SUCC
   CLOSE-FUN ;

\ ---- running the production pipeline -----------------------------------------
\ EVERY RUN HERE DECLARES A PLACEMENT, and a trap-carrying routine is the reason.
\ The branch reaches the shared runtime routine, so its
\ displacement depends on where this routine's own bytes go - which is what
\ A64EMIT:PLACE-AT tells the emitter, and what the publication seam holds the
\ emission to afterwards. The slot named is the one the seam would claim, so the
\ number is the real one rather than a stand-in.
: PLACE ( -- )
   NPUB:NEXT-SLOT A64EMIT:PLACE-AT ;

: RUN-MIXED-BODY ( IR-CTX:ctx -- )
   HIR-MOD
   s" mfam" NTRAP:FAMILY BUILD-MIXED
   PLACE
   CC BB TXT TXT-N 0 4 1 1 NFIX:RUN-HABU ;

: RUN-MIXED ( -- )
   NFIX:BINDING [: RUN-MIXED-BODY ;] IR-CTX:WITH-CONTEXT ;

: RUN-SWAP-BODY ( IR-CTX:ctx -- )
   HIR-MOD
   s" wfam" NTRAP:FAMILY BUILD-SWAPPED
   PLACE
   CC BB TXT TXT-N 0 4 1 1 NFIX:RUN-HABU ;

: RUN-SWAP ( -- )
   NFIX:BINDING [: RUN-SWAP-BODY ;] IR-CTX:WITH-CONTEXT ;

: RUN-DEAD-BODY ( IR-CTX:ctx -- )
   HIR-MOD
   s" dfam" NTRAP:FAMILY BUILD-ALL-DEAD
   PLACE
   CC BB TXT TXT-N 0 4 1 1 NFIX:RUN-HABU ;

: RUN-DEAD ( -- )
   NFIX:BINDING [: RUN-DEAD-BODY ;] IR-CTX:WITH-CONTEXT ;

: RUN-VOID-BODY ( IR-CTX:ctx -- )
   HIR-MOD
   s" vfam" NTRAP:FAMILY BUILD-DEAD-VOID
   PLACE
   CC BB TXT TXT-N 0 4 0 0 NFIX:RUN-HABU ;

: RUN-VOID ( -- )
   NFIX:BINDING [: RUN-VOID-BODY ;] IR-CTX:WITH-CONTEXT ;

\ ---- reading the emitted branch ----------------------------------------------
\ The one place this suite decodes an instruction, and it decodes exactly one
\ field of exactly one form: the unconditional branch's signed word offset. The
\ shape check in front of it is what keeps that honest - a word that is not a `b`
\ is not asked for its displacement, so a routine that ended in something else
\ fails as that rather than as a wrong address.
\ Native compilation retires its temporary emission. Read the published record
\ for source cases; hand-built IR cases still read their sealed emission.
variable CODE-BASE
variable CODE-LEN

: RECORD-CODE ( ptr u8 n -- )
   XREF-FIND {: rec:ptr :}
   rec XREF-FOUND? 0= if s" native-trap: missing code record" 76 die then
   rec XREF-START CODE-BASE !
   rec XREF-LEN CODE-LEN ! ;

: EMISSION-CODE ( -- ) 0 CODE-BASE ! ;

: CODE-PLACEMENT ( -- n )
   CODE-BASE @ dup 0<> if exit then drop A64EMIT:PLACEMENT ;

: CODE-INSNS ( -- n )
   CODE-BASE @ 0<> if CODE-LEN @ 4 / exit then A64EMIT:INSNS ;

TRUSTED: RECORD-WORD@ ( n -- n )
   4 * CODE-BASE @ + @ $FFFFFFFF and ;

: CODE-WORD@ ( n -- n )
   CODE-BASE @ 0<> if RECORD-WORD@ exit then A64EMIT:WORD@ ;

4 constant INSN-BYTES
$FC000000 constant B-MASK
$14000000 constant B-OP
$3FFFFFF constant B-IMM
$2000000 constant B-SIGN
$4000000 constant B-SPAN
$D65F03C0 constant RET-WORD

: B-WORD? ( n -- bool )
   $7C000000 and B-OP = ;

: B-DISP ( n -- n )
   {: w:n :}
   w B-IMM and {: v:n :}
   v B-SIGN and 0<> if v B-SPAN - exit then
   v ;

\ Where the LAST instruction of the sealed emission branches to, as an absolute
\ address. The emission was measured from the placement it was given, so the
\ instruction's own address is that placement plus its position, and the branch
\ reaches that plus its displacement. This is the only reading in the suite that
\ can decide whether two separately compiled routines go to ONE routine.
: LAST-TARGET ( -- n )
   CODE-INSNS 1- {: k:n :}
   k CODE-WORD@ B-DISP {: d:n :}
   CODE-PLACEMENT  k d + INSN-BYTES *  + ;

: LAST-IS-BRANCH? ( -- bool )
   CODE-INSNS 1- CODE-WORD@ B-WORD? ;

: RETS-IN-EMISSION ( -- n )
   0
   CODE-INSNS 0 ?do
      i CODE-WORD@ RET-WORD = if 1+ then
   loop ;

\ How many instructions of the emission move the data-stack pointer. Both forms
\ are an add or a subtract of an immediate whose source and destination are that
\ one register, which is what the placement survey's answer is spent on.
$FF800000 constant ADDSUB-MASK
$91000000 constant ADDI-OP
$D1000000 constant SUBI-OP
$1F constant REG-MASK

: DMOVE-WORD? ( n -- bool )
   {: w:n :}
   w ADDSUB-MASK and ADDI-OP =  w ADDSUB-MASK and SUBI-OP =  or 0= if false exit then
   w REG-MASK and A64EFF:DSTACK-GPR <> if false exit then
   w 5 rshift REG-MASK and A64EFF:DSTACK-GPR = ;

: DMOVES-IN-EMISSION ( -- n )
   0
   CODE-INSNS 0 ?do
      i CODE-WORD@ DMOVE-WORD? if 1+ then
   loop ;

\ And how many move the MACHINE stack pointer, which is what a routine's own
\ frame costs: it is taken once and given back once. The ADD/SUB immediate form
\ names operand 31 as source and destination, where that operand is the stack
\ pointer rather than the zero register. A routine with no frame emits neither,
\ so this counting zero is what "frameless" is as bytes.
: SPMOVE-WORD? ( n -- bool )
   {: w:n :}
   w ADDSUB-MASK and ADDI-OP =  w ADDSUB-MASK and SUBI-OP =  or 0= if false exit then
   w REG-MASK and A64EFF:SP-GPR <> if false exit then
   w 5 rshift REG-MASK and A64EFF:SP-GPR = ;

\ THE OTHER WAY THE POINTER MOVES, and the one a frame that keeps only the link
\ register uses: AArch64 writes the base register back as part of the transfer,
\ so `str x30,[sp,#-16]!` is the whole of taking the frame and `ldr x30,[sp],#16`
\ the whole of giving it back. Counting only the ADD/SUB form would read such a
\ routine as frameless, which is why the count asks for BOTH spellings: what the
\ case below claims is that the pointer moves twice, not which instructions say
\ so. Mode bits 11:10 are 00 for the plain unscaled access that moves nothing,
\ 01 for post-index and 11 for pre-index, so a spill slot read off sp is not
\ one of these.
$FFE00000 constant WB-CLASS-MASK     \ the opcode bits of an unscaled 64-bit access
$F8000000 constant WB-STORE-OP
$F8400000 constant WB-LOAD-OP
$00000C00 constant WB-MODE-MASK

: WBMOVE-WORD? ( n -- bool )
   {: w:n :}
   w WB-CLASS-MASK and {: op:n :}
   op WB-STORE-OP =  op WB-LOAD-OP =  or 0= if false exit then
   w WB-MODE-MASK and 0= if false exit then
   w 5 rshift REG-MASK and A64EFF:SP-GPR = ;

: SPMOVE-ANY? ( n -- bool )
   {: w:n :}
   w SPMOVE-WORD? if true exit then
   w WBMOVE-WORD? ;

: SPMOVES-IN-EMISSION ( -- n )
   0
   CODE-INSNS 0 ?do
      i CODE-WORD@ SPMOVE-ANY? if 1+ then
   loop ;

\ ---- two routines, one target ------------------------------------------------
\ The two are compiled separately, at two DIFFERENT placements, so their branches
\ carry two different displacements. What has to be equal is where those
\ displacements land, and it has to be the address the dictionary answers for the
\ one shared routine - which is the whole of "emitted once tree-wide".
64 constant SECOND-SKEW              \ sixteen instructions past the first placement

variable T-FIRST
variable T-SECOND

: TWO-TRAPS-BODY ( IR-CTX:ctx -- )
   HIR-MOD
   s" onefam" NTRAP:FAMILY {: k:n :}
   s" TRA" k BUILD-TRAP-ONLY
   PLACE
   CC BB TXT TXT-N 0 4 1 1 NFIX:RUN-HABU
   LAST-TARGET T-FIRST ! ;

: TWO-TRAPS-SECOND-BODY ( IR-CTX:ctx -- )
   HIR-MOD
   s" twofam" NTRAP:FAMILY {: k:n :}
   s" TRB" k BUILD-TRAP-ONLY
   NPUB:NEXT-SLOT SECOND-SKEW + A64EMIT:PLACE-AT
   CC BB TXT TXT-N 0 4 1 1 NFIX:RUN-HABU
   LAST-TARGET T-SECOND ! ;

: RUN-TWO-TRAPS ( -- )
   NFIX:BINDING [: TWO-TRAPS-BODY ;] IR-CTX:WITH-CONTEXT
   NFIX:BINDING [: TWO-TRAPS-SECOND-BODY ;] IR-CTX:WITH-CONTEXT ;

: RUN-SUCC-BODY ( IR-CTX:ctx -- )
   HIR-MOD
   s" sfam" NTRAP:FAMILY BUILD-TRAP-SUCC ;

: RUN-SUCC ( -- )
   NFIX:BINDING [: RUN-SUCC-BODY ;] IR-CTX:WITH-CONTEXT ;

\ ---- running the forge in a child --------------------------------------------
\ The forge ends its process, so it cannot be run in this one. The child is this
\ same file under the argument the tail dispatches on, run through the engine
\ under test rather than through a named binary, so a candidate validation
\ measures the candidate.
$4000 constant CAP-CAP
30000 constant CHILD-MS

create OUT-BUF CAP-CAP allot
create ERR-BUF CAP-CAP allot

variable CHILD-OUT-N
variable CHILD-ERR-N
variable CHILD-RC

: CHILD-ARGV ( -- )
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   s" test/compiler/aot-mode.f" >LEN PROC-ARGV+
   s" test/compiler/native-trap-noret.f" >LEN PROC-ARGV+ ;

: CHILD-RUN ( -- )
   CHILD-ARGV
   ENGINE-CANDIDATE:PATH$ >LEN
   OUT-BUF CAP-CAP >LEN
   ERR-BUF CAP-CAP >LEN
   CHILD-MS >MS
   RUN-ARGV-CAPTURE-OUTCOME       \ ( out-len err-len outcome )
   PROC-OUTCOME>RC RC>N CHILD-RC !
   LEN>N CHILD-ERR-N !
   LEN>N CHILD-OUT-N ! ;

: CHILD-ERR$ ( -- ptr u8 n )
   ERR-BUF CHILD-ERR-N @ ;

\ ---- the other kind of row, and the other exit -------------------------------
\ A trap site the ELABORATOR builds after a call that does not come back carries
\ a row of the second kind, and reaching it would mean the certificate the caller
\ was compiled against was false. It says so in its own words and with its own
\ status - the diagnostic of a scrutinee whose tag matched no arm would be a lie
\ about a word that returned - so the two are proved to differ in BOTH, in a
\ process that dies of it.
: NORET-CASE ( -- )
   s" the two kinds are two rows even under one name" T-LABEL
   s" ntrapz" NTRAP:FAMILY {: f:n :}
   s" ntrapz" NTRAP:NO-RETURN {: d:n :}
   f d T<>
   f NTRAP:KIND@ NTRAP:TAG T=
   d NTRAP:KIND@ NTRAP:NO-RET T=
   f NTRAP:NAME$ s" ntrapz" T$=
   d NTRAP:NAME$ s" ntrapz" T$=

   s" and each kind is idempotent under its own name" T-LABEL
   f  s" ntrapz" NTRAP:FAMILY  T=
   d  s" ntrapz" NTRAP:NO-RETURN  T=

   s" a name this table cannot hold is refused for either kind" T-LABEL
   [: s" " NTRAP:NO-RETURN drop ;] E-NTRAP-NAME TTHROWSQ

   s" the no-return exit is ENGINE-ERROR:CODE-CERT, not the bad-tag one" T-LABEL
   CHILD-RUN
   CHILD-RC @ ENGINE-ERROR:CODE-CERT T=

   s" and the diagnostic names the word that came back" T-LABEL
   CHILD-ERR$ s" hb: ntrapy returned" CONTAINS? TTRUE

   s" and says nothing about a tag" T-LABEL
   CHILD-ERR$ s" tag" CONTAINS? TFALSE ;

\ ---- what a compiled all-dead routine is as bytes ----------------------------
\ The contract says no frame and no saved return address; this is that read off
\ the emission the compilation sealed. A routine's own frame costs one instruction
\ that moves the machine stack pointer at each end, and this one moves it
\ nowhere - which is what dropping the reserve and the link save IS - while the
\ data-stack pointer still moves, so a routine that emitted nothing at all
\ would not pass either.

: COMPILED-DEAD-BYTES-CASE ( -- )
   s" : NTB ( n -- ) drop E-A-EMPTY throw ;" EV
   s" NTB" RECORD-CODE

   s" a compiled all-dead routine moves the machine stack pointer nowhere"
   T-LABEL
   SPMOVES-IN-EMISSION 0 T=

   s" and carries no return at all" T-LABEL
   RETS-IN-EMISSION 0 T=

   s" while it still enters through the caller's data stack" T-LABEL
   DMOVES-IN-EMISSION 0 T<>

   s" and it leaves by branching to the shared trap routine" T-LABEL
   LAST-IS-BRANCH? TTRUE
   LAST-TARGET  NTRAP:ROUTINE$ NDICT:CALL-TARGET  T=

   EMISSION-CODE ;

\ The same routine's calling sibling, which is the contrast that makes the case
\ above say something: a body that calls and DOES come back takes a frame and
\ gives it back, so its emission moves the machine stack pointer exactly twice --
\ in the two writeback transfers that save and restore the link register.
\ `abs` is an external primitive, so this really is a call.
: COMPILED-CALL-BYTES-CASE ( -- )
   s" : NTC ( n -- n ) abs 1 + ;" EV
   s" NTC" RECORD-CODE
   s" its recorded span ends immediately before the trailing return" T-LABEL
   CODE-LEN @ INSN-BYTES / CODE-WORD@ RET-WORD T=
   INSN-BYTES CODE-LEN +!

   s" a compiled routine that calls and returns moves it twice" T-LABEL
   SPMOVES-IN-EMISSION 2 T=

   s" and ends in the return the other one has nowhere for" T-LABEL
   RETS-IN-EMISSION 0 T<>

   EMISSION-CODE ;

\ The two claims about a routine with no return: the emission ends in the branch
\ that leaves, and there is no return instruction ANYWHERE in it. The second is
\ what "no epilogue" means as bytes - a routine that still gave the frame back or
\ still returned would carry one - and it is read off the emission rather than
\ inferred from the module.
: DEAD-BYTES-CASE ( -- )
   NFIX:BINDING [: RUN-DEAD-BODY ;] IR-CTX:WITH-CONTEXT

   s" a routine whose every path traps ends in the branch that leaves" T-LABEL
   LAST-IS-BRANCH? TTRUE

   s" and its emission carries no return at all" T-LABEL
   RETS-IN-EMISSION 0 T=

   s" and the branch it ends in reaches the one shared routine" T-LABEL
   LAST-TARGET  NTRAP:ROUTINE$ NDICT:CALL-TARGET  T= ;

\ The placement survey with trap sites in it. The routine takes and publishes
\ nothing, so the entry wants the pointer where the caller left it; its two trap
\ sites each want it one cell up. Two votes beat one, so the pointer stands one
\ cell up and the routine pays exactly ONE adjustment - the entry's. A survey
\ blind to trap sites would stand it where the entry wants and pay two.
: VOID-PLACE-CASE ( -- )
   s" a routine of two trap sites pays one pointer adjustment, not two" T-LABEL
   [: RUN-VOID ;] 0 TTHROWSQ
   DMOVES-IN-EMISSION 1 T= ;

: SHARED-TARGET-CASE ( -- )
   RUN-TWO-TRAPS

   s" two separately compiled traps branch to ONE address" T-LABEL
   T-FIRST @ T-SECOND @ T=

   s" and it is the address the dictionary answers for the shared routine"
   T-LABEL
   T-FIRST @  NTRAP:ROUTINE$ NDICT:CALL-TARGET  T=

   s" which the two reached from two different placements" T-LABEL
   CODE-PLACEMENT  NPUB:NEXT-SLOT  T<> ;

public

: RUN ( -- )
   T-RESET

   \ ---- the family table ----
   s" a family name answers a stable ordinal, and the same name the same one"
   T-LABEL
   s" alpha" NTRAP:FAMILY {: x:n :}
   s" beta"  NTRAP:FAMILY {: y:n :}
   x  s" alpha" NTRAP:FAMILY  T=
   y  s" beta"  NTRAP:FAMILY  T=
   x y T<>

   s" the ordinal reads back as the name it was made from" T-LABEL
   x NTRAP:NAME$ s" alpha" T$=
   y NTRAP:NAME$ s" beta"  T$=

   s" an ordinal no row holds is refused rather than named" T-LABEL
   [: NTRAP:COUNT NTRAP:NAME$ drop drop ;] E-NTRAP-ORD TTHROWSQ
   [: -1 NTRAP:NAME$ drop drop ;] E-NTRAP-ORD TTHROWSQ

   s" a name this table cannot hold is refused rather than truncated" T-LABEL
   [: s" " NTRAP:FAMILY drop ;] E-NTRAP-NAME TTHROWSQ

   \ ---- the terminator through the production pipeline ----
   s" a routine that returns AND traps goes through production" T-LABEL
   [: RUN-MIXED ;] 0 TTHROWSQ

   s" and the same two arms with the trap block selected first" T-LABEL
   [: RUN-SWAP ;] 0 TTHROWSQ

   s" a routine whose every path traps publishes too" T-LABEL
   [: RUN-DEAD ;] 0 TTHROWSQ

   s" a trap that names a successor is refused by the operation model" T-LABEL
   [: RUN-SUCC ;] E-IR-OP-ARITY TTHROWSQ

   \ ---- what the emission and the placement really are ----
   DEAD-BYTES-CASE
   VOID-PLACE-CASE
   SHARED-TARGET-CASE

   \ ---- what a routine compiled from source is as bytes ----
   COMPILED-DEAD-BYTES-CASE
   COMPILED-CALL-BYTES-CASE

   \ ---- and the whole of it, in a process that dies ----
   NORET-CASE

   T-REPORT ;

;package

NTRAP-TEST:RUN
