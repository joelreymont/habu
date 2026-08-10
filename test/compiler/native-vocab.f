\ native-vocab.f - THE COMPARISON AND BITWISE VOCABULARY, COMPILED AND RUN.
\
\ Every word this suite covers is new to the chain: `>`, `>=`, `<>`, `0=`, `and`,
\ `or`, `xor`, `lshift`, `rshift`, `invert`, `cells` and `2drop`. Each one is put
\ through the same path a real definition takes - the engine compiles it, the
\ recording unit keeps the tape its reader filled, the elaborator turns that tape
\ into HIR, the machine stages select, allocate, validate and emit, and the bytes
\ are published into code space and CALLED - and the answer the compiled routine
\ gives is compared with the answer the interpreted word gives on the same
\ arguments.
\
\ WHY THE INTERPRETED WORD IS THE ORACLE AND NOT A TABLE OF EXPECTED NUMBERS. A
\ table can only disagree with a lowering that CHANGED; it cannot disagree with
\ one that was always wrong, because whoever wrote the table and whoever wrote
\ the lowering can be wrong about the same word. So every row here records the
\ compiled answer AND the engine's answer, and both are asserted against the same
\ literal: a lowering that computes something else has to make the engine compute
\ it too before this suite goes green.
\
\ AND WHY THE ARGUMENTS ARE THE ONES THEY ARE. Each word is exercised where its
\ lowering could be wrong and nowhere else would show it:
\   `>` and `>=`   both orders of an unequal pair and a negative pair, because a
\                  lowering that reached them by turning `<`'s operands round
\                  answers the complement on every row;
\   `0=`           a flag, a value that is neither a flag nor zero, and zero,
\                  because the engine's `0=` is an equality against zero and NOT
\                  a complement of a flag - it answers false for 5 as well as for
\                  -1;
\   `lshift`       counts of 0, 63, 64 and 65, because the machine's shift-by-
\                  register form takes the count modulo the register width and so
\                  does the engine's own primitive: 1 shifted by 64 is 1;
\   `invert`       0 and -1, the two values whose complement is the other one;
\   `cells`        a negative argument, because the engine shifts left by three
\                  and this chain multiplies by eight, and a sign-extending shift
\                  would be a different function.
\
\ THE FUSION IS CHECKED BY EXECUTION AND BY THE CONDITION FIELD. A comparison
\ standing right above the branch that tests it becomes one compare-and-branch,
\ and which way round its two successors go is the one thing that fusion has to
\ get right. Three cases below run a two-way branch under each new condition and
\ compare the answer with the interpreted word, and each also reads the condition
\ out of the emitted conditional branch - so a fused branch encoded under the
\ complement of its own relation reddens on the field as well as on the answer. A
\ fourth runs a LOOP whose exit test is a fused `>`, where a reversed polarity is
\ not a wrong answer but a routine that never leaves.
\
\ THE TWO EXEMPLARS AT THE END ARE THE SURVEY'S. `TAG` is the checker's own
\ `7 and` and `WS?` is the JSON reader's `or`-chain of equalities against four
\ named constants - the two words the hot-word survey named as what this leaf is
\ for. `WS?` is compiled against constants the engine really defines, whose
\ values the word model is told exactly as it is told a `create`d data word's
\ address, so the whole shape - a `constant` mentioned in a body - is exercised
\ rather than inlined away.

require lib/test.f
require src/compiler/native/feed.f
require src/compiler/native/elaborate.f
require test/compiler/native-chain-fixture.f
require test/compiler/native-run-fixture.f

package NVOCAB-TEST
\ The ARM64 encoders are package A64ASM's public surface (src/arch/arm64/asm.f).
using A64ASM
private

\ ---- the boundaries this suite needs -----------------------------------------
\ `evaluate` is the metaprogramming boundary the checker does not model and the
\ only way to put a definition through the real compile path from inside a test.
TRUSTED: EV ( ptr u8 n -- ) evaluate ;
TRUSTED: EV-N ( ptr u8 n -- n ) evaluate ;

\ ---- what one run parks ------------------------------------------------------
\ A quotation cannot read the enclosing word's locals and every run is entered
\ through one, so everything a run needs is parked here first. That is also what
\ lets the whole prelude of a case - build the module, record the tape,
\ elaborate, select, allocate, emit - be ONE word that every case calls, instead
\ of a copy per fixture.
here CELL 1- and CELL swap - CELL 1- and allot
1 TYPED-BUFFER R-CTX IR-CTX:ctx
1 TYPED-BUFFER R-BLD IR-BUILD:builder
1 TYPED-BUFFER R-TAPE IR-ARENA:view
variable R-IN                        \ the definition's declared input count
variable R-OUT                       \ and its output count
variable R-EXTRA                     \ word-model rows beyond the dialect's own

: CC ( -- IR-CTX:ctx )           0 R-CTX @ ;
: BB ( -- IR-BUILD:builder )     0 R-BLD @ ;
: TAPE ( -- IR-ARENA:view )      0 R-TAPE @ ;

\ The source text of the definition being compiled, copied here so that one
\ recording word serves every case.
256 constant SRC-CAP
create SBUF SRC-CAP allot
variable SBUF-U

: SRC! ( ptr u8 n -- )
   {: a u:n :} \ typed-local-lint: allow-bare-local - a keeps the ptr u8 byte-span role
   u SRC-CAP > if E-NSRC-CAP throw then
   0 begin dup u < while
      dup a + c@  over SBUF + c!
      1+
   repeat drop
   u SBUF-U ! ;

: SRC ( -- ptr u8 n )
   SBUF SBUF-U @ ;

\ The buffer the recorded definition's text is kept in. The producer copies the
\ reader's text here as the scan opens; instruction selection reads it back and
\ refuses it unless it digests to the source the HIR module was compiled from.
256 constant TEXT-CAP
create TXT TEXT-CAP allot

64 constant TAPE-CAP
8 constant REGS                      \ enough for the deepest body here

\ ---- the stages, in the order they run ---------------------------------------
: HIR-MOD ( IR-CTX:ctx -- IR-BUILD:builder )
   {: c:IR-CTX:ctx :}
   IR-BUILD:PLAN-BEGIN
   IR-BUILD:PLAN-DEFAULT
   c HIR:NEW-BUILDER {: b:IR-BUILD:builder :}
   c b HIR:REGISTER
   b ;

\ The word model, committed to the dialect's own vocabulary plus whatever extra
\ rows the case declared it would add. A case that names a `constant` in its body
\ raises R-EXTRA and fills those rows itself.
: MODEL ( -- IR-ARENA:arena IR-ARENA:arena )
   CC BB IR-BUILD:MODULE-KEY
   HIR-WORD:WORDS R-EXTRA @ +  HIR-WORD:PICK-CELLS  HIR-WORD:NEW
   {: p:IR-ARENA:arena r:IR-ARENA:arena :}
   CC BB p r HIR-WORD:REGISTER-WORDS
   p r ;

\ Compile the parked source through the production path with a recording unit
\ open, and park the sealed tape.
: RECORD ( -- )
   CC BB IR-BUILD:MODULE-KEY TAPE-CAP NTAPE:NEW {: tp:IR-ARENA:arena :}
   CC BB tp TXT TEXT-CAP NFEED:BEGIN-UNIT
   SRC EV
   NFEED:END-UNIT drop  0 R-TAPE ! ;

\ How many bytes the reader handed over, read off the LIVE builder because the
\ text has to be presented to instruction selection and selection takes its
\ binding before the module freezes.
: TEXT-LEN ( -- n )
   CC BB  TAPE BB IR-BUILD:MODULE-KEY 0 NTAPE:SPAN@ IR-SOURCE:SPAN-SRC
   IR-BUILD:SOURCE-LEN ;

\ The whole prelude of a case: the module, the word model, the recorded tape, the
\ elaboration and the four machine stages. Every case below starts with it, so no
\ case can drive the chain differently from its neighbour.
\
\ THE ONE SEAM WHERE ARITY ENTERS is the same one test/compiler/native-chain.f
\ names: the declared effect is stated by the caller rather than read off the
\ checker that just accepted it, until dot habu-bind-checker-env-ed4f9f87 lands.
: PREP ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 0 R-CTX !
   c HIR-MOD 0 R-BLD !
   MODEL {: p:IR-ARENA:arena r:IR-ARENA:arena :}
   RECORD
   CC BB TAPE p r R-IN @ R-OUT @ NELAB:COLON drop
   CC BB TXT TEXT-LEN 0 REGS R-IN @ R-OUT @ NFIX:RUN-HABU ;

\ What a case declares before it runs: the source, and the effect the definition
\ carries. R-EXTRA is set back to zero here, so a case that needs extra word-model
\ rows raises it AFTER this and no case inherits its neighbour's.
: CASE! ( ptr u8 n n n -- )
   {: a u:n in:n out:n :} \ typed-local-lint: allow-bare-local - a keeps the ptr u8 byte-span role
   a u SRC!
   in R-IN !
   out R-OUT !
   0 R-EXTRA ! ;

\ ---- reading the emitted instructions ----------------------------------------
\ A conditional branch, found by its own form rather than at an index: which
\ instruction it lands at depends on the register allocation and the block
\ layout, and neither is what these cases are about. The form is the B.cond
\ encoding with its displacement field and its condition cleared.
$FF000010 constant BCOND-MASK
$54000000 constant BCOND-FORM

: BCONDS ( -- n )
   0
   A64EMIT:INSNS 0 ?do
      i A64EMIT:WORD@ BCOND-MASK and BCOND-FORM = if 1+ then
   loop ;

\ The condition the last conditional branch carries, which is the four-bit field
\ at the bottom of the word. Every case that reads it asserts BCONDS is one, so
\ "the last" is "the only".
: BCOND-COND ( -- n )
   -1
   A64EMIT:INSNS 0 ?do
      i A64EMIT:WORD@ dup BCOND-MASK and BCOND-FORM =
      if $F and nip else drop then
   loop ;

\ A conditional select, found by its own form: the Csel encoding with its three
\ register fields and its condition cleared. It is what an if-conversion emits
\ where a branch used to be, so a case that reads it beside BCONDS is asserting
\ both halves of that trade at once - the branch went and the select arrived.
$FFE00C00 constant CSEL-MASK
$9A800000 constant CSEL-FORM

: CSELS ( -- n )
   0
   A64EMIT:INSNS 0 ?do
      i A64EMIT:WORD@ CSEL-MASK and CSEL-FORM = if 1+ then
   loop ;

\ The condition the last conditional select carries, in the same four-bit field
\ a conditional branch carries it in. Every case that reads it asserts CSELS is
\ one, so "the last" is "the only".
: CSEL-COND ( -- n )
   -1
   A64EMIT:INSNS 0 ?do
      i A64EMIT:WORD@ dup CSEL-MASK and CSEL-FORM =
      if 12 rshift $F and nip else drop then
   loop ;

\ The bitwise complement, found the same way: Orn with the zero register and both
\ its register fields cleared. It is the whole of what `invert` compiles to, so
\ counting it says the chain reached the one-instruction form and did not
\ materialise all-ones into a register and take an exclusive or.
$FFE0FFE0 constant MVN-SHAPE
: MVN-FORM ( -- n )
   0 0 ENC-MVN MVN-SHAPE and ;

: MVNS ( -- n )
   0
   A64EMIT:INSNS 0 ?do
      i A64EMIT:WORD@ MVN-SHAPE and MVN-FORM = if 1+ then
   loop ;

\ ---- the comparisons that materialise a flag ---------------------------------
\ `>` with both orders of an unequal pair and with a negative pair. A lowering
\ that reached greater-than by comparing the operands the other way round answers
\ the complement on every one of the three rows, and a lowering that used an
\ unsigned condition answers the wrong thing on the negative pair alone.
: GT-BODY ( IR-CTX:ctx -- n n n n n n )
   PREP
   NRUN:PUBLISH {: fn:n :}
   3 4 fn NRUN:ENTER2
   4 3 fn NRUN:ENTER2
   -1 -2 fn NRUN:ENTER2
   s" 3 4 NVC-GT" EV-N
   s" 4 3 NVC-GT" EV-N
   s" -1 -2 NVC-GT" EV-N ;

: GT-CASE ( -- )
   s" : NVC-GT ( n n -- bool ) > ;" 2 1 CASE!
   s" a greater-than materialises the flag the engine materialises" T-LABEL
   NFIX:BINDING [: GT-BODY ;] IR-CTX:WITH-CONTEXT
   -1 T= -1 T= 0 T= -1 T= -1 T= 0 T= ;

\ `>=` the same way, with the equal pair added: it is the one argument that tells
\ greater-or-equal from greater.
: GE-BODY ( IR-CTX:ctx -- n n n n n n )
   PREP
   NRUN:PUBLISH {: fn:n :}
   3 4 fn NRUN:ENTER2
   4 3 fn NRUN:ENTER2
   3 3 fn NRUN:ENTER2
   s" 3 4 NVC-GE" EV-N
   s" 4 3 NVC-GE" EV-N
   s" 3 3 NVC-GE" EV-N ;

: GE-CASE ( -- )
   s" : NVC-GE ( n n -- bool ) >= ;" 2 1 CASE!
   s" a greater-or-equal materialises the flag the engine materialises" T-LABEL
   NFIX:BINDING [: GE-BODY ;] IR-CTX:WITH-CONTEXT
   -1 T= -1 T= 0 T= -1 T= -1 T= 0 T= ;

\ `<>` cannot be reached by turning operands round at all - it is the complement
\ of equality - so the equal pair and the unequal pair between them say whether
\ the condition really was the not-equal one.
: NE-BODY ( IR-CTX:ctx -- n n n n )
   PREP
   NRUN:PUBLISH {: fn:n :}
   3 4 fn NRUN:ENTER2
   3 3 fn NRUN:ENTER2
   s" 3 4 NVC-NE" EV-N
   s" 3 3 NVC-NE" EV-N ;

: NE-CASE ( -- )
   s" : NVC-NE ( n n -- bool ) <> ;" 2 1 CASE!
   s" an inequality materialises the flag the engine materialises" T-LABEL
   NFIX:BINDING [: NE-BODY ;] IR-CTX:WITH-CONTEXT
   0 T= -1 T= 0 T= -1 T= ;

\ `0=` is an equality against ZERO and not the complement of a flag. The three
\ arguments are what says so: zero answers true, and BOTH -1 - which is a flag -
\ and 5 - which is not - answer false. A lowering that complemented its argument
\ would answer -1 for 0 and 0 for -1 and would then have to answer something for
\ 5, which is the row that pins it.
: ZEQ-BODY ( IR-CTX:ctx -- n n n n n n )
   PREP
   NRUN:PUBLISH {: fn:n :}
   0 fn NRUN:ENTER1
   -1 fn NRUN:ENTER1
   5 fn NRUN:ENTER1
   s" 0 NVC-ZEQ" EV-N
   s" -1 NVC-ZEQ" EV-N
   s" 5 NVC-ZEQ" EV-N ;

: ZEQ-CASE ( -- )
   s" : NVC-ZEQ ( n -- bool ) 0= ;" 1 1 CASE!
   s" 0= answers false for every nonzero value, flag or not" T-LABEL
   NFIX:BINDING [: ZEQ-BODY ;] IR-CTX:WITH-CONTEXT
   0 T= 0 T= -1 T= 0 T= 0 T= -1 T= ;

\ ---- the comparisons that fuse into a select ---------------------------------
\ The same three relations, each standing immediately above the `if` that tests
\ it. Both arms of each are a single constant, so the whole selection is
\ if-converted and what the comparison fuses into is a machine SELECT rather
\ than a branch (src/compiler/native/select.f). Each body answers 1 on the arm
\ the relation chose and 0 on the other, so a select whose two sources went the
\ wrong way round answers the wrong number on every row - and the three argument
\ pairs are chosen so that the EQUAL pair tells greater-or-equal from greater
\ and not-equal from equal.
\
\ AND THE CONDITION IS READ OUT OF THE EMITTED SELECT AS WELL. Two relations can
\ agree on any finite set of arguments; what says which one was really encoded is
\ the four-bit field the Csel carries - the same field a conditional branch
\ carries it in, read through the same table - and it is asserted against the
\ assembler's own name for that condition rather than a number written here.
\ There is exactly one select in each of these routines and no conditional
\ branch at all, which is what makes "the field" a well-posed question and is
\ also the whole of what the conversion did to these bodies. The fused BRANCH is
\ still reached by every loop, which is what GTL-CASE below is: a loop's back
\ edge is the one thing an if-conversion may never speculate over.
: GTF-BODY ( IR-CTX:ctx -- n n n n n n n n n )
   PREP
   BCONDS
   CSELS
   CSEL-COND
   NRUN:PUBLISH {: fn:n :}
   4 3 fn NRUN:ENTER2
   3 3 fn NRUN:ENTER2
   3 4 fn NRUN:ENTER2
   s" 4 3 NVC-GTF" EV-N
   s" 3 3 NVC-GTF" EV-N
   s" 3 4 NVC-GTF" EV-N ;

: GTF-CASE ( -- )
   s" : NVC-GTF ( n n -- n ) 2dup > if 2drop 1 exit then 2drop 0 ;" 2 1 CASE!
   s" a greater-than fuses into its select under the greater-than condition"
   T-LABEL
   NFIX:BINDING [: GTF-BODY ;] IR-CTX:WITH-CONTEXT
   0 T= 0 T= 1 T= 0 T= 0 T= 1 T=
   C-GT T= 1 T= 0 T= ;

: GEF-BODY ( IR-CTX:ctx -- n n n n n n n n n )
   PREP
   BCONDS
   CSELS
   CSEL-COND
   NRUN:PUBLISH {: fn:n :}
   4 3 fn NRUN:ENTER2
   3 3 fn NRUN:ENTER2
   3 4 fn NRUN:ENTER2
   s" 4 3 NVC-GEF" EV-N
   s" 3 3 NVC-GEF" EV-N
   s" 3 4 NVC-GEF" EV-N ;

: GEF-CASE ( -- )
   s" : NVC-GEF ( n n -- n ) 2dup >= if 2drop 1 exit then 2drop 0 ;" 2 1 CASE!
   s" a greater-or-equal fuses into its select under its own condition" T-LABEL
   NFIX:BINDING [: GEF-BODY ;] IR-CTX:WITH-CONTEXT
   0 T= 1 T= 1 T= 0 T= 1 T= 1 T=
   C-GE T= 1 T= 0 T= ;

: NEF-BODY ( IR-CTX:ctx -- n n n n n n n n n )
   PREP
   BCONDS
   CSELS
   CSEL-COND
   NRUN:PUBLISH {: fn:n :}
   4 3 fn NRUN:ENTER2
   3 3 fn NRUN:ENTER2
   3 4 fn NRUN:ENTER2
   s" 4 3 NVC-NEF" EV-N
   s" 3 3 NVC-NEF" EV-N
   s" 3 4 NVC-NEF" EV-N ;

: NEF-CASE ( -- )
   s" : NVC-NEF ( n n -- n ) 2dup <> if 2drop 1 exit then 2drop 0 ;" 2 1 CASE!
   s" an inequality fuses into its select under the not-equal condition" T-LABEL
   NFIX:BINDING [: NEF-BODY ;] IR-CTX:WITH-CONTEXT
   1 T= 0 T= 1 T= 1 T= 0 T= 1 T=
   C-NE T= 1 T= 0 T= ;

\ ---- a loop whose exit test is a fused comparison ----------------------------
\ `begin 1+ dup 3 > until` counts up until it is past three, so the relation is
\ what decides whether the routine leaves at all. A fused branch whose successors
\ went the wrong way round does not answer a different number here: it either
\ leaves on the first turn - answering one more than it was given - or never
\ leaves. Both are caught, the first by the answer and the second by the process
\ not finishing.
: GTL-BODY ( IR-CTX:ctx -- n n n n )
   PREP
   NRUN:PUBLISH {: fn:n :}
   0 fn NRUN:ENTER1
   10 fn NRUN:ENTER1
   s" 0 NVC-GTL" EV-N
   s" 10 NVC-GTL" EV-N ;

: GTL-CASE ( -- )
   s" : NVC-GTL ( n -- n ) begin 1+ dup 3 > until ;" 1 1 CASE!
   s" a loop whose exit test is a fused greater-than terminates" T-LABEL
   NFIX:BINDING [: GTL-BODY ;] IR-CTX:WITH-CONTEXT
   11 T= 4 T= 11 T= 4 T= ;

\ ---- the bitwise words -------------------------------------------------------
\ `and`, `or` and `xor` over a pair whose four bit patterns differ in every
\ position that could tell one from another: 12 is 1100 and 10 is 1010, so the
\ three answers are 8, 14 and 6 and no two of them agree.
: BITS-BODY ( IR-CTX:ctx -- n n n n n n )
   PREP
   NRUN:PUBLISH {: fn:n :}
   12 10 fn NRUN:ENTER2
   -1 0 fn NRUN:ENTER2
   -1 -1 fn NRUN:ENTER2
   s" 12 10 NVC-AND" EV-N
   s" -1 0 NVC-AND" EV-N
   s" -1 -1 NVC-AND" EV-N ;

: AND-CASE ( -- )
   s" : NVC-AND ( n n -- n ) and ;" 2 1 CASE!
   s" and combines its two arguments bit for bit" T-LABEL
   NFIX:BINDING [: BITS-BODY ;] IR-CTX:WITH-CONTEXT
   -1 T= 0 T= 8 T= -1 T= 0 T= 8 T= ;

: OR-BODY ( IR-CTX:ctx -- n n n n )
   PREP
   NRUN:PUBLISH {: fn:n :}
   12 10 fn NRUN:ENTER2
   -1 0 fn NRUN:ENTER2
   s" 12 10 NVC-OR" EV-N
   s" -1 0 NVC-OR" EV-N ;

: OR-CASE ( -- )
   s" : NVC-OR ( n n -- n ) or ;" 2 1 CASE!
   s" or combines its two arguments bit for bit" T-LABEL
   NFIX:BINDING [: OR-BODY ;] IR-CTX:WITH-CONTEXT
   -1 T= 14 T= -1 T= 14 T= ;

: XOR-BODY ( IR-CTX:ctx -- n n n n )
   PREP
   NRUN:PUBLISH {: fn:n :}
   12 10 fn NRUN:ENTER2
   -1 -1 fn NRUN:ENTER2
   s" 12 10 NVC-XOR" EV-N
   s" -1 -1 NVC-XOR" EV-N ;

: XOR-CASE ( -- )
   s" : NVC-XOR ( n n -- n ) xor ;" 2 1 CASE!
   s" xor combines its two arguments bit for bit" T-LABEL
   NFIX:BINDING [: XOR-BODY ;] IR-CTX:WITH-CONTEXT
   0 T= 6 T= 0 T= 6 T= ;

\ ---- the shifts --------------------------------------------------------------
\ The count comes off the stack, so the machine form is the shift-BY-REGISTER one
\ and it takes the count modulo the register width. That is not a rounding this
\ chain chose: it is what the engine's own `lshift` does, because it is the same
\ instruction. The four counts are the two ends of the field and the two just
\ past it - 64 shifts by nothing at all and 65 shifts by one - and the immediate
\ form, which has no such wrap and cannot take a computed count anyway, answers
\ differently on both.
: LSH-BODY ( IR-CTX:ctx -- n n n n n n n n )
   PREP
   NRUN:PUBLISH {: fn:n :}
   1 0 fn NRUN:ENTER2
   1 63 fn NRUN:ENTER2
   1 64 fn NRUN:ENTER2
   1 65 fn NRUN:ENTER2
   s" 1 0 NVC-LSH" EV-N
   s" 1 63 NVC-LSH" EV-N
   s" 1 64 NVC-LSH" EV-N
   s" 1 65 NVC-LSH" EV-N ;

: LSH-CASE ( -- )
   s" : NVC-LSH ( n n -- n ) lshift ;" 2 1 CASE!
   s" lshift takes a computed count, modulo the register width" T-LABEL
   NFIX:BINDING [: LSH-BODY ;] IR-CTX:WITH-CONTEXT
   2 T= 1 T= -9223372036854775808 T= 1 T=
   2 T= 1 T= -9223372036854775808 T= 1 T= ;

\ `rshift` is the LOGICAL shift, which is what makes the answer for -1 a positive
\ number: an arithmetic shift of -1 is -1 whatever the count. The counts are the
\ same four, for the same reason.
: RSH-BODY ( IR-CTX:ctx -- n n n n n n n n )
   PREP
   NRUN:PUBLISH {: fn:n :}
   -1 0 fn NRUN:ENTER2
   -1 63 fn NRUN:ENTER2
   -1 64 fn NRUN:ENTER2
   -1 1 fn NRUN:ENTER2
   s" -1 0 NVC-RSH" EV-N
   s" -1 63 NVC-RSH" EV-N
   s" -1 64 NVC-RSH" EV-N
   s" -1 1 NVC-RSH" EV-N ;

: RSH-CASE ( -- )
   s" : NVC-RSH ( n n -- n ) rshift ;" 2 1 CASE!
   s" rshift is logical and takes a computed count" T-LABEL
   NFIX:BINDING [: RSH-BODY ;] IR-CTX:WITH-CONTEXT
   9223372036854775807 T= -1 T= 1 T= -1 T=
   9223372036854775807 T= -1 T= 1 T= -1 T= ;

\ ---- the complement ----------------------------------------------------------
\ `invert` of 0 is -1 and of -1 is 0, and of 5 is -6. It compiles to ONE
\ instruction - the complement form - which is what the count below says: a
\ lowering that reached the same answer by moving all-ones into a register and
\ taking an exclusive or would emit none of them and four move-wides besides.
: INV-BODY ( IR-CTX:ctx -- n n n n n n n )
   PREP
   MVNS
   NRUN:PUBLISH {: fn:n :}
   0 fn NRUN:ENTER1
   -1 fn NRUN:ENTER1
   5 fn NRUN:ENTER1
   s" 0 NVC-INV" EV-N
   s" -1 NVC-INV" EV-N
   s" 5 NVC-INV" EV-N ;

: INV-CASE ( -- )
   s" : NVC-INV ( n -- n ) invert ;" 1 1 CASE!
   s" invert is one complement instruction and answers what the engine answers"
   T-LABEL
   NFIX:BINDING [: INV-BODY ;] IR-CTX:WITH-CONTEXT
   -6 T= 0 T= -1 T= -6 T= 0 T= -1 T=
   1 T= ;

\ ---- the two rows that are one literal and one rename ------------------------
\ `cells` is a multiplication by eight where the engine is a shift left by three.
\ The negative argument is what says the two agree: a shift that sign-extended,
\ or a multiplication by some other width, answers something else for -3.
: CELLS-BODY ( IR-CTX:ctx -- n n n n n n )
   PREP
   NRUN:PUBLISH {: fn:n :}
   0 fn NRUN:ENTER1
   3 fn NRUN:ENTER1
   -3 fn NRUN:ENTER1
   s" 0 NVC-CELLS" EV-N
   s" 3 NVC-CELLS" EV-N
   s" -3 NVC-CELLS" EV-N ;

: CELLS-CASE ( -- )
   s" : NVC-CELLS ( n -- n ) cells ;" 1 1 CASE!
   s" cells multiplies by eight, negative arguments included" T-LABEL
   NFIX:BINDING [: CELLS-BODY ;] IR-CTX:WITH-CONTEXT
   -24 T= 24 T= 0 T= -24 T= 24 T= 0 T= ;

\ `2drop` produces no operation at all, so what has to be right is WHICH two
\ values leave: the three arguments are distinct, so a rename that dropped the
\ wrong pair answers 2 or 3 instead of 1.
: DROP2-BODY ( IR-CTX:ctx -- n n )
   PREP
   NRUN:PUBLISH {: fn:n :}
   1 2 3 fn NRUN:ENTER3
   s" 1 2 3 NVC-2DROP" EV-N ;

: DROP2-CASE ( -- )
   s" : NVC-2DROP ( n n n -- n ) 2drop ;" 3 1 CASE!
   s" 2drop leaves the value under the two it consumed" T-LABEL
   NFIX:BINDING [: DROP2-BODY ;] IR-CTX:WITH-CONTEXT
   1 T= 1 T= ;

\ ---- the survey's first exemplar: the checker's TAG --------------------------
\ `7 and` is src/core/checker.f's tag extractor and the single commonest shape
\ the hot-word survey found. The three arguments cover a value whose low bits are
\ all set, one whose low bits are all clear, and one in between.
: TAG-BODY ( IR-CTX:ctx -- n n n n n n )
   PREP
   NRUN:PUBLISH {: fn:n :}
   255 fn NRUN:ENTER1
   8 fn NRUN:ENTER1
   13 fn NRUN:ENTER1
   s" 255 NVC-TAG" EV-N
   s" 8 NVC-TAG" EV-N
   s" 13 NVC-TAG" EV-N ;

: TAG-CASE ( -- )
   s" : NVC-TAG ( n -- n ) 7 and ;" 1 1 CASE!
   s" the checker's tag extractor compiles and runs" T-LABEL
   NFIX:BINDING [: TAG-BODY ;] IR-CTX:WITH-CONTEXT
   5 T= 0 T= 7 T= 5 T= 0 T= 7 T= ;

\ ---- the survey's second exemplar: the JSON reader's WS? ---------------------
\ `dup SP = over TAB = or over LF = or swap CR = or` is lib/json-read.f's
\ whitespace test, and it is the exemplar because of what it needs at once: four
\ equalities, three `or`s, three stack renames and four named CONSTANTS.
\
\ THE CONSTANTS ARE THE ENGINE'S, WHICH IS THE POINT. Each one is created by
\ evaluating a `constant` declaration through the same front end the definition
\ goes through, and the word model is then given its NAME. Nothing here writes a
\ number down and nothing here reads one back either: the model asks the engine
\ what each name denotes, exactly as it does for a `create`d data word's address.
: WS-CONSTS ( -- )
   s" 32 constant NVC-SP" EV
   s" 9 constant NVC-TAB" EV
   s" 10 constant NVC-LF" EV
   s" 13 constant NVC-CR" EV ;

: WS-FIXED ( IR-ARENA:arena ptr u8 n -- )
   {: r:IR-ARENA:arena a u:n :} \ typed-local-lint: allow-bare-local - a keeps the ptr u8 byte-span role
   CC BB r  CC BB a u IR-BUILD:INTERN-SYMBOL  HIR-WORD:DECLARE-FIXED ;

\ The four rows the body's constants need, on top of the dialect's vocabulary.
\ MODEL already committed the table to them, because the case raised R-EXTRA.
: WS-MODEL ( -- IR-ARENA:arena IR-ARENA:arena )
   MODEL {: p:IR-ARENA:arena r:IR-ARENA:arena :}
   r s" NVC-SP"  WS-FIXED
   r s" NVC-TAB" WS-FIXED
   r s" NVC-LF"  WS-FIXED
   r s" NVC-CR"  WS-FIXED
   p r ;

\ The same prelude PREP runs, with the constant-carrying model in place of the
\ plain one. It is written out rather than parameterised because a quotation
\ cannot carry a word to call.
: WS-PREP ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 0 R-CTX !
   c HIR-MOD 0 R-BLD !
   WS-MODEL {: p:IR-ARENA:arena r:IR-ARENA:arena :}
   RECORD
   CC BB TAPE p r R-IN @ R-OUT @ NELAB:COLON drop
   CC BB TXT TEXT-LEN 0 REGS R-IN @ R-OUT @ NFIX:RUN-HABU ;

: WS-BODY ( IR-CTX:ctx -- n n n n n n n n n n )
   WS-PREP
   NRUN:PUBLISH {: fn:n :}
   32 fn NRUN:ENTER1
   9 fn NRUN:ENTER1
   10 fn NRUN:ENTER1
   13 fn NRUN:ENTER1
   65 fn NRUN:ENTER1
   s" 32 NVC-WS" EV-N
   s" 13 NVC-WS" EV-N
   s" 65 NVC-WS" EV-N
   s" 0 NVC-WS" EV-N
   s" 33 NVC-WS" EV-N ;

: WS-CASE ( -- )
   s" : NVC-WS ( n -- bool ) dup NVC-SP = over NVC-TAB = or over NVC-LF = or swap NVC-CR = or ;"
   1 1 CASE!
   4 R-EXTRA !
   s" the JSON reader's whitespace test compiles and runs" T-LABEL
   NFIX:BINDING [: WS-BODY ;] IR-CTX:WITH-CONTEXT
   0 T= 0 T= 0 T= -1 T= -1 T=
   0 T= -1 T= -1 T= -1 T= -1 T= ;

public

: RUN ( -- )
   T-RESET
   GT-CASE
   GE-CASE
   NE-CASE
   ZEQ-CASE
   GTF-CASE
   GEF-CASE
   NEF-CASE
   GTL-CASE
   AND-CASE
   OR-CASE
   XOR-CASE
   LSH-CASE
   RSH-CASE
   INV-CASE
   CELLS-CASE
   DROP2-CASE
   TAG-CASE
   WS-CONSTS
   WS-CASE
   T-REPORT ;

;using
;package

NVOCAB-TEST:RUN
