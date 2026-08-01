\ codegen-compare-new.f - the new code generator's column of the comparison.
\ One concern: which corpus word the new chain can compile, and what it costs.
\
\ EVERY CORPUS WORD IS ACCOUNTED FOR. A word is either compiled - the real chain
\ runs on it and it gets a row of its own - or declared a gap that names the
\ capability the straight-line subset still lacks. Nothing is skipped: the
\ coverage check below refuses a pass in which some corpus word is neither, so
\ "the new column has fewer rows" can only ever mean "these named capabilities
\ are missing", never "the harness quietly stopped looking".
\
\ WHAT THE SUBSET IS TODAY. src/compiler/native/hir-word.f declares twenty-three
\ source words - `+ - * < <=`, `1-` and `1+`, the two memory words `@` and `!`,
\ the seven structured control words `if then begin until ?do loop i`, and the
\ seven renames `2dup dup drop swap over nip rot` - plus integer literals, and
\ src/compiler/native/hir.f gives the dialect twelve operations. A word of the
\ corpus is expressible exactly when its body is those words and nothing else,
\ plus - for a body that names a `create`d data word - the one address the
\ harness states (dot habu-resolve-a-data-a1c8067f). Seven of the eleven are:
\ the empty word, the three-argument sum, the sum of two squares - which is the
\ one that shows the renames costing nothing at all - the two-way branch, both
\ loop forms, and the cell bump.
\
\ THE CAPABILITIES THE OTHER FIVE WAIT FOR are the vocabulary below, and each
\ gap row names every one it needs rather than the first that stops it - a word
\ that needs a branch and a comparison is not unblocked by branches alone, and a
\ reader planning the next capability should see that.
\
\ HOW A COVERED ROW IS CHECKED. The routine the chain emitted is published into
\ code space and CALLED on the same pinned inputs the old column used, and its
\ answers are recorded as that row's outputs. The head-to-head check is then an
\ exact comparison of the two rows' outputs: the same corpus word compiled two
\ ways has to compute the same thing, and a row where it does not is a finding
\ the run reports and exits non-zero on. Bytes are exact too.
\
\ AND IT IS CALLED THE WAY A HABU WORD IS. The routines are compiled under the
\ data-stack convention, so the pinned inputs go on the data stack and the
\ routine is entered by the same branch the interpreter uses (NRUN:ENTER0..3,
\ whose body is `execute`). The old column's word call is one branch too, so the
\ two costs are finally about the emitted code rather than about a marshalling
\ trampoline. One difference is left and is not hidden: the new column's entry
\ goes through the address on the stack rather than through an address the
\ engine compiled into the call site, which is one indirect branch more. That is
\ what each path's own calibration row measures, and the report prints the
\ absolute nanoseconds of both empty calls so a reader can subtract it.
\
\ ONE CONTEXT PER WORD. A module holds about seventeen arenas and the live
\ registry holds sixty-four, and a covered word builds a source module, a word
\ model, an immediate table, a tape and a machine module. Each therefore runs
\ inside its own context, which gives its arenas back when it leaves.

require lib/errors.f
require lib/prelude.f
require lib/string.f
require tools/codegen-compare-core.f
require tools/codegen-compare-chain.f
require tools/codegen-compare-corpus.f

package CODEGEN-NEW

public

\ What a corpus word needs that the straight-line subset has not got. A gap row
\ stores these rather than a sentence, so a row that names no capability at all
\ is unwritable and the report renders every one of them the same way. In order:
\ a branch, a loop, or an exit from the middle of one; a typed locals frame;
\ calling another word, recursion included; a load or a store; an ordering or
\ equality operation; integer division.
ENUM cap DERIVE eq
   control-flow
   locals
   calls
   memory
   comparison
   division
;ENUM

private

16 constant GAP-MAX
6 constant CAP-MAX
4 constant REGS                   \ registers a straight-line corpus routine may use

\ A routine with control flow needs more, and says why. A block argument and
\ every value handed to it across an edge are one class holding one register for
\ the whole span between them, so a loop's carried values each hold a register
\ from the pre-header to the latch whether or not they are read in between. That
\ is the conservatism of the hull intervals src/compiler/native/regalloc.f
\ documents, and it is paid in registers rather than in correctness.
8 constant LOOP-REGS

GAP-MAX CODEGEN-COMPARE:NAME-MAX * BUFFER: GAP-NAMES
create GAP-LENS GAP-MAX cells allot
create GAP-CAP-N GAP-MAX cells allot
create GAP-CAPS GAP-MAX CAP-MAX * cells allot

variable GAP-N

: SLOT ( ptr a n -- ptr a )
   cells + ;

: GAP-OK ( n -- n )
   dup 0 < over GAP-N @ >= or if E-CODEGEN-COMPARE-ROW throw then ;

: GAP-NAME-AT ( n -- ptr u8 )
   CODEGEN-COMPARE:NAME-MAX * GAP-NAMES + ;

\ A stored row is cells, so a capability crosses to a number here and back
\ there. The decoder is exhaustive and refuses a code outside the vocabulary at
\ first touch, so a corrupted row cannot decode as some other capability.
: CAP-CODE ( CODEGEN-NEW:cap -- n )
   MATCH cap
      control-flow OF 0 ENDOF
      locals       OF 1 ENDOF
      calls        OF 2 ENDOF
      memory       OF 3 ENDOF
      comparison   OF 4 ENDOF
      division     OF 5 ENDOF
   ;MATCH ;

: N>CAP ( n -- CODEGEN-NEW:cap )
   case
      0 of CODEGEN--NEW-CAP:CONTROL-FLOW endof
      1 of CODEGEN--NEW-CAP:LOCALS endof
      2 of CODEGEN--NEW-CAP:CALLS endof
      3 of CODEGEN--NEW-CAP:MEMORY endof
      4 of CODEGEN--NEW-CAP:COMPARISON endof
      5 of CODEGEN--NEW-CAP:DIVISION endof
      E-CODEGEN-COMPARE-ROW throw
   endcase ;

\ Every name this file writes down has to be a corpus word the old column really
\ measured. A misspelling would otherwise become a gap for a word that does not
\ exist, or leave a real word accounted for by nothing.
: CORPUS-CK ( ptr u8 n -- ) {: a:ptr u:n :}
   CODEGEN-COMPARE:PATH-OLD a u CODEGEN-COMPARE:FIND-ROW 0 < if
      E-CODEGEN-COMPARE-CORPUS throw
   then ;

public

\ Declare a corpus word the straight-line subset cannot express yet, and the
\ first capability it is waiting for. There is no way to declare one without a
\ capability.
: GAP ( ptr u8 n CODEGEN-NEW:cap -- ) {: a:ptr u:n c:CODEGEN-NEW:cap :}
   GAP-N @ GAP-MAX >= if E-CODEGEN-COMPARE-CAP throw then
   u CODEGEN-COMPARE:NAME-MAX > if E-CODEGEN-COMPARE-CAP throw then
   a u CORPUS-CK
   a  GAP-N @ GAP-NAME-AT  u STR-LEN BYTE-COPY-LEN
   u GAP-LENS GAP-N @ SLOT !
   c CAP-CODE GAP-CAPS GAP-N @ CAP-MAX * SLOT !
   1 GAP-CAP-N GAP-N @ SLOT !
   GAP-N @ 1+ GAP-N ! ;

\ Another capability the gap just declared is also waiting for.
: GAP-ALSO ( CODEGEN-NEW:cap -- ) {: c:CODEGEN-NEW:cap :}
   GAP-N @ 1- GAP-OK {: k:n :}
   GAP-CAP-N k SLOT @ {: j:n :}
   j CAP-MAX >= if E-CODEGEN-COMPARE-CAP throw then
   c CAP-CODE GAP-CAPS k CAP-MAX * j + SLOT !
   j 1+ GAP-CAP-N k SLOT ! ;

: GAPS ( -- n )
   GAP-N @ ;

: GAP-NAME$ ( n -- ptr u8 n ) {: k:n :}
   k GAP-OK GAP-NAME-AT
   GAP-LENS k SLOT @ ;

: GAP-CAPS@ ( n -- n ) {: k:n :}
   GAP-CAP-N k GAP-OK SLOT @ ;

: GAP-CAP@ ( n n -- CODEGEN-NEW:cap ) {: k:n j:n :}
   k GAP-OK drop
   j 0 < j k GAP-CAPS@ >= or if E-CODEGEN-COMPARE-ROW throw then
   GAP-CAPS k CAP-MAX * j + SLOT @ N>CAP ;

\ How a capability reads in the report. The one place a capability becomes text.
: CAP$ ( CODEGEN-NEW:cap -- ptr u8 n ) {: c:CODEGEN-NEW:cap :}
   c MATCH cap
      control-flow OF s" control flow" ENDOF
      locals       OF s" locals" ENDOF
      calls        OF s" calls" ENDOF
      memory       OF s" memory access" ENDOF
      comparison   OF s" comparison" ENDOF
      division     OF s" division" ENDOF
   ;MATCH ;

private

\ ---- the covered words -------------------------------------------------------
\ Each one states the corpus body as the subset spells it, the declared stack
\ effect the elaborator is still handed as two counts, and the pinned inputs the
\ old column used. Nothing below catches: a refusal here means this file claimed
\ a word was expressible when it is not.
\
\ The line is the definition WITHOUT its frame - `ADD3 + +`, not `: ADD3 + + ;` -
\ because that is the shape a real tape has: the engine consumes the opening `:`
\ and the closing `;` before the checker's reader sees a token, so a produced tape
\ carries no frame row and the elaborator reads the name/body boundary off the
\ recorded parser mode. The comment above each body shows the corpus word as it
\ is really written, so the two can still be held side by side.

\ `: NOOP ( -- ) ;` - the calibration row. It returns nothing, so it has no
\ result register to check and no output to compare; what it measures is the
\ floor of a call on this path, which every other new row is divided by.
: NOOP-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   s" NOOP" NSRC:TEXT!
   c 0 0 REGS CODEGEN-CHAIN:CHAIN
   CODEGEN-CHAIN:PUBLISH!
   s" CODEGEN-CORPUS:NOOP" CODEGEN-CHAIN:BYTES
   [: CODEGEN-CHAIN:FN@ NRUN:ENTER0 ;]
   [: ;]
   CODEGEN-COMPARE:MEASURE-EMITTED
   CODEGEN-COMPARE:CALIBRATE ;

\ `: ADD3 ( n n n -- n ) + + ;`
: ADD3-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   s" ADD3 + +" NSRC:TEXT!
   c 3 1 REGS CODEGEN-CHAIN:CHAIN
   CODEGEN-CHAIN:PUBLISH!
   s" CODEGEN-CORPUS:ADD3" CODEGEN-CHAIN:BYTES
   [: 1 2 3 CODEGEN-CHAIN:FN@ NRUN:ENTER3 drop ;]
   [: 1 2 3 CODEGEN-CHAIN:FN@ NRUN:ENTER3 CODEGEN-COMPARE:VECTOR
      -5 5 7 CODEGEN-CHAIN:FN@ NRUN:ENTER3 CODEGEN-COMPARE:VECTOR ;]
   CODEGEN-COMPARE:MEASURE-EMITTED ;

\ `: SQUARE-SUM ( n n -- n ) dup * swap dup * + ;` - four renames and three
\ operations, and the renames are where the new chain stops paying.
: SQUARE-SUM-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   s" SQUARE-SUM dup * swap dup * +" NSRC:TEXT!
   c 2 1 REGS CODEGEN-CHAIN:CHAIN
   CODEGEN-CHAIN:PUBLISH!
   s" CODEGEN-CORPUS:SQUARE-SUM" CODEGEN-CHAIN:BYTES
   [: 3 4 CODEGEN-CHAIN:FN@ NRUN:ENTER2 drop ;]
   [: 3 4 CODEGEN-CHAIN:FN@ NRUN:ENTER2 CODEGEN-COMPARE:VECTOR
      -2 5 CODEGEN-CHAIN:FN@ NRUN:ENTER2 CODEGEN-COMPARE:VECTOR ;]
   CODEGEN-COMPARE:MEASURE-EMITTED ;

\ `: MAX2 ( n n -- n ) 2dup < if swap then drop ;` - the two-way branch. Four
\ blocks: the entry that compares, the stub the false arm reaches the join
\ through, the true arm, and the join that takes both arms' two values as its
\ arguments. It is the smallest word in the corpus whose answer depends on which
\ way a branch went, so the head-to-head check below - both argument orders, the
\ same two the old column uses - is what says the branch went the right way.
: MAX2-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   s" MAX2 2dup < if swap then drop" NSRC:TEXT!
   c 2 1 REGS CODEGEN-CHAIN:CHAIN
   CODEGEN-CHAIN:PUBLISH!
   s" CODEGEN-CORPUS:MAX2" CODEGEN-CHAIN:BYTES
   [: 3 4 CODEGEN-CHAIN:FN@ NRUN:ENTER2 drop ;]
   [: 3 4 CODEGEN-CHAIN:FN@ NRUN:ENTER2 CODEGEN-COMPARE:VECTOR
      9 -1 CODEGEN-CHAIN:FN@ NRUN:ENTER2 CODEGEN-COMPARE:VECTOR ;]
   CODEGEN-COMPARE:MEASURE-EMITTED ;

\ `: SUM-TO ( n -- n ) 0 swap 0 ?do i + loop ;` - the counted loop. Seven blocks,
\ and the one place in the corpus where the loop index is a value the chain
\ carries in a register rather than a frame the engine pushes.
: SUM-TO-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   s" SUM-TO 0 swap 0 ?do i + loop" NSRC:TEXT!
   c 1 1 LOOP-REGS CODEGEN-CHAIN:CHAIN
   CODEGEN-CHAIN:PUBLISH!
   s" CODEGEN-CORPUS:SUM-TO" CODEGEN-CHAIN:BYTES
   [: 16 CODEGEN-CHAIN:FN@ NRUN:ENTER1 drop ;]
   [: 16 CODEGEN-CHAIN:FN@ NRUN:ENTER1 CODEGEN-COMPARE:VECTOR
      1 CODEGEN-CHAIN:FN@ NRUN:ENTER1 CODEGEN-COMPARE:VECTOR ;]
   CODEGEN-COMPARE:MEASURE-EMITTED ;

\ `: COUNT-DOWN ( n -- n ) begin 1- dup 0 <= until ;` - the other loop form, with
\ the test at the end. The second pinned input is negative, so the loop runs once
\ and leaves; the first counts all the way down. Between them they measure both
\ ways out of a back edge.
: COUNT-DOWN-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   s" COUNT-DOWN begin 1- dup 0 <= until" NSRC:TEXT!
   c 1 1 LOOP-REGS CODEGEN-CHAIN:CHAIN
   CODEGEN-CHAIN:PUBLISH!
   s" CODEGEN-CORPUS:COUNT-DOWN" CODEGEN-CHAIN:BYTES
   [: 16 CODEGEN-CHAIN:FN@ NRUN:ENTER1 drop ;]
   [: 16 CODEGEN-CHAIN:FN@ NRUN:ENTER1 CODEGEN-COMPARE:VECTOR
      -3 CODEGEN-CHAIN:FN@ NRUN:ENTER1 CODEGEN-COMPARE:VECTOR ;]
   CODEGEN-COMPARE:MEASURE-EMITTED ;

\ `: CELL-BUMP ( n -- n ) BUMP-CELL ! BUMP-CELL @ 1+ dup BUMP-CELL ! ;` - the
\ memory word, written here exactly as the corpus writes it, `BUMP-CELL` and
\ all. What the chain is told about that name is its address, because the chain
\ cannot yet ask the engine what a data word is (dot
\ habu-resolve-a-data-a1c8067f); the address it is told is the corpus's own, so
\ the routine this column compiles bumps the SAME cell the interpreted word
\ bumps, and both columns record the cell's contents as an output. That is what
\ makes the head-to-head check a statement about the store and the load: a
\ routine that computed `n 1+` and touched no memory would answer the same two
\ numbers and fail on the other two.
: CELL-BUMP-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   s" CELL-BUMP BUMP-CELL ! BUMP-CELL @ 1+ dup BUMP-CELL !" NSRC:TEXT!
   c s" BUMP-CELL" CODEGEN-CORPUS:BUMP-ADDR 1 1 REGS CODEGEN-CHAIN:CHAIN-DATA
   CODEGEN-CHAIN:PUBLISH!
   s" CODEGEN-CORPUS:CELL-BUMP" CODEGEN-CHAIN:BYTES
   [: 7 CODEGEN-CHAIN:FN@ NRUN:ENTER1 drop ;]
   [: 7 CODEGEN-CHAIN:FN@ NRUN:ENTER1 CODEGEN-COMPARE:VECTOR
      CODEGEN-CORPUS:BUMP-CELL@ CODEGEN-COMPARE:VECTOR
      -1 CODEGEN-CHAIN:FN@ NRUN:ENTER1 CODEGEN-COMPARE:VECTOR
      CODEGEN-CORPUS:BUMP-CELL@ CODEGEN-COMPARE:VECTOR ;]
   CODEGEN-COMPARE:MEASURE-EMITTED ;

: COVERED-CASES ( -- )
   NFIX:BINDING [: NOOP-BODY ;] IR-CTX:WITH-CONTEXT
   NFIX:BINDING [: ADD3-BODY ;] IR-CTX:WITH-CONTEXT
   NFIX:BINDING [: SQUARE-SUM-BODY ;] IR-CTX:WITH-CONTEXT
   NFIX:BINDING [: MAX2-BODY ;] IR-CTX:WITH-CONTEXT
   NFIX:BINDING [: SUM-TO-BODY ;] IR-CTX:WITH-CONTEXT
   NFIX:BINDING [: COUNT-DOWN-BODY ;] IR-CTX:WITH-CONTEXT
   NFIX:BINDING [: CELL-BUMP-BODY ;] IR-CTX:WITH-CONTEXT ;

\ ---- the words the subset cannot express yet ---------------------------------
: GAP-CASES ( -- )
   s" CODEGEN-CORPUS:LERP" CODEGEN--NEW-CAP:LOCALS GAP
      CODEGEN--NEW-CAP:DIVISION GAP-ALSO
   s" CODEGEN-CORPUS:FACT" CODEGEN--NEW-CAP:CONTROL-FLOW GAP
      CODEGEN--NEW-CAP:CALLS GAP-ALSO
      CODEGEN--NEW-CAP:COMPARISON GAP-ALSO
   s" CODEGEN-CORPUS:BYTE-SUM" CODEGEN--NEW-CAP:CONTROL-FLOW GAP
      CODEGEN--NEW-CAP:LOCALS GAP-ALSO
      CODEGEN--NEW-CAP:MEMORY GAP-ALSO
   s" CODEGEN-CORPUS:BYTE-FIND" CODEGEN--NEW-CAP:CONTROL-FLOW GAP
      CODEGEN--NEW-CAP:LOCALS GAP-ALSO
      CODEGEN--NEW-CAP:MEMORY GAP-ALSO
      CODEGEN--NEW-CAP:COMPARISON GAP-ALSO ;

\ ---- accounting --------------------------------------------------------------
: GAP-FOR? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   false
   GAP-N @ 0 ?do
      i GAP-NAME$ a u STR= if drop true leave then
   loop ;

\ Every corpus word is compiled or declared a gap. A word that is neither would
\ leave the new column quietly shorter than the old one, which is the one failure
\ a comparison harness must never have.
: COVERAGE-CK ( -- )
   CODEGEN-COMPARE:ROWS 0 ?do
      i CODEGEN-COMPARE:PATH@ CODEGEN-COMPARE:PATH-OLD = if
         CODEGEN-COMPARE:PATH-NEW i CODEGEN-COMPARE:NAME$
         CODEGEN-COMPARE:FIND-ROW 0 < if
            i CODEGEN-COMPARE:NAME$ GAP-FOR? 0= if
               E-CODEGEN-COMPARE-CORPUS throw
            then
         then
      then
   loop ;

public

: RESET ( -- )
   0 GAP-N ! ;

\ Compile every corpus word the subset can express, declare the rest, and check
\ that between them they account for all of it. Runs after the old column, whose
\ rows the names are checked against.
: RUN ( -- )
   RESET
   COVERED-CASES
   GAP-CASES
   COVERAGE-CK ;

\ The old row this new row is the head-to-head partner of.
: PARTNER ( n -- n ) {: k:n :}
   CODEGEN-COMPARE:PATH-OLD k CODEGEN-COMPARE:NAME$ CODEGEN-COMPARE:FIND-ROW ;

\ Did the routine the new chain emitted compute what the old word computes? This
\ is the equality the whole comparison turns on.
: ROW-MATCH? ( n -- bool ) {: k:n :}
   k PARTNER {: b:n :}
   b 0 < if false exit then
   k b CODEGEN-COMPARE:SAME-OUTPUTS? ;

: MISMATCHES ( -- n )
   0
   CODEGEN-COMPARE:ROWS 0 ?do
      i CODEGEN-COMPARE:PATH@ CODEGEN-COMPARE:PATH-NEW = if
         i ROW-MATCH? 0= if 1+ then
      then
   loop ;

;package
