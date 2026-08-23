\ judge-test.f - the scheduled half of the code generator judge.
\ Run: bin/hb --load tools/judge-test.f
\
\ WHAT IT ASSERTS, AND WHY IT IS THE SAME WORDS THE COMMAND RUNS. Everything
\ below goes through tools/judge/check.f, which is what
\ `bin/hb --load tools/judge.f -- --check` drives, so a green run here is the
\ command passing rather than a second implementation of it agreeing with
\ itself.
\
\   the artifact          this tree's judgement and the committed file are the
\                         same bytes, and the half compared is the half habu's
\                         own chain determines - a forged clang cell does not
\                         move it and a moved chain byte count does
\   no larger row         no subject the chain compiled into more bytes than the
\                         engine's emitter wrote for the same body
\   the refusals          are E-A64RA-SPILL on two named rows, E-NFEED-LITERAL on
\                         one, and NOTHING ELSE. The old comparison named its
\                         refused subjects on a list; here the code is checked,
\                         so a refusal that changed its reason would fail even
\                         while the row still read REFUSED, and a third
\                         capability gap cannot arrive unnamed
\   the derived text      is the corpus file's own program, checked on the rows
\                         whose bodies the chain refuses - the only rows whose
\                         text no compiled artifact can vouch for
\
\ NOTHING HERE READS A CLOCK. Every column of the artifact is exact: a byte
\ count off a dictionary record, a refusal code from the compiler, and a verdict
\ that follows from the two. That is what lets this run in a parallel group
\ beside the other codegen members.

require lib/errors.f
require lib/string.f
require lib/test.f
require src/compiler/native/dict.f
require src/compiler/a64-effect.f
require tools/codegen-compare-corpus.f
require tools/judge/cost.f
require tools/judge/traffic.f
require tools/judge/check.f

package JUDGE-TEST

private

\ A refusal code the artifact's own head names, with the dot it waits on. A code
\ that is neither is a capability gap nobody has written down, and the count
\ below is what makes that fail rather than read as one more REFUSED row.
: NAMED-CODE? ( n -- bool ) {: rc:n :}
   rc E-A64RA-SPILL = ;

: NAMED-REFUSALS ( -- n )
   0
   JUDGE-ROW:ROWS 0 ?do
      i JUDGE-ROW:REFUSED? if
         i JUDGE-ROW:NEW-RC@ NAMED-CODE? if 1+ then
      then
   loop ;

: ROW-OF ( ptr u8 n -- n )
   JUDGE-ROW:FIND ;

: ARTIFACT-CASES ( -- )
   JUDGE-CHECK:DIFF-AT -1 T=
   JUDGE-ROW:LARGER-ROWS 0 T=
   JUDGE-ROW:DISAGREEING-ROWS 0 T= ;

\ ---- what a host may move, and what it may not -------------------------------
\ THE CLANG COLUMN IS OUTSIDE THE COMPARED BYTES, and this is where that is
\ proved on the real table. A clang cell is not falsifiable by any mutation of
\ habu, which is what docs/proofs.md asks of anything standing inside a gate;
\ and the column is Mach-O only by construction today, because
\ tools/codegen-compare-cc.f DECIDE refuses when HB-TARGET-MACOS? is false -
\ the byte counts come out of `nm -m` and `size -m`. So a host that is not
\ Mach-O has no column at all, and while a cell of it stood inside the checked
\ half this gate was red on such a host for no habu reason.
\
\ THE FORGE IS OVER THE LIVE FORTY-SIX-ROW TABLE, not a fixture table, and it
\ writes a reference value into EVERY row - which is exactly what a Mach-O host
\ adds to this run that a host without the column does not. So the case says,
\ in the only way a host without it can: regenerating the artifact where the
\ reference builds moves the printed half and cannot move one byte of the half
\ the gate compares. BOTH directions are asserted, because a forge that was
\ quietly dropped would pass a one-sided one: the whole artifact must differ
\ and the checked half must not. The cells are saved and put back, so the cases
\ after this read the table the measurement left.
\
\ AND THE REFERENCE NOTE, by OFFSET rather than by searching the checked half
\ for the word `clang`, which that half's own prose is entitled to use. The
\ note opens on a fixed line on every host, so the case needs no branch on
\ which of the two notes this host rendered.
\
\ AND THE OTHER DIRECTION OF THE GATE ITSELF, because a comparison that cannot
\ fail proves nothing: one chain byte count moved is refused by that same byte
\ check.

$6000 constant SNAP-CAP
create SNAP SNAP-CAP allot
variable SNAP-U

64 constant SAVE-MAX
create REF-B-SAVE SAVE-MAX cells allot
create REF-V-SAVE SAVE-MAX cells allot

: SLOT ( ptr a n -- ptr a )
   cells + ;

: SNAP! ( ptr u8 n -- ) {: a:ptr u:n :}
   u SNAP-CAP > if E-JUDGE-REPORT-CAP throw then
   a SNAP u STR-LEN BYTE-COPY-LEN
   u SNAP-U ! ;

: SNAP$ ( -- ptr u8 n )
   SNAP SNAP-U @ ;

: CHECKED-NOW$ ( -- ptr u8 n )
   JUDGE-REPORT:TEXT$ JUDGE-BASE:CHECKED$ ;

\ A reference byte count no host would produce, and a different one per row, so
\ a renderer that carried any single one of them into the checked half moves it.
: FORGED ( n -- n ) {: k:n :}
   k 7 * 3 + ;

\ Everything about one row the reference column owns, written together: the byte
\ count, the answer, and the memory the twin wrote. They go together because
\ giving a row a byte count is what makes it COVERED?, and a covered row's
\ answer and memory are both compared - so bytes alone would be half a host and
\ would report the unwritten answer as a disagreement.
\
\ THE MEMORY IS WRITTEN AS THE ENGINE'S, and that is exact rather than
\ convenient: a run in which any twin's memory differed from the engine's is a
\ run with a disagreeing row, which the artifact's own
\ `columns disagreeing on the answer: 0` and INPUT-CASES below both refuse. So
\ on a host that built a reference column this is the value that column already
\ held, and putting it back is putting back what was there; on a host without
\ one, nothing reads it. That is why this file never reads
\ JUDGE-ROW:REF-WITNESS@, which row.f keeps private to its own agreement check.
: REF-PUT ( n n n -- ) {: k:n bytes:n value:n :}
   k bytes JUDGE-ROW:REF!
   k  k JUDGE-ROW:REF-PICOS@  k JUDGE-ROW:REF-FLOOR@  value
      JUDGE-ROW:REF-COST!
   k  k JUDGE-ROW:OLD-WITNESS@  JUDGE-ROW:REF-WITNESS! ;

: REF-SAVE! ( -- )
   JUDGE-ROW:ROWS SAVE-MAX > if E-JUDGE-BASE-CAP throw then
   JUDGE-ROW:ROWS 0 ?do
      i JUDGE-ROW:REF-BYTES@  REF-B-SAVE i SLOT !
      i JUDGE-ROW:REF-VALUE@  REF-V-SAVE i SLOT !
   loop ;

: REF-RESTORE ( -- )
   JUDGE-ROW:ROWS 0 ?do
      i  REF-B-SAVE i SLOT @  REF-V-SAVE i SLOT @  REF-PUT
   loop ;

\ WHAT A MACH-O HOST ADDS TO THIS RUN, on every row: a byte count, and an answer
\ that AGREES with the engine's. The second half is not an assumption - the
\ committed artifact's `columns disagreeing on the answer: 0` is the record that
\ the reference really did agree where it was built.
: REF-FORGE ( -- )
   JUDGE-ROW:ROWS 0 ?do
      i  i FORGED  i JUDGE-ROW:OLD-VALUE@  REF-PUT
   loop ;

\ Where a text first carries another, or -1. The anchor case wants an OFFSET
\ rather than a yes or no, because what it asserts is which side of the marker
\ the reference note fell on.
: AT$ ( ptr u8 n ptr u8 n -- n ) {: a:ptr u:n b:ptr v:n :}
   -1
   u v < if 0 else u v - 1+ then 0 ?do
      dup 0 < if
         a i + v b v STR= if drop i then
      then
   loop ;

: HOST-CASES ( -- )
   s" the tree and the committed artifact agree before anything is forged"
   T-LABEL
   JUDGE-CHECK:DIFF-AT -1 T=

   JUDGE-REPORT:TEXT$ SNAP!
   REF-SAVE!
   REF-FORGE

   s" a whole reference column, forged, moves the artifact" T-LABEL
   JUDGE-REPORT:TEXT$ SNAP$ STR= TFALSE

   s" and does not move one byte of the checked half" T-LABEL
   CHECKED-NOW$ SNAP$ JUDGE-BASE:CHECKED$ T$=
   JUDGE-CHECK:DIFF-AT -1 T=
   JUDGE-ROW:DISAGREEING-ROWS 0 T=

   \ THE ONE THING ABOUT THE REFERENCE THE CHECKED HALF STILL SEES, and it is a
   \ finding rather than a fact about a host: a twin that answered differently
   \ from the engine on a pinned input. That is what the column is compared FOR,
   \ and it is why the tally stays above the marker while the bytes go below it.
   s" a reference that ANSWERS differently is a finding and does move it"
   T-LABEL
   0  0 FORGED  0 JUDGE-ROW:OLD-VALUE@ 1+  REF-PUT
   JUDGE-ROW:DISAGREEING-ROWS 1 T=
   JUDGE-CHECK:DIFF-AT 0 >= TTRUE

   REF-RESTORE

   s" and the table is back where the measurement left it" T-LABEL
   JUDGE-ROW:DISAGREEING-ROWS 0 T=
   JUDGE-CHECK:DIFF-AT -1 T=

   \ The note names the column, or names why this host has none. Either way it
   \ is below the line the check stops at.
   s" the reference note is below the marker, not inside the checked half"
   T-LABEL
   JUDGE-REPORT:TEXT$ JUDGE-REPORT:REFERENCE-ANCHOR$ AT$ {: anchor:n :}
   anchor 0 >= TTRUE
   JUDGE-REPORT:TEXT$ JUDGE-BASE:MARK-AT {: mark:n :}
   anchor mark > TTRUE

   \ And the gate can still fail: one chain byte count moved is refused.
   s" a moved chain byte count is refused by that same comparison" T-LABEL
   0 JUDGE-ROW:NEW-BYTES@ {: bytes:n :}
   0 JUDGE-ROW:NEW-TAIL? {: tail:bool :}
   0 bytes 4 + tail JUDGE-ROW:NEW!
   JUDGE-CHECK:DIFF-AT 0 >= TTRUE
   0 bytes tail JUDGE-ROW:NEW!
   JUDGE-CHECK:DIFF-AT -1 T= ;

\ ---- which WAY the artifact moved --------------------------------------------
\ The byte comparison above says the tree and the artifact are the same text. It
\ cannot say what a difference WOULD have been, and a chain row that grew and
\ one that shrank are two different events - a regression against ourselves and
\ progress. tools/judge/base.f adjudicates them apart and `--check` prints that
\ adjudication under its disagreement.
\
\ THE FIXTURE IS THE REAL MEASUREMENT WITH ONE NUMBER MOVED. The committed
\ artifact is the artifact, and the row that is shifted is a row of this run, so
\ what is compared is a whole real table against a whole real file differing in
\ exactly one cell. tools/judge/base-test.f attacks the reader itself with
\ artifacts built to fool it; what is asserted here is that it reads the REAL
\ one - every row of it, nothing lost, nothing gained, nothing to report - so a
\ report that changed the shape of a row is caught here rather than leaving
\ those fixtures agreeing with a format nothing writes any more.

: ADJUDICATE-COMMITTED ( -- n )
   JUDGE-CHECK:COMMITTED$ JUDGE-BASE:LOAD-FROM
   JUDGE-BASE:ADJUDICATE ;

: DIRECTION-CASES ( -- )
   JUDGE-BASE:QUIET!

   s" the committed artifact reads back as the rows this run measured" T-LABEL
   ADJUDICATE-COMMITTED 0 T=
   JUDGE-BASE:ROWS JUDGE-ROW:ROWS T=
   JUDGE-BASE:REGRESSIONS 0 T=
   JUDGE-BASE:IMPROVEMENTS 0 T=
   JUDGE-BASE:ENGINE-MOVES 0 T=
   JUDGE-BASE:LOST 0 T=
   JUDGE-BASE:GAINED 0 T=

   0 JUDGE-ROW:NEW-BYTES@ {: bytes:n :}
   0 JUDGE-ROW:NEW-TAIL? {: tail:bool :}

   s" a chain row bigger than the artifact's is a regression and a finding"
   T-LABEL
   0 bytes 4 + tail JUDGE-ROW:NEW!
   ADJUDICATE-COMMITTED 1 T=
   JUDGE-BASE:REGRESSIONS 1 T=
   JUDGE-BASE:IMPROVEMENTS 0 T=

   s" a chain row smaller than the artifact's is progress and is not" T-LABEL
   0 bytes 4 - tail JUDGE-ROW:NEW!
   ADJUDICATE-COMMITTED 0 T=
   JUDGE-BASE:REGRESSIONS 0 T=
   JUDGE-BASE:IMPROVEMENTS 1 T=

   0 bytes tail JUDGE-ROW:NEW!
   JUDGE-BASE:LOUD!

   s" and the row is back where the measurement left it" T-LABEL
   ADJUDICATE-COMMITTED 0 T= ;

\ ---- the pinned inputs past the first ----------------------------------------
\ A row is TIMED on one input and VALUED on every input it states. The ones past
\ the first exist for the arms the timed one does not take, and what they
\ establish is that independently compiled programs still agree about them - so
\ what is asserted here is that the rows really state them, that a row naming a
\ buffer the C file does not carry is counted apart rather than skipped in
\ silence, and that a row cannot state more than the shared pass holds.
\
\ THE ARGUMENT FOR THEM, MEASURED. A wrong answer planted in a C twin's BASE
\ CASE - right on ten, wrong on zero - is caught as a disagreeing row with
\ CODEGEN-CORPUS:FACT's second input and is not caught without it. That is the
\ whole of what these inputs buy, and it is a difference the artifact's own
\ tally shows.

: INPUT-CASES ( -- )
   s" every row is valued on at least its own pinned input" T-LABEL
   JUDGE-ROW:TOTAL-INPUTS JUDGE-ROW:ROWS >= TTRUE

   s" and the table states far more than one input per row" T-LABEL
   JUDGE-ROW:TOTAL-INPUTS JUDGE-ROW:ROWS 2 * > TTRUE

   \ One input per rung, which is what a ladder is.
   s" a row states as many inputs as its subject has arms" T-LABEL
   s" CODEGEN-CORPUS4:LADDER" ROW-OF JUDGE-ROW:INPUTS@ 8 T=
   s" CODEGEN-CORPUS2:WS?" ROW-OF JUDGE-ROW:INPUTS@ 5 T=
   s" CODEGEN-CORPUS3:MAX-F" ROW-OF JUDGE-ROW:INPUTS@ 6 T=

   \ The two subjects whose point is a STORE state ONE input each: what they
   \ wrote is read back over five cells, so the witness already reaches what a
   \ second input would.
   s" a row whose point is a store states one and says so" T-LABEL
   s" CODEGEN-CORPUS3:T-SGD!" ROW-OF JUDGE-ROW:INPUTS@ 1 T=
   s" CODEGEN-CORPUS2:VEC-COPY-CELLS" ROW-OF JUDGE-ROW:INPUTS@ 1 T=

   \ THE REFERENCE IS OPTIONAL ONE INPUT AT A TIME, as it already was one row at
   \ a time. tools/clang/twins.c carries no S-VEC and no Z-VEC, so an input over
   \ either has no reference program.
   \
   \ WHAT IS COUNTED HERE IS WHAT TWO FILES STATE - the corpus row, through
   \ IN+ and HABU-ONLY+, and the C file - and not what a toolchain did. So
   \ these numbers are the same on a host that builds a reference column and on
   \ one that cannot, which is why the artifact's CHECKED tally carries them.
   \ How many tuples the column actually REACHED is
   \ JUDGE-ROW:TOTAL-REF-REACHED, printed below the marker beside the reason
   \ there is no column, and that pair is what stops a comparison never made
   \ from reading as one made and passed.
   s" an input the C file has no buffer for is counted apart, not skipped"
   T-LABEL
   s" CODEGEN-CORPUS3:T-SUM" ROW-OF JUDGE-ROW:INPUTS@ 3 T=
   s" CODEGEN-CORPUS3:T-SUM" ROW-OF JUDGE-ROW:REF-INPUTS@ 2 T=
   s" CODEGEN-CORPUS3:T-NORM2" ROW-OF JUDGE-ROW:INPUTS@ 3 T=
   s" CODEGEN-CORPUS3:T-NORM2" ROW-OF JUDGE-ROW:REF-INPUTS@ 2 T=
   s" CODEGEN-CORPUS3:T-REL-L2" ROW-OF JUDGE-ROW:INPUTS@ 4 T=
   s" CODEGEN-CORPUS3:T-REL-L2" ROW-OF JUDGE-ROW:REF-INPUTS@ 2 T=

   s" and every other row's inputs all have a program in the C file" T-LABEL
   JUDGE-ROW:TOTAL-INPUTS JUDGE-ROW:TOTAL-REF-INPUTS -  4 T=

   s" a row that disagreed on any input disagrees" T-LABEL
   JUDGE-ROW:DISAGREEING-ROWS 0 T= ;

\ ---- the caller's data stack, counted ----------------------------------------
\ The ds-old and ds-new columns of the artifact. A counter that knew only some
\ of the spellings of one access would report an access the routine really makes
\ as no access at all, and a change of addressing mode or of register file would
\ read as a saving - so the counter is handed the exact encodings rather than
\ checked against another count of the same code. The base register is asked of
\ src/compiler/a64-effect.f, which is the chain's own statement of where the
\ running engine keeps that pointer, so these fixtures follow it if it moves.

: DS-BASE ( -- n )
   A64EFF:DSTACK-GPR 5 lshift ;

\ Any register that is not the data-stack pointer. Flipping the low bit is
\ enough and stays inside the register field.
: OTHER-BASE ( -- n )
   A64EFF:DSTACK-GPR 1 xor 5 lshift ;

: DS? ( n -- bool )
   JUDGE-TRAFFIC:INSN? ;

: TRAFFIC-FORM-CASES ( -- )
   s" every spelling of a whole-cell access to that register is counted" T-LABEL
   $F9000000 DS-BASE or DS? TTRUE            \ str  Xt, [ds, #imm]
   $F9400000 DS-BASE or DS? TTRUE            \ ldr  Xt, [ds, #imm]
   $F8000000 DS-BASE or DS? TTRUE            \ stur Xt, [ds, #simm]
   $F8400000 DS-BASE or DS? TTRUE            \ ldur Xt, [ds, #simm]
   $FD000000 DS-BASE or DS? TTRUE            \ str  Dt, [ds, #imm]
   $FD400000 DS-BASE or DS? TTRUE            \ ldr  Dt, [ds, #imm]
   $FC000000 DS-BASE or DS? TTRUE            \ stur Dt, [ds, #simm]
   $FC400000 DS-BASE or DS? TTRUE            \ ldur Dt, [ds, #simm]

   s" and not one of them counts against any other base register" T-LABEL
   $F9000000 OTHER-BASE or DS? TFALSE
   $F9400000 OTHER-BASE or DS? TFALSE
   $F8000000 OTHER-BASE or DS? TFALSE
   $F8400000 OTHER-BASE or DS? TFALSE
   $FD000000 OTHER-BASE or DS? TFALSE
   $FD400000 OTHER-BASE or DS? TFALSE
   $FC000000 OTHER-BASE or DS? TFALSE
   $FC400000 OTHER-BASE or DS? TFALSE

   s" a narrower access is not a cell of this stack" T-LABEL
   $B9000000 DS-BASE or DS? TFALSE           \ str  Wt
   $B9400000 DS-BASE or DS? TFALSE           \ ldr  Wt

   s" and an instruction that reaches no memory is not an access at all" T-LABEL
   $8B000000 DS-BASE or DS? TFALSE           \ add  Xd, ds, Xm
   $91000000 DS-BASE or DS? TFALSE ;         \ add  Xd, ds, #imm

\ And the column over real routines. A name nothing published is refused rather
\ than answered with zero, which is what a silently missing routine would
\ otherwise look like: no accesses at all.
: TRAFFIC-COLUMN-CASES ( -- )
   s" a subject this image does not hold is refused, not counted as zero"
   T-LABEL
   [: s" CODEGEN-CORPUS4:NO-SUCH-WORD" JUDGE-TRAFFIC:COUNT drop ;]
   E-CODEGEN-COMPARE-SUBJECT TTHROWSQ

   \ ONE row of the table is heavier, and it is named rather than only counted,
   \ for the reason the refusal cases below are: a count would not notice which
   \ row came back. On CALL-FAN-BIG the engine CALLS its callee five times, so
   \ its caller touches no slot at all and the callees do that work in their own
   \ routines, while the chain copies the five bodies in and makes its own entry
   \ and exit.
   s" one row of the table touches the caller's stack more often" T-LABEL
   JUDGE-ROW:HEAVIER-ROWS 1 T=
   s" CODEGEN-CORPUS4:CALL-FAN-BIG" ROW-OF JUDGE-ROW:HEAVIER? TTRUE
   s" CODEGEN-CORPUS4:CALL-FAN-BIG" ROW-OF JUDGE-ROW:OLD-TRAFFIC@ 0 T=

   \ The rows the whole column is about: a loop body whose intermediates the
   \ engine moves through memory every turn and the chain keeps in registers.
   s" and the loop rows spend a fraction of what the engine spends" T-LABEL
   s" CODEGEN-CORPUS3:T-SUM" ROW-OF JUDGE-ROW:HEAVIER? TFALSE
   s" CODEGEN-CORPUS4:PRESSURE-LOOP" ROW-OF JUDGE-ROW:HEAVIER? TFALSE
   s" CODEGEN-CORPUS2:WS?" ROW-OF JUDGE-ROW:HEAVIER? TFALSE

   \ A refused row has no routine to count, so it is neither heavier nor
   \ lighter, and the artifact writes a dash rather than a zero.
   s" a column with no routine is not measured rather than measured at zero"
   T-LABEL
   JUDGE-ROW:NOT-MEASURED -1 T= ;

\ ---- the generated bodies, attacked ------------------------------------------
\ tools/judge/cost.f builds a body out of a row's input text and one column's
\ word. A body that compiles is not a body that measures the row, so every way
\ of getting one wrong is checked here against the shipped generator.

: FAN$ ( -- ptr u8 n )
   s" CODEGEN-CORPUS4:CALL-FAN" ;

: FAN-J$ ( -- ptr u8 n )
   s" CODEGEN-CORPUS4:CALL-FAN-J4" ;

: FAN-ENTRY ( -- n )
   FAN$ NDICT:CALL-TARGET ;

: FAN-J-ENTRY ( -- n )
   FAN-J$ NDICT:CALL-TARGET ;

\ THE ONE A COMPARISON OF ANSWERS CANNOT SEE. A body built for the chain's
\ column that names the ENGINE's word is the right program in the wrong column:
\ it computes what the row computes, so every answer agrees, and the cost
\ reported for the chain is the engine's. The address is what catches it.
: WRONG-COLUMN-CASES ( -- )
   FAN-ENTRY 0 T<>
   FAN-J-ENTRY 0 T<>
   FAN-ENTRY FAN-J-ENTRY T<>
   [: FAN$ FAN-J-ENTRY JUDGE-COST:COLUMN-CK ;] E-JUDGE-COST-COLUMN TTHROWSQ
   [: FAN-J$ FAN-ENTRY JUDGE-COST:COLUMN-CK ;] E-JUDGE-COST-COLUMN TTHROWSQ
   [: s" CODEGEN-CORPUS4:NO-SUCH-WORD" FAN-ENTRY JUDGE-COST:COLUMN-CK ;]
      E-JUDGE-COST-COLUMN TTHROWSQ
   FAN$ FAN-ENTRY JUDGE-COST:COLUMN-CK
   FAN-J$ FAN-J-ENTRY JUDGE-COST:COLUMN-CK ;

\ AND THE ONES IT CAN. An input list one number short does not type, so the
\ checker declines the generated body rather than the run timing a program with
\ a stack it never had; an input list with the WRONG number computes something
\ else, which is what the answers column exists to notice.
: WRONG-INPUT-CASES ( -- )
   [: s" " FAN$ 1 JUDGE-COST:P-CELL JUDGE-COST:VALUE drop ;] E-JUDGE-COST-CHECK TTHROWSQ
   [: s" 7 7" FAN$ 1 JUDGE-COST:P-CELL JUDGE-COST:VALUE drop ;] E-JUDGE-COST-CHECK TTHROWSQ
   s" 7" FAN$ 1 JUDGE-COST:P-CELL JUDGE-COST:VALUE
   s" 8" FAN$ 1 JUDGE-COST:P-CELL JUDGE-COST:VALUE T<> ;

\ THE BOUNDARY INPUTS, THROUGH BOTH CODE GENERATORS. The pinned input a row is
\ timed on runs the longest path through it; these run the arithmetic off the
\ ends of the signed range, where a lost sign or a narrowed width is visible and
\ nowhere else is. Both columns compile the same corpus text, so the two must
\ answer the same cell.
: BOUNDARY-CASES ( -- )
   s" $8000000000000000" FAN$ 1 JUDGE-COST:P-CELL JUDGE-COST:VALUE
   s" $8000000000000000" FAN-J$ 1 JUDGE-COST:P-CELL JUDGE-COST:VALUE T=
   s" $7FFFFFFFFFFFFFFF" FAN$ 1 JUDGE-COST:P-CELL JUDGE-COST:VALUE
   s" $7FFFFFFFFFFFFFFF" FAN-J$ 1 JUDGE-COST:P-CELL JUDGE-COST:VALUE T=
   s" -1" FAN$ 1 JUDGE-COST:P-CELL JUDGE-COST:VALUE
   s" -1" FAN-J$ 1 JUDGE-COST:P-CELL JUDGE-COST:VALUE T= ;

\ NO ROW OF THE TABLE IS REFUSED. The count is asserted at zero, and the balance
\ between it and the codes the artifact's head names is kept because a refusal
\ carrying some OTHER code would be a capability gap nobody has written down and
\ must not read as the one that closed.
\
\ THE THREE ROWS THAT WERE GAPS EACH KEEP A CASE saying so rather than simply
\ leaving the list. CODEGEN-CORPUS2:SYM-FOLD-C waited on a tape that could
\ record a hexadecimal literal (dot habu-record-the-engine-79c570ed).
\ CODEGEN-CORPUS4:CALL-PRESSURE carried the loop-spill code until the elaborator
\ learnt to leave a crossing local in a register the callee spares
\ (src/compiler/native/elaborate.f CALL-KEEPS?). CODEGEN-CORPUS4:PRESSURE-LOOP
\ was the last one of all and carried the same code until
\ src/compiler/native/loop.f moved the reads that cannot change with the turn out
\ of its body, so it never holds the fourteen values the allocator refused to
\ place.
\
\ THE CASES ARE WRITTEN AS "NOT REFUSED" AND NOT ONLY AS A COUNT, because a count
\ would not notice which row came back. A regression in any of the three puts its
\ row back among the refusals carrying the code this file still names, so the
\ balance would hold and nothing else here would see it.
: REFUSAL-CASES ( -- )
   JUDGE-ROW:REFUSED-ROWS 0 T=
   JUDGE-ROW:REFUSED-ROWS NAMED-REFUSALS T=
   s" CODEGEN-CORPUS4:PRESSURE-LOOP" ROW-OF JUDGE-ROW:REFUSED? TFALSE
   s" CODEGEN-CORPUS2:SYM-FOLD-C" ROW-OF JUDGE-ROW:REFUSED? TFALSE
   s" CODEGEN-CORPUS4:CALL-PRESSURE" ROW-OF JUDGE-ROW:REFUSED? TFALSE ;

\ A compiled row vouches for its own text: the chain read it, the checker
\ certified it and a routine came out. PRESSURE-LOOP keeps its case from the
\ years it had no such witness, because the corpus's own program is what the
\ whole row means and a body quietly narrowed to make it compile would be a
\ different program measured under the same name.
\ SYM-FOLD-C keeps its case now that it compiles, for the other half of the
\ argument its row used to carry in the head of the artifact: its constants are
\ written $41, $5A and $20, and a corpus quietly respelled in decimal to make a
\ row green would be a different program measured under the same name.
\ The source is loaded first because the reader holds ONE file at a time and
\ the pass that judged every corpus left the last of them in it.
: TEXT-CASES ( -- )
   s" tools/codegen-compare-corpus4.f" JUDGE-SRC:LOAD
   s" PRESSURE-LOOP" JUDGE-SRC:FIND s" -J4" JUDGE-SRC:TEXT$
   S\" : PRESSURE-LOOP-J4 ( ptr n n -- n ) {: base:ptr len:n :}\n   0\n   len 0 ?do\n      base @  base 8 + @  base 16 + @  base 24 + @  base 32 + @\n      base 40 + @  base 48 + @  base 56 + @  base 64 + @  base 72 + @\n      base 80 + @  base 88 + @  base 96 + @  base 104 + @\n      + + + + + + + + + + + + + +\n   loop ;" T$=

   s" tools/codegen-compare-corpus2.f" JUDGE-SRC:LOAD
   s" SYM-FOLD-C" JUDGE-SRC:FIND s" -J2" JUDGE-SRC:TEXT$
   S\" : SYM-FOLD-C-J2 ( n -- n ) {: c:n :}\n   c $41 < if c exit then\n   c $5A > if c exit then\n   c $20 or ;" T$= ;

\ ---- the storage entry, on real corpus data ----------------------------------
\ A body that names one of its file's storage words needs the migration entry
\ that can express storage. Measured rather than described: corpus 1's
\ CELL-BUMP, which WRITES its cell, is refused with E-A64RAV-DKEEP under the
\ plain entry and compiles under this one; corpus 2's FILL-COPY names TWO and
\ is refused here, because the entry takes one spelling and picking the first
\ would compile a body whose other cell resolved to whatever the scope held.
\
\ THE PUBLICATION IS AT THE FOOT OF THIS FILE, inside corpus 1's own package,
\ because a migration publishes where the interpreter's wordlist points and
\ BUMP-CELL is private to that corpus. What is asserted here is what that line
\ left behind.

variable BUMP-RC

: STORAGE-CASES ( -- )
   BUMP-RC @ 0 T=
   s" tools/codegen-compare-corpus.f" JUDGE-SRC:LOAD
   s" CODEGEN-CORPUS:" JUDGE-CHAIN:QUALIFIER!
   s" -JT1" JUDGE-CHAIN:SUFFIX!
   s" CELL-BUMP" JUDGE-SRC:FIND JUDGE-CHAIN:SIZE 0 T<>

   s" tools/codegen-compare-corpus2.f" JUDGE-SRC:LOAD
   s" -JT2" JUDGE-CHAIN:SUFFIX!
   s" FILL-COPY" JUDGE-SRC:FIND JUDGE-CHAIN:PUBLISH E-JUDGE-CHAIN-DATA T= ;

\ ---- what a generated body did with what the subject left ---------------------
\ Three of the corpora hold a subject whose point is a STORE, and one holds a
\ subject that answers a flag while two hold subjects that answer a double.
\ Every one of those is a value a generated body has to project or read back
\ before two columns can be compared on it, so the projection is pinned here on
\ the numbers themselves rather than left to the columns agreeing about zero.

: WITNESS-CASES ( -- )
   s" CODEGEN-CORPUS:CELL-BUMP" ROW-OF JUDGE-ROW:WITNESSED? TTRUE
   s" CODEGEN-CORPUS:ADD3" ROW-OF JUDGE-ROW:WITNESSED? TFALSE

   \ CELL-BUMP stores its argument, reads it back and stores that plus one, so
   \ on the pinned 7 the cell holds 8 and the answer is the same 8. Folded
   \ together the two would be zero whatever either was; apart, each says what it
   \ measured.
   s" CODEGEN-CORPUS:CELL-BUMP" ROW-OF JUDGE-ROW:OLD-VALUE@ 8 T=
   s" CODEGEN-CORPUS:CELL-BUMP" ROW-OF JUDGE-ROW:OLD-WITNESS@ 8 T=
   s" CODEGEN-CORPUS:CELL-BUMP" ROW-OF JUDGE-ROW:NEW-WITNESS@ 8 T=

   \ VEC-COPY-CELLS and T-SGD! leave nothing at all, so the witness is the whole
   \ of what their rows compare. A zero there would be the reader never running.
   s" CODEGEN-CORPUS2:VEC-COPY-CELLS" ROW-OF JUDGE-ROW:OLD-WITNESS@ 0 T<>
   s" CODEGEN-CORPUS3:T-SGD!" ROW-OF JUDGE-ROW:OLD-WITNESS@ 0 T<> ;

\ A flag is not a number and a double is not a cell until each is projected, and
\ both projections are the ones the comparison already records that kind by.
: PROJECTION-CASES ( -- )
   \ WS? on a space answers true, which is 1 through the shared flag projection.
   s" CODEGEN-CORPUS2:WS?" ROW-OF JUDGE-ROW:OLD-VALUE@ 1 T=
   s" CODEGEN-CORPUS2:WS?" ROW-OF JUDGE-ROW:NEW-VALUE@ 1 T=

   \ T-SUM over -2.5 0.0 1.5 0.25 is -0.75, whose cell is $BFE8000000000000.
   s" CODEGEN-CORPUS3:T-SUM" ROW-OF JUDGE-ROW:OLD-VALUE@
      -0.75 JUDGE-COST:REAL-BITS T=
   s" CODEGEN-CORPUS3:T-SUM" ROW-OF JUDGE-ROW:NEW-VALUE@
      -0.75 JUDGE-COST:REAL-BITS T=

   \ And the generator refuses what it cannot project or read back rather than
   \ emitting a body that reaches past the top of the stack or records the
   \ subject's own answer under a second name.
   [: 2 JUDGE-COST:P-REAL JUDGE-COST:FOLD+ ;] E-JUDGE-COST-FOLD TTHROWSQ
   [: 2 JUDGE-COST:P-FLAG JUDGE-COST:FOLD+ ;] E-JUDGE-COST-FOLD TTHROWSQ
   [: s" 7" FAN$ s" " 1 JUDGE-COST:WITNESS drop ;] E-JUDGE-COST-WITNESS TTHROWSQ ;

\ ---- the C call shape a reference row is reached through ----------------------
\ A twin that takes or answers a double has a shape the subject's arity cannot
\ give, so the row states it - and a spelling nobody can read an answer kind off
\ is refused rather than guessed at. Run last, because opening a corpus here
\ moves the reader and the chain's suffix.

: SHAPE-CASES ( -- )
   s" tools/codegen-compare-corpus4.f" s" -JS" s" CODEGEN-CORPUS4:"
      JUDGE-PASS:CORPUS!
   [: s" LADDER" s" hc4_ladder" s" IIZ" JUDGE-PASS:ROW-ABI! ;]
      E-JUDGE-PASS-SHAPE TTHROWSQ
   [: s" LADDER" s" hc4_ladder" s" " JUDGE-PASS:ROW-ABI! ;]
      E-JUDGE-PASS-SHAPE TTHROWSQ
   [: s" NO-SUCH-SUBJECT" s" hc4_ladder" JUDGE-PASS:ROW! ;]
      E-JUDGE-SRC-ROW TTHROWSQ
   s" LADDER" s" hc4_ladder" s" IID" JUDGE-PASS:ROW-ABI!
   JUDGE-PASS:NAME$ s" CODEGEN-CORPUS4:LADDER" T$=

   \ A row cannot state more inputs than the shared pass holds tuples for, and
   \ the refusal names that rather than reading as a text buffer overflowing: an
   \ input silently dropped is an arm silently not compared.
   [: JUDGE-PASS:IN-MAX 0 ?do JUDGE-PASS:ALSO loop ;]
   E-JUDGE-PASS-INPUTS TTHROWSQ ;

public

\ Where corpus 1's derived cell-stepping word records what the chain answered.
: BUMP-RC! ( n -- )
   BUMP-RC ! ;

: RUN ( -- )
   T-RESET
   JUDGE-CHECK:JUDGE-ALL
   ARTIFACT-CASES
   HOST-CASES
   DIRECTION-CASES
   INPUT-CASES
   TRAFFIC-FORM-CASES
   TRAFFIC-COLUMN-CASES
   REFUSAL-CASES
   WITNESS-CASES
   PROJECTION-CASES
   TEXT-CASES
   WRONG-COLUMN-CASES
   WRONG-INPUT-CASES
   BOUNDARY-CASES
   STORAGE-CASES
   SHAPE-CASES
   T-REPORT ;

;package

\ Corpus 1's CELL-BUMP through the storage entry, published where its private
\ cell is reachable. The line is here rather than inside a word because
\ `package` is read at load and a migration lands where the interpreter points.
package JUDGE-TEST
public
: PUBLISH-BUMP ( -- )
   s" CODEGEN-CORPUS:" JUDGE-CHAIN:QUALIFIER!
   s" -JT1" JUDGE-CHAIN:SUFFIX!
   s" tools/codegen-compare-corpus.f" JUDGE-SRC:LOAD
   s" CELL-BUMP" JUDGE-SRC:FIND JUDGE-CHAIN:PUBLISH-CALLING JUDGE-TEST:BUMP-RC! ;
;package

package CODEGEN-CORPUS
public
JUDGE-TEST:PUBLISH-BUMP
;package

JUDGE-TEST:RUN
