\ judge/cost.f - timing one row's program in one code generator's column, and
\ reading the value it computes. One concern: turning a row's pinned input TEXT
\ and one word's spelling into a measured program.
\
\ THE TIMING DISCIPLINE IS THIS FILE'S OWN, and so are the two projections a
\ measured value crosses into a comparable cell. They lived in the comparison
\ harness's measurement store until that store went; the generator below is the
\ only thing that writes either of them into a body, so they belong beside it.
\
\ WHY THE BODY IS GENERATED. A timed body has to CALL the subject, and a call
\ names the word, and a body is compiled before it runs - so a body written by
\ hand in a source file can only name words that already exist. Two things
\ follow, and they are why the old comparison looks the way it does. First, a
\ subject the chain REFUSES has no word at all, so a hand-written body naming it
\ does not compile and the whole file fails to load: the old harness lives with
\ that by keeping a hand-written list of the refused subjects and simply not
\ writing bodies for them. Second, the same pinned input has to be retyped once
\ per column - the old comparison wrote it once for the engine, again for the
\ chain and a third time for the C twin - and three
\ copies of a number is three chances to time one column on a different program
\ from the other two.
\
\ Here the input is stated ONCE, as text, and a body is built from it for
\ whichever column is being measured. A refused subject simply has no body
\ built, which needs no list and no exception: nothing was ever written down
\ that has to be kept in step.
\
\ WHAT IS GENERATED, EXACTLY. Three shapes, and nothing else:
\
\   : <name> ( -- ) [: <inputs> <call> drop ;] TIME-ONLY PICOS! ;
\   : <name> ( -- ) <inputs> <call> <fold> RESULT! ;
\   : <name> ( -- ) <inputs> <call> drop… <readback> RESULT! ;
\
\ `<inputs>` is the row's own text and `<call>` is the column's - the engine's
\ word, the chain's word, or the foreign-call shape that reaches the C twin. The
\ timed shape is the same one the old comparison wrote by hand, so what is timed
\ here is what that harness timed: a quotation that calls the subject once and
\ drops its answer.
\
\ `<fold>` accounts for what the subject left - `xor` for several values, the
\ float or flag projection for one that is not a plain cell, a literal zero for
\ a subject that leaves nothing. The third shape is for a subject whose point is
\ a STORE: it runs the same program and then the row's own reader for the memory
\ the subject wrote. It is a separate number rather than a fold into the first
\ for the reason WITNESS gives below.
\
\ AND THE GENERATOR CANNOT SILENTLY TIME THE WRONG PROGRAM. A generated body is
\ text, and text that compiles is not text that computes the right thing: an
\ input list with a number missing still compiles for some subjects, and a body
\ built against the wrong column's word compiles perfectly. So every row is also
\ VALUED, by the second shape above, in every column - and the columns must
\ agree. A generator that built one column's body wrong makes that column answer
\ something else, and the run fails on the answers rather than reporting a time
\ for a program nobody meant to run. tools/judge-test.f hands the generator
\ deliberately wrong bodies and checks that each one is caught.
\
\ THE EVALUATE BOUNDARY, WHICH IS NAMED AND SMALL. The checker rejects
\ `evaluate` inside a checked definition, so the four words that reach it are
\ TRUSTED: and are the only unchecked words in the judge. What they compile is
\ NOT unchecked: every generated definition is handed to the checker first, and
\ a definition the checker declines is a refusal here rather than a program that
\ runs. The unchecked compile that follows is the shape
\ tools/codegen-role.f established and gives its reason for - a checked
\ re-compile of an already-certified definition hits strict duplicate rejection
\ - and the certification hook is put back immediately after, on the failure
\ path as well as the ordinary one.
\
\ THE PATTERN AND ITS OWNER, CITED. The four words below are the
\ tools/codegen-role.f CGR-EVALUATE / CGR-CHECK! / CGR-HOOK / CGR-HOOK! quartet,
\ which is itself the test/prop-test-core.f CHK / CHK-COMPILE-CERT precedent, and
\ they are the evaluate-class named boundary the four-class TRUSTED ruling keeps
\ rather than the kind it retires. Dot habu-primitive-effect-axiom-1119f176 is
\ the retirement owner: when `evaluate` and `CHECK!` carry primitive effect
\ axioms these four stop being trusted and this paragraph goes with them.
\
\ EVERY MEASUREMENT GETS A FRESH NAME. A generated word is never redefined,
\ because a redefinition would leave the old record retired while a reader could
\ still reach it, and because two columns of one row measured through one name
\ would be two programs with one dictionary record between them.

require lib/errors.f
require lib/prelude.f
require lib/string.f
require lib/codegen.f
require lib/fmt.f
require src/compiler/native/dict.f

package JUDGE-COST

\ ---- how long one body takes -------------------------------------------------
\ One timed run executes the body REPS times back to back and divides the
\ elapsed monotonic nanoseconds by REPS. The body is timed RUNS times and the
\ FASTEST run is kept: interference from the rest of the machine can only make a
\ run slower, so the minimum is the closest estimate of the real cost available.
\ Per-call costs are a few nanoseconds, so a measurement is kept in picoseconds,
\ which leaves three significant digits where whole nanoseconds leave one.
\
\ SEVEN SHORT RUNS RATHER THAN THREE LONG ONES, measured and not preferred.
\ Three runs of a million repetitions was tried first: on a 12-core host under
\ 16 competing processes one case had all three runs hit by the same sustained
\ scheduling delay and came out 4.1 times its idle cost. More, shorter runs give
\ the fastest-run rule more chances at a clean window, and the whole pass got
\ faster as well - 0.50 s idle against 0.90 s.

public

250000 constant REPS
7 constant RUNS

private

1000 constant PICOS-PER-NS        \ the unit a measurement is kept in
$7FFFFFFFFFFFFFFF constant PICOS-MAX

variable FASTEST

\ typed-local-lint: allow-bare-local - q is the timing body; its effect is in
\ the stack signature and a local annotation cannot carry a quotation effect.
: RUN-ONCE ( [ -- ] -- n ) {: q :}
   mono-ns {: t0:n :}
   REPS 0 ?do q execute loop
   mono-ns t0 - PICOS-PER-NS * REPS / ;

: SAMPLE ( n -- ) {: picos:n :}
   picos FASTEST @ < if picos FASTEST ! then ;

\ typed-local-lint: allow-bare-local - q is the timing body, as in RUN-ONCE.
: TIME-RUNS ( [ -- ] -- ) {: q :}
   PICOS-MAX FASTEST !
   RUNS 0 ?do q RUN-ONCE SAMPLE loop
   FASTEST @ 0= if E-CODEGEN-COMPARE-CLOCK throw then ;

public

\ What one body costs, in picoseconds per call. Public because every generated
\ timing body ends in it.
\ typed-local-lint: allow-bare-local - q is a timing body, as in RUN-ONCE.
: TIME-ONLY ( [ -- ] -- n ) {: q :}
   q TIME-RUNS
   FASTEST @ ;

\ ---- a value that is not a cell, as the cell a column is compared on ---------
\ Each crossing has exactly one route, because two columns crossing by two
\ routes of their own would let a difference in the routes read as a difference
\ in the compiled code. Both are public for that reason: the generated bodies
\ name them and must name the same ones.

: FLAG-BITS ( bool -- n )
   if 1 else 0 then ;

\ A double is already one data-stack cell holding its raw IEEE754 bit pattern,
\ so this is a retype and not a conversion. The recorded value is the whole
\ cell, which makes two columns agree only if every bit agrees: finer than float
\ equality at +0.0 against -0.0, and at a NaN, both deliberately.
CAST: REAL-BITS ( r -- n ) ;

private

$400 constant TEXT-CAP
TEXT-CAP CODEGEN:BUFFER TXT

variable SEQ                       \ how many bodies have been generated
variable PICOS-CELL
variable RESULT-CELL
variable TWIN-CELL

\ ---- the evaluate boundary ---------------------------------------------------
\ The only unchecked words in the judge. Each is one line and does one thing.

TRUSTED: EVALUATE-UNCHECKED ( ptr u8 n -- ) 0 set-check evaluate ;
TRUSTED: CERTIFY ( ptr u8 n -- n ) CHECK! ;
TRUSTED: CERT-HOOK ( ptr u8 n -- n ) CHECK! dup -1 <> IF 70 throw THEN ;
TRUSTED: CERT-HOOK! ( -- )
   LOWER-CERT-HOOK:INSTALL
   ['] CERT-HOOK set-check ;

variable EV-A
variable EV-U

: EV$ ( -- ptr u8 n )
   EV-A 0 ptr-field @ EV-U @ ;

: EV-GO ( -- )
   EV$ EVALUATE-UNCHECKED ;

\ Compile the text and put the certification hook back whatever happened, so a
\ refusal here cannot leave later definitions compiling untyped.
: EV ( ptr u8 n -- ) {: a:ptr u:n :}
   a EV-A 0 ptr-field !  u EV-U !
   [: EV-GO ;] catch {: rc:n :}
   CERT-HOOK!
   rc 0<> if
      s" judge: a generated body failed to compile:" type cr
      a u type cr
      E-JUDGE-COST-COMPILE throw
   then ;

\ The definition text without its `: ` and its ` ;`, which is what the checker
\ takes.
: CERTIFY-DEF ( ptr u8 n -- ) {: a:ptr u:n :}
   a 2 + u 4 - CERTIFY -1 <> if
      s" judge: a generated body failed the checker:" type cr
      a u type cr
      E-JUDGE-COST-CHECK throw
   then ;

\ ---- building one body -------------------------------------------------------

: NAME+ ( -- )
   s" JB" TXT CODEGEN:APPEND-STRING
   SEQ @ TXT CODEGEN:APPEND-DECIMAL
   SEQ @ 1+ SEQ ! ;

\ `: JBn ( -- ) ` - the frame every generated body opens with.
: OPEN+ ( -- )
   TXT CODEGEN:RESET
   s" : " TXT CODEGEN:APPEND-STRING
   NAME+
   s"  ( -- ) " TXT CODEGEN:APPEND-STRING ;

: CLOSE+ ( -- )
   s"  ;" TXT CODEGEN:APPEND-STRING ;

\ The name the body just built goes by, so the text that RUNS it can name it.
create RUN-NAME 32 allot
variable RUN-NAME-U

: RUN-NAME! ( -- )
   s" JB" {: pa:ptr pu:n :}
   pa RUN-NAME pu STR-LEN BYTE-COPY-LEN
   SB-RESET SEQ @ 1- FMT:SB-U SB$ {: da:ptr du:n :}
   da RUN-NAME pu + du STR-LEN BYTE-COPY-LEN
   pu du + RUN-NAME-U ! ;

\ Certify the definition, compile it, and run it. The invocation is appended
\ after the definition's `;` so one evaluate does both: a second one would need
\ its own text and its own failure path for no gain.
: COMPILE-AND-RUN ( -- )
   TXT CODEGEN:CONTENTS CERTIFY-DEF
   RUN-NAME!
   s"  " TXT CODEGEN:APPEND-STRING
   RUN-NAME RUN-NAME-U @ TXT CODEGEN:APPEND-STRING
   TXT CODEGEN:CONTENTS EV ;

public

\ Compile one whole definition through the ENGINE: the checker first, then the
\ engine's own compiler, over the same audited evaluate boundary every generated
\ body crosses. It is here rather than in the caller because the boundary is
\ here: the differential oracle's engine column has to be the production compile
\ path and not a second way in.
: DEFINE ( ptr u8 n -- ) {: a:ptr u:n :}
   a u CERTIFY-DEF
   a u EV ;

\ The same, and then RUN it, by appending the invocation after the definition's
\ `;` so one evaluate does both - which is what COMPILE-AND-RUN above does for a
\ body this file generated itself. A caller that compiles a driver once and runs
\ it over many inputs needs this rather than a fresh definition per input: every
\ generated word is a dictionary record that is never reclaimed.
: DEFINE-RUN ( ptr u8 n ptr u8 n -- ) {: da:ptr du:n na:ptr nu:n :}
   da du CERTIFY-DEF
   TXT CODEGEN:RESET
   da du TXT CODEGEN:APPEND-STRING
   s"  " TXT CODEGEN:APPEND-STRING
   na nu TXT CODEGEN:APPEND-STRING
   TXT CODEGEN:CONTENTS EV ;

\ Where a generated timing body leaves its measurement, and a generated value
\ body its answer. Public because the generated text names them.
: PICOS! ( n -- )
   PICOS-CELL ! ;

: RESULT! ( n -- )
   RESULT-CELL ! ;

\ The C function a reference column's body calls. Public for the same reason.
: TWIN@ ( -- n )
   TWIN-CELL @ ;

: TWIN! ( n -- )
   TWIN-CELL ! ;

\ The one guard a generated body needs that its ANSWER cannot give. A body built
\ against the wrong column's word computes the right answer - it is the right
\ program, in the wrong column - and would be reported as that column's cost. So
\ before a habu column is measured, the call text it was built from is resolved
\ the way the engine resolves it and held against the address that column's
\ routine really starts at. A reference column has no single name to resolve and
\ is not checked this way; what stands for it there is the answer, which comes
\ from a different compiler on a different program.
: COLUMN-CK ( ptr u8 n n -- ) {: a:ptr u:n entry:n :}
   entry 0= if E-JUDGE-COST-COLUMN throw then
   a u NDICT:CALL-TARGET entry <> if
      s" judge: a column's body would have called " type a u type cr
      E-JUDGE-COST-COLUMN throw
   then ;

\ How a body disposes of what the subject left. A row's subject leaves as many
\ values as its stack comment declares, and a generated body has to account for
\ every one of them or the checker declines it. A TIMED body drops them all; a
\ VALUED body folds them into one cell with `xor`, so a row that leaves two
\ results is compared on both rather than on whichever happened to be on top.
: DROPS+ ( n -- ) {: outs:n :}
   outs 0 ?do s"  drop" TXT CODEGEN:APPEND-STRING loop ;

\ A DOUBLE and a FLAG are not cells a row can be compared on until they are
\ projected, and each is projected by the one word above that records that kind,
\ so two columns crossing from a float or a flag to a recorded cell by two
\ routes of their own cannot read as a difference in the compiled code. Neither
\ projection reaches past the value on top, so a subject
\ leaving one UNDER another value is refused rather than folded through a swap
\ nobody wrote: no corpus has that shape and inventing one here would be
\ inventing the program.
0 constant P-CELL
1 constant P-REAL
2 constant P-FLAG

: FOLD+ ( n n -- ) {: outs:n proj:n :}
   proj P-CELL = if
      outs 0= if s"  0" TXT CODEGEN:APPEND-STRING exit then
      outs 1- 0 ?do s"  xor" TXT CODEGEN:APPEND-STRING loop
      exit
   then
   outs 1 <> if E-JUDGE-COST-FOLD throw then
   proj P-REAL = if s"  JUDGE-COST:REAL-BITS" TXT CODEGEN:APPEND-STRING exit then
   proj P-FLAG = if s"  JUDGE-COST:FLAG-BITS" TXT CODEGEN:APPEND-STRING exit then
   E-JUDGE-COST-FOLD throw ;

\ Time one row's program in one column: the row's pinned inputs as text, the
\ text that consumes them, and how many values the subject leaves. Answers
\ picoseconds per call, measured by TIME-ONLY above and therefore by the
\ discipline every measured row of this repository is read under.
: TIME ( ptr u8 n ptr u8 n n -- n ) {: ia:ptr iu:n ca:ptr cu:n outs:n :}
   0 PICOS-CELL !
   OPEN+
   s" [: " TXT CODEGEN:APPEND-STRING
   ia iu TXT CODEGEN:APPEND-STRING
   s"  " TXT CODEGEN:APPEND-STRING
   ca cu TXT CODEGEN:APPEND-STRING
   outs DROPS+
   s"  ;] JUDGE-COST:TIME-ONLY JUDGE-COST:PICOS!" TXT CODEGEN:APPEND-STRING
   CLOSE+
   COMPILE-AND-RUN
   PICOS-CELL @ ;

\ The measurement's own floor: the timing word run over an EMPTY quotation. It
\ is what every habu column's number carries in common - the loop, the call into
\ the quotation, and the return - so subtracting it leaves the cost of the one
\ call the body makes. Both habu columns lose the same constant, so their
\ difference is untouched by it either way.
: FLOOR ( -- n )
   0 PICOS-CELL !
   OPEN+
   s" [: ;] JUDGE-COST:TIME-ONLY JUDGE-COST:PICOS!" TXT CODEGEN:APPEND-STRING
   CLOSE+
   COMPILE-AND-RUN
   PICOS-CELL @ ;

\ The value that same program computes, on the same pinned inputs, through the
\ same column. What makes a time a measurement of the right program.
: VALUE ( ptr u8 n ptr u8 n n n -- n )
   {: ia:ptr iu:n ca:ptr cu:n outs:n proj:n :}
   0 RESULT-CELL !
   OPEN+
   ia iu TXT CODEGEN:APPEND-STRING
   s"  " TXT CODEGEN:APPEND-STRING
   ca cu TXT CODEGEN:APPEND-STRING
   outs proj FOLD+
   s"  JUDGE-COST:RESULT!" TXT CODEGEN:APPEND-STRING
   CLOSE+
   COMPILE-AND-RUN
   RESULT-CELL @ ;

\ What the same program LEFT IN MEMORY, read back through the row's own reader
\ after the call and answered as one cell. It is a second number and not a fold
\ into the first, and that is the whole point: a subject's answer and the memory
\ it wrote are two observations, and combining them into one cell lets a column
\ that got both wrong the same way cancel out - CELL-BUMP answers exactly what
\ its cell holds, so the two folded together are identically zero and prove
\ nothing at all. Compared separately, neither can hide the other.
: WITNESS ( ptr u8 n ptr u8 n ptr u8 n n -- n )
   {: ia:ptr iu:n ca:ptr cu:n ra:ptr ru:n outs:n :}
   ru 0= if E-JUDGE-COST-WITNESS throw then
   0 RESULT-CELL !
   OPEN+
   ia iu TXT CODEGEN:APPEND-STRING
   s"  " TXT CODEGEN:APPEND-STRING
   ca cu TXT CODEGEN:APPEND-STRING
   outs DROPS+
   s"  " TXT CODEGEN:APPEND-STRING
   ra ru TXT CODEGEN:APPEND-STRING
   s"  JUDGE-COST:RESULT!" TXT CODEGEN:APPEND-STRING
   CLOSE+
   COMPILE-AND-RUN
   RESULT-CELL @ ;

;package
