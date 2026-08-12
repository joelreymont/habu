\ judge/cost.f - timing one row's program in one code generator's column, and
\ reading the value it computes. One concern: turning a row's pinned input TEXT
\ and one word's spelling into a measured program.
\
\ WHY THE BODY IS GENERATED. A timed body has to CALL the subject, and a call
\ names the word, and a body is compiled before it runs - so a body written by
\ hand in a source file can only name words that already exist. Two things
\ follow, and they are why the old comparison looks the way it does. First, a
\ subject the chain REFUSES has no word at all, so a hand-written body naming it
\ does not compile and the whole file fails to load: the old harness lives with
\ that by keeping a hand-written list of the refused subjects and simply not
\ writing bodies for them. Second, the same pinned input has to be retyped once
\ per column - tools/codegen-compare-cases4.f writes it for the engine,
\ tools/codegen-compare-new4.f writes it again for the chain, and
\ tools/codegen-compare-c4.f writes it a third time for the C twin - and three
\ copies of a number is three chances to time one column on a different program
\ from the other two.
\
\ Here the input is stated ONCE, as text, and a body is built from it for
\ whichever column is being measured. A refused subject simply has no body
\ built, which needs no list and no exception: nothing was ever written down
\ that has to be kept in step.
\
\ WHAT IS GENERATED, EXACTLY. Two shapes, and nothing else:
\
\   : <name> ( -- ) [: <inputs> <call> drop ;] TIME-ONLY PICOS! ;
\   : <name> ( -- ) <inputs> <call> RESULT! ;
\
\ `<inputs>` is the row's own text and `<call>` is the column's - the engine's
\ word, the chain's word, or the foreign-call shape that reaches the C twin. The
\ timed shape is the same one tools/codegen-compare-cases4.f writes by hand, so
\ what is timed here is what that harness times: a quotation that calls the
\ subject once and drops its answer.
\
\ AND THE GENERATOR CANNOT SILENTLY TIME THE WRONG PROGRAM. A generated body is
\ text, and text that compiles is not text that computes the right thing: an
\ input list with a number missing still compiles for some subjects, and a body
\ built against the wrong column's word compiles perfectly. So every row is also
\ VALUED, by the second shape above, in every column - and the columns must
\ agree. A generator that built one column's body wrong makes that column answer
\ something else, and the run fails on the answers rather than reporting a time
\ for a program nobody meant to run. tools/judge/cost-test.f hands the generator
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
require tools/codegen-compare-core.f

package JUDGE-COST

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

\ How many bodies this process has generated. A reader of a failed run wants it,
\ and tools/judge/cost-test.f asserts that a refused subject added none.
: BODIES ( -- n )
   SEQ @ ;

\ Time one row's program in one column: the row's pinned inputs as text, and the
\ text that consumes them and leaves the answer. Answers picoseconds per call,
\ measured the way every other row of this repository is measured - the
\ comparison harness's own timing word, which runs a fixed number of repetitions
\ a fixed number of times and keeps the fastest run.
: TIME ( ptr u8 n ptr u8 n -- n ) {: ia:ptr iu:n ca:ptr cu:n :}
   0 PICOS-CELL !
   OPEN+
   s" [: " TXT CODEGEN:APPEND-STRING
   ia iu TXT CODEGEN:APPEND-STRING
   s"  " TXT CODEGEN:APPEND-STRING
   ca cu TXT CODEGEN:APPEND-STRING
   s"  drop ;] CODEGEN-COMPARE:TIME-ONLY JUDGE-COST:PICOS!" TXT CODEGEN:APPEND-STRING
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
   s" [: ;] CODEGEN-COMPARE:TIME-ONLY JUDGE-COST:PICOS!" TXT CODEGEN:APPEND-STRING
   CLOSE+
   COMPILE-AND-RUN
   PICOS-CELL @ ;

\ The value that same program computes, on the same pinned inputs, through the
\ same column. What makes a time a measurement of the right program.
: VALUE ( ptr u8 n ptr u8 n -- n ) {: ia:ptr iu:n ca:ptr cu:n :}
   0 RESULT-CELL !
   OPEN+
   ia iu TXT CODEGEN:APPEND-STRING
   s"  " TXT CODEGEN:APPEND-STRING
   ca cu TXT CODEGEN:APPEND-STRING
   s"  JUDGE-COST:RESULT!" TXT CODEGEN:APPEND-STRING
   CLOSE+
   COMPILE-AND-RUN
   RESULT-CELL @ ;

;package
