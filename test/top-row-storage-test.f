\ top-row-storage-test.f - the interpret row types top-level storage as a colon
\ body does: a refused top-level store gets its colon-body twin's code, token
\ and reason with rc 70, and the cell keeps its old contents. Programs run
\ through `evaluate-closed`, the row --load keeps in this process; a child runs
\ only the boundaries named at their cases (a crash, a rewired search order, the
\ file label, the REPL).
require src/lib/error.f
require src/lib/test.f
require src/lib/string.f
require src/lib/test/eval.f
require src/lib/test/ending.f
require src/lib/test/subject.f
require src/lib/pty-harness.f
require src/lib/engine-candidate.f

\ File-global: the programs compile at run time in the scope a real program
\ writes them in.
variable TS-V
create TS-B 8 allot
: TS-BN ( -- ^n ) TS-B ;
: TS-BC ( -- ^u8 ) TS-B ;
PTR-VARIABLE TS-PV
TYPED-VARIABLE TS-HK [ -- ]
: TS-X ( -- ) ;
: TS-F2 ( n -- n n ) dup ;
: TS-RR ( r -- ) drop ;
: TS-NN ( n -- ) drop ;
TYPED-VARIABLE TS-SV str
: TS-SV-LEN ( -- n ) TS-SV @ str> nip ;
: TS-SV-SET ( -- ) s" xyz" TS-SV ! ;

package TS-NT
public
NEWTYPE tk
private
CAST: >TK ( n -- tk )
public
: MK ( -- tk ) 5 >TK ;
;package

\ TS-R1 leaves a dead segment two levels deep on a pool stack; TS-R2's text gets that
\ stack one level deep, and TS-EVD under its catch must not select the dead segment.
TRUSTED: TS-EVD ( -- ) s" 1 drop" str> evaluate ;
: TS-R1 ( -- ) s" TS-EVD" evaluate-closed ;
: TS-R2 ( -- ) s" 5 ' TS-EVD catch drop TS-NN" evaluate-closed ;
TRUSTED: TS-Z ( -- ) s" TS-R1" str> evaluate TS-R2 ;

package TOP-ROW-STORAGE-TEST
using PTY-HARNESS

private

9 constant SIB-RC                    \ the sibling evaluation's own throw


$4000 constant IO-CAP
create OUT IO-CAP allot
create ERR IO-CAP allot
variable OUT-U
variable ERR-U

: ARM ( -- )   \ capture the next diagnostic as JSON instead of printing it
   REJECTION:CAPTURE  true REJECTION:JSON ! ;

: DISARM ( -- )
   false REJECTION:JSON !  REJECTION:BUFFER-OFF ;

: HAS? ( str -- ) {: s :}
   REJECTION:BUFFER$ s CONTAINS? TTRUE ;

: CLEAR ( -- )   \ every cell a refused store would have written
   0 TS-V !  0 TS-BN !  NULL-PTR TS-PV ! ;

: UNTOUCHED ( -- )
   TS-V @ 0 T=  TS-BN @ 0 T=  TS-BC c@ 0 T=  TS-PV @ NULL-PTR = TTRUE ;

: HERE-N ( -- n )
   here NULL-PTR - ;

\ ---- the diagnostics a colon body gives -------------------------------------

: MISMATCH ( -- )
   S\" \"code\":\"E-MISMATCH\"" HAS? ;

: RAW-PTR ( -- )
   S\" \"code\":\"E-RAW-CELL-PTR\"" HAS?
   S\" \"message\":\"raw storage cell: a pointer cannot be stored in or fetched from an undeclared cell\"" HAS? ;

: RAW-FAMILY ( -- )
   S\" \"message\":\"raw storage cell: an undeclared cell holds only a plain number, flag or character, never a nominal value\"" HAS?
   S\" \"help\":\"keep the value in storage that names its type, `TYPED-VARIABLE NAME t` or `TYPED-BUFFER NAME t`; for `constant`, define a word that leaves it, `: NAME ( -- t ) ... ;`\"" HAS? ;

: RAW-XT ( -- )
   S\" \"code\":\"E-RAW-CELL-PTR\"" HAS?
   S\" \"message\":\"raw storage cell: an undeclared cell cannot hold an execution token / a quotation\"" HAS? ;

: AT-COMMA ( -- )   S\" \"token\":\",\"" HAS? ;
: AT-STORE ( -- )   S\" \"token\":\"!\"" HAS? ;
: AT-ADD ( -- )     S\" \"token\":\"+!\"" HAS? ;
: AT-CSTORE ( -- )  S\" \"token\":\"c!\"" HAS? ;
: AT-CONSTANT ( -- ) S\" \"token\":\"constant\"" HAS? ;
: GOT-PTR ( -- )    S\" \"found\":\"^" HAS? ;
: GOT-XT ( -- )     S\" \"found\":\"xt" HAS? ;

\ ---- the refused programs, each beside its colon-body twin --------------------

: CASE-CREATE-XT ( -- )
   s" create T ' DUP , : the xt is refused as in a body, the `,` lays nothing" T-LABEL
   ARM
   [: s" : TS-B1 ( -- ) ['] DUP , ;" evaluate-closed ;] 70 TTHROWSQ
   MISMATCH AT-COMMA GOT-XT
   HERE-N s" create TS-T3B" evaluate-closed HERE-N swap -
   ARM
   HERE-N [: s" create TS-T3 ' DUP ," evaluate-closed ;] 70 TTHROWSQ
   MISMATCH AT-COMMA GOT-XT
   HERE-N swap - swap T=
   DISARM ;

: CASE-CREATE-HERE ( -- )
   s" create T here , : the pointer is refused as in a body, the `,` lays nothing" T-LABEL
   ARM
   [: s" : TS-B2 ( -- ) here , ;" evaluate-closed ;] 70 TTHROWSQ
   MISMATCH AT-COMMA GOT-PTR
   HERE-N s" create TS-T4B" evaluate-closed HERE-N swap -
   ARM
   HERE-N [: s" create TS-T4 here ," evaluate-closed ;] 70 TTHROWSQ
   MISMATCH AT-COMMA GOT-PTR
   HERE-N swap - swap T=
   DISARM ;

: CASE-VARIABLE-HERE ( -- )
   s" variable V here V ! : refused by the raw-cell rule, V stays 0" T-LABEL
   ARM
   [: s" : TS-B3 ( -- ) here TS-V ! ;" evaluate-closed ;] 70 TTHROWSQ
   RAW-PTR AT-STORE
   ARM
   [: s" variable TS-NV here TS-NV !" evaluate-closed ;] 70 TTHROWSQ
   RAW-PTR AT-STORE
   DISARM ;                 \ the refused program's TS-NV loses its name

: CASE-VARIABLE-XT ( -- )
   s" variable W ' DUP W ! : refused by the raw-cell xt rule, W stays 0" T-LABEL
   ARM
   [: s" : TS-B4 ( -- ) ['] DUP TS-V ! ;" evaluate-closed ;] 70 TTHROWSQ
   RAW-XT AT-STORE
   ARM
   [: s" variable TS-NW ' DUP TS-NW !" evaluate-closed ;] 70 TTHROWSQ
   RAW-XT AT-STORE
   DISARM ;                 \ the refused program's TS-NW loses its name

: CASE-CONSTANT-PTR ( -- )
   s" here constant C : refused by the raw-cell rule, as here V ! is" T-LABEL
   ARM
   [: s" here constant TS-HC" evaluate-closed ;] 70 TTHROWSQ
   RAW-PTR AT-CONSTANT
   DISARM ;

: CASE-CONSTANT-XT ( -- )
   s" ' DUP constant C : refused by the raw-cell xt rule, as ' DUP V ! is" T-LABEL
   ARM
   [: s" ' DUP constant TS-XC" evaluate-closed ;] 70 TTHROWSQ
   RAW-XT AT-CONSTANT
   DISARM ;

: CASE-CONSTANT-FAMILY ( -- )
   s" a NEWTYPE value to constant is refused, as one stored into V is" T-LABEL
   ARM
   [: s" TS-NT:MK constant TS-NC" evaluate-closed ;] 70 TTHROWSQ
   MISMATCH AT-CONSTANT  S\" \"found\":\"ts-nt:tk\"" HAS?  RAW-FAMILY
   ARM
   [: s" variable TS-NV TS-NT:MK TS-NV !" evaluate-closed ;] 70 TTHROWSQ
   MISMATCH AT-STORE  RAW-FAMILY
   DISARM ;                 \ the refused program's TS-NV loses its name

: CASE-TYPED-N-PTR ( -- )
   s" here BN ! through a ( -- ^n ) accessor is refused as in a body" T-LABEL
   CLEAR ARM
   [: s" : TS-B5 ( -- ) here TS-BN ! ;" evaluate-closed ;] 70 TTHROWSQ
   MISMATCH AT-STORE GOT-PTR
   ARM
   [: s" here TS-BN !" evaluate-closed ;] 70 TTHROWSQ
   MISMATCH AT-STORE GOT-PTR
   DISARM UNTOUCHED ;

: CASE-TYPED-N-XT ( -- )
   s" an xt through the same accessor is refused as in a body" T-LABEL
   CLEAR ARM
   [: s" : TS-B6 ( -- ) ['] DUP TS-BN ! ;" evaluate-closed ;] 70 TTHROWSQ
   MISMATCH AT-STORE GOT-XT
   ARM
   [: s" ' DUP TS-BN !" evaluate-closed ;] 70 TTHROWSQ
   MISMATCH AT-STORE GOT-XT
   DISARM UNTOUCHED ;

: CASE-ADD-STORE ( -- )
   s" pointer and xt operands to BN +! are refused as in a body" T-LABEL
   CLEAR ARM
   [: s" : TS-B7 ( -- ) here TS-BN +! ;" evaluate-closed ;] 70 TTHROWSQ
   MISMATCH AT-ADD GOT-PTR
   ARM
   [: s" here TS-BN +!" evaluate-closed ;] 70 TTHROWSQ
   MISMATCH AT-ADD GOT-PTR
   ARM
   [: s" : TS-B8 ( -- ) ['] DUP TS-BN +! ;" evaluate-closed ;] 70 TTHROWSQ
   MISMATCH AT-ADD GOT-XT
   ARM
   [: s" ' DUP TS-BN +!" evaluate-closed ;] 70 TTHROWSQ
   MISMATCH AT-ADD GOT-XT
   DISARM UNTOUCHED ;

: CASE-BYTE-STORE ( -- )
   s" pointer and xt operands to c! through ( -- ^u8 ) are refused as in a body" T-LABEL
   CLEAR ARM
   [: s" : TS-B9 ( -- ) here TS-BC c! ;" evaluate-closed ;] 70 TTHROWSQ
   MISMATCH AT-CSTORE GOT-PTR
   ARM
   [: s" here TS-BC c!" evaluate-closed ;] 70 TTHROWSQ
   MISMATCH AT-CSTORE GOT-PTR
   ARM
   [: s" : TS-B10 ( -- ) ['] DUP TS-BC c! ;" evaluate-closed ;] 70 TTHROWSQ
   MISMATCH AT-CSTORE GOT-XT
   ARM
   [: s" ' DUP TS-BC c!" evaluate-closed ;] 70 TTHROWSQ
   MISMATCH AT-CSTORE GOT-XT
   DISARM UNTOUCHED ;

: CASE-POINTER-SUM ( -- )
   S\" s\" abc\" str> + . : a byte pointer into a scalar consumer is refused as in a body" T-LABEL
   ARM
   [: S\" : TS-B11 ( -- ) s\" abc\" str> + . ;" evaluate-closed ;] 70 TTHROWSQ
   MISMATCH S\" \"token\":\".\"" HAS?  S\" \"found\":\"^u8\"" HAS?
   ARM
   [: S\" s\" abc\" str> + ." evaluate-closed ;] 70 TTHROWSQ
   MISMATCH S\" \"token\":\".\"" HAS?  S\" \"found\":\"^u8\"" HAS?
   DISARM ;

: CASE-REAL-TO-N ( -- )
   s" 0.5 N with N ( n -- ) : a real into a scalar consumer is refused as in a body" T-LABEL
   ARM
   [: s" : TS-B14 ( -- ) 0.5 TS-NN ;" evaluate-closed ;] 70 TTHROWSQ
   MISMATCH S\" \"token\":\"TS-NN\"" HAS?  S\" \"found\":\"r\"" HAS?
   ARM
   [: s" 0.5 TS-NN" evaluate-closed ;] 70 TTHROWSQ
   MISMATCH S\" \"token\":\"TS-NN\"" HAS?  S\" \"found\":\"r\"" HAS?
   DISARM ;

\ The row knows a TRUSTED: body's declared effect, not the cells it leaves: it
\ rebuilds them as unknown, and a typed consumer refuses an unknown cell.
: CASE-LEFTOVER ( -- )
   s" a cell a TRUSTED: body leaves past its effect is refused at its next typed consumer" T-LABEL
   CLEAR ARM
   [: s" TRUSTED: TS-L1 ( -- ) here ;  TS-L1 TS-PV !" evaluate-closed ;] 70 TTHROWSQ
   S\" \"code\":\"E-STALE-READ\"" HAS?  AT-STORE
   S\" \"message\":\"unknown cell: this reads a cell beneath the cells the interpret row knows" HAS?
   DISARM UNTOUCHED ;

\ A top-level rejection names the file, line and column, in prose and JSON. A
\ string `evaluate` runs lies outside the file's bytes, so its rejection is
\ placed at the `evaluate` in the file, with the string's line in an evaluated
\ note. The load holds the resolved path, so the assertions match its tail.
: INCLUDE-SITE ( -- )
   [: s" include test/fixtures/top-row-site.f" evaluate-closed ;] 70 TTHROWSQ ;

: INCLUDE-EVAL ( -- )
   [: s" include test/fixtures/top-row-eval.f" evaluate-closed ;] 70 TTHROWSQ ;

: CASE-SITE ( -- )
   s" a top-level refusal names its file and line" T-LABEL
   CLEAR  REJECTION:CAPTURE
   INCLUDE-SITE
   s" /test/fixtures/top-row-site.f:5:11" HAS?  s" error[E-RAW-CELL-PTR]" HAS?
   REJECTION:BUFFER-OFF
   ARM  INCLUDE-SITE
   S\" /test/fixtures/top-row-site.f\",\"line\":5,\"column\":11," HAS?  RAW-PTR
   DISARM UNTOUCHED ;

: CASE-EVAL-SITE ( -- )
   s" a top-level rejection in an evaluated string names its evaluate and the string's line" T-LABEL
   CLEAR  REJECTION:CAPTURE
   INCLUDE-EVAL
   s" /test/fixtures/top-row-eval.f:5:" HAS?  s" error[E-RAW-CELL-PTR]" HAS?
   s" = evaluated: here TS-V !" HAS?  s" pointer through an untyped cell" HAS?
   REJECTION:BUFFER-OFF
   ARM  INCLUDE-EVAL
   S\" /test/fixtures/top-row-eval.f\",\"line\":5," HAS?
   S\" \"token\":\"evaluate\"" HAS?
   S\" \"evaluated\":{\"source_line\":\"here TS-V !\"" HAS?  RAW-PTR
   DISARM UNTOUCHED ;

\ ---- children -----------------------------------------------------------------

: CHILD ( str -- PROCESS:ending ) {: src :}
   src OUT IO-CAP >LEN ERR IO-CAP >LEN PROCESS:WAIT-FOREVER SUBJECT:RUN
   {: outu:len erru:len oc :}
   outu LEN>N OUT-U !  erru LEN>N ERR-U !  oc ;

: OUT$ ( -- str )   OUT OUT-U @ >str ;
: ERR$ ( -- str )   ERR ERR-U @ >str ;

: EXITS ( str n -- ) {: src want:n :}
   src CHILD {: oc :}
   src OUT$ ERR$ oc want T-ENDING-EXITED= ;

: ERR-HAS? ( str -- ) {: s :}   ERR$ s CONTAINS? TTRUE ;
: ERR-LACKS? ( str -- ) {: s :}   ERR$ s CONTAINS? TFALSE ;
: OUT-HAS? ( str -- ) {: s :}   OUT$ s CONTAINS? TTRUE ;

\ A child: unchecked, this program reads below the closed floor or branches to
\ xt 0, which crashes the process.
: CASE-EXECUTE-UNDERFLOW ( -- )
   s" ' F execute with F's input missing is refused as in a body" T-LABEL
   ARM
   [: s" : TS-B12 ( -- ) ['] TS-F2 execute ;" evaluate-closed ;] 70 TTHROWSQ
   S\" \"code\":\"E-INPUT-UNDERFLOW\"" HAS?  S\" \"token\":\"execute\"" HAS?
   DISARM
   s" true REJECTION:JSON ! ' TS-F2 execute" 70 EXITS
   S\" \"code\":\"E-INPUT-UNDERFLOW\"" ERR-HAS?  S\" \"token\":\"execute\"" ERR-HAS? ;

: CASE-CATCH-NON-XT ( -- )
   s" 0 0 catch hands catch a number and is refused as in a body" T-LABEL
   ARM
   [: s" : TS-B13 ( -- ) 0 0 catch ;" evaluate-closed ;] 70 TTHROWSQ
   S\" \"code\":\"E-MISMATCH\"" HAS?  S\" \"token\":\"catch\"" HAS?
   DISARM
   s" true REJECTION:JSON ! 0 0 catch" 70 EXITS
   S\" \"code\":\"E-MISMATCH\"" ERR-HAS?  S\" \"token\":\"catch\"" ERR-HAS?
   S\" \"found\":\"n\"" ERR-HAS? ;

\ A child: unchecked, this program branches to the address `here` the TRUSTED:
\ body left, which crashes the process.
: CASE-EXECUTE-UNKNOWN ( -- )
   s" execute of a cell a TRUSTED: body left names the unknown cell" T-LABEL
   s" true REJECTION:JSON ! TRUSTED: TS-L2 ( -- ) here ;  TS-L2 execute" 70 EXITS
   S\" \"code\":\"E-STALE-READ\"" ERR-HAS?  S\" \"token\":\"execute\"" ERR-HAS? ;

\ A child: unchecked, `evaluate` would read a string from whatever the stack
\ holds. `'` of a word with no stack effect pushes an xt of no known effect.
: CASE-EXECUTE-OPAQUE-TICK ( -- )
   s" execute of ' of a word with no stack effect names the opaque xt" T-LABEL
   s" true REJECTION:JSON ! ' evaluate execute" 70 EXITS
   S\" \"code\":\"E-EXEC-OPAQUE-XT\"" ERR-HAS?  S\" \"token\":\"execute\"" ERR-HAS? ;

\ A child: unchecked, the second `2r>` pops into the return frame of the code
\ running the program, which crashes the process. The row carries only the two
\ cells `2>r` pushed, so reading below them is refused with a body's
\ return-stack reason.
: CASE-RETURN-BELOW ( -- )
   s" r> below a body's return frame names the return-stack reason" T-LABEL
   REJECTION:CAPTURE
   [: s" : TS-B15 ( -- ) r> drop ;" evaluate-closed ;] 70 TTHROWSQ
   s" error[E-RETURN-STACK]: return stack: this reads below the return-stack cells pushed here" HAS?
   s" = evaluated: : TS-B15 ( -- ) r> drop ;" HAS?
   REJECTION:BUFFER-OFF
   s" 2r> below the saved return cells is refused with the same reason" T-LABEL
   s" 1 2 2>r 2r> 2r> 2drop" 70 EXITS
   s" error[E-RETURN-STACK]: return stack: this reads below the return-stack cells pushed here" ERR-HAS?
   s" = expected: a b" ERR-HAS? ;

\ A signature naming one row as data and after `|` binds its caller's return
\ base at the call, with no read token: the seal refuses the bound base
\ (CHECK-NO-BORROW, QUOT-RSEAL), pinned at the last token walked. A child for
\ the run: unchecked, TS-B17's r@ reads below its return frame, which crashes
\ the process.
: CASE-RETURN-BOUND ( -- )
   s" a call binding the return base with no read is refused by the seal" T-LABEL
   ARM
   [: s" : TS-U1 ( U | U -- U | U ) ; : TS-B16 ( n -- n ) TS-U1 ;" evaluate-closed ;]
   70 TTHROWSQ
   S\" \"code\":\"E-RETURN-STACK\"" HAS?  S\" \"token\":\"TS-U1\"" HAS?
   DISARM
   s" r@ over a return base a call bound is refused by the seal, never run" T-LABEL
   s" true REJECTION:JSON ! : TS-U2 ( U | U -- U | U ) ; : TS-B17 ( n -- n n ) TS-U2 r@ ; : TS-T7 ( -- n ) 5 TS-B17 drop ; TS-T7 . cr"
   70 EXITS
   S\" \"code\":\"E-RETURN-STACK\"" ERR-HAS?  S\" \"token\":\"r@\"" ERR-HAS?
   s" a literal binding its return base with no read is refused at ;]" T-LABEL
   ARM
   [: s" : TS-U3 ( U | U -- U | U ) ; : TS-B18 ( -- ) [: 5 TS-U3 r@ drop drop ;] drop ;" evaluate-closed ;]
   70 TTHROWSQ
   S\" \"code\":\"E-RETURN-STACK\"" HAS?  S\" \"token\":\";]\"" HAS?
   DISARM
   s" a literal reading its caller's pushed cell through the bound base is refused at ;]" T-LABEL
   ARM
   [: s" : TS-U4 ( U | U -- U | U ) ; : TS-B19 ( -- n ) 5 >r [: 7 TS-U4 r@ swap drop ;] execute r> drop ;" evaluate-closed ;]
   70 TTHROWSQ
   S\" \"code\":\"E-RETURN-STACK\"" HAS?  S\" \"token\":\";]\"" HAS?
   DISARM ;

\ A child: REJECTION:LABEL! has no inverse; the label stays on this test file for
\ every later case. A step in an evaluated buffer is placed at the word in this
\ file that ran it, so its rejection names the label, with the step in an
\ evaluated note.
: CASE-LABEL ( -- )
   s" a tool's file label names an evaluated step's rejection at the word that ran it" T-LABEL
   S\" s\" /subject/label.f\" REJECTION:LABEL! true REJECTION:JSON ! here TS-V !"
   70 EXITS
   S\" \"file\":\"/subject/label.f\"" ERR-HAS?
   S\" \"evaluated\":{\"source_line\":\"s\\\" /subject/label.f" ERR-HAS?
   S\" \"code\":\"E-RAW-CELL-PTR\"" ERR-HAS? ;

\ A child, as CASE-LABEL is. A candidate text is in no file, so its rejection
\ is placed at <input>, never at the label.
: CASE-LABEL-CANDIDATE ( -- )
   s" a tool's file label stays off a candidate text's rejection" T-LABEL
   S\" s\" /subject/label.f\" REJECTION:LABEL! true REJECTION:JSON ! s\" TS-LC ( -- ) TS-LC-UNDEFINED\" CHECK-CANDIDATE! drop"
   0 EXITS
   S\" \"file\":\"<input>\"" ERR-HAS?
   S\" \"code\":\"E-UNDEFINED\"" ERR-HAS? ;

\ A child: unchecked, `c@` reads through a cell TS-L3 left, which crashes the
\ process. The literal pads TS-L3's cells as unknown (CHECKER-TOP-PUSH).
: CASE-SHORT-ROW ( -- )
   s" a literal over a row shorter than the stack leaves the cells beneath unknown" T-LABEL
   S\" true REJECTION:JSON ! TRUSTED: TS-EV2 ( str -- ) str> evaluate 2drop ; TRUSTED: TS-L3 ( -- ) 0 0 ; s\\\" TS-L3 s\\\" abc\\\"\" TS-EV2 drop c@ . cr"
   70 EXITS
   S\" \"code\":\"E-STALE-READ\"" ERR-HAS?  S\" \"token\":\"c@\"" ERR-HAS? ;

\ A child, whose `using` outlives its program: a used public retired by
\ `undefine` resolves past it to the global or the live public, as the engine does.
: SHADOW ( -- )
   s" package TS-SHADOW public : drop ( n -- ) . ; undefine drop ;package using TS-SHADOW 1 drop " SB-APPEND ;
: AMBIG ( -- )
   s" package TS-AMB-A public : TS-AMB ( n -- ) drop ; ;package " SB-APPEND
   s" package TS-AMB-B public : TS-AMB ( n -- ) drop ; undefine TS-AMB ;package " SB-APPEND
   s" using TS-AMB-A using TS-AMB-B 1 TS-AMB " SB-APPEND ;
: AFTER ( -- )   S\" s\" AFTER\" type cr" SB-APPEND ;
: P3 ( -- )   S\" true REJECTION:JSON ! s\" abc\" str> + . cr" SB-APPEND ;

: RUNS-AFTER ( str -- ) {: code :}
   SB$ 0 EXITS  s" AFTER" OUT-HAS?  code ERR-LACKS? ;

: REFUSES-P3 ( str -- ) {: code :}
   SB$ 70 EXITS
   S\" \"code\":\"E-MISMATCH\"" ERR-HAS?  S\" \"token\":\".\"" ERR-HAS?
   code ERR-LACKS? ;

: CASE-SCOPES ( -- )
   s" a retired used public shadowing a global: 1 drop runs, rc 0" T-LABEL
   SB-RESET SHADOW AFTER  s" E-USING-SHADOW-GLOBAL" RUNS-AFTER
   s" a retired used public shadowing a global: p3 is refused by its own diagnostic" T-LABEL
   SB-RESET SHADOW P3  s" E-USING-SHADOW-GLOBAL" REFUSES-P3
   s" two used publics of one tail, one retired: 1 TS-AMB runs, rc 0" T-LABEL
   SB-RESET AMBIG AFTER  s" ambiguous" RUNS-AFTER
   s" two used publics of one tail, one retired: p3 is refused by its own diagnostic" T-LABEL
   SB-RESET AMBIG P3  s" ambiguous" REFUSES-P3 ;

\ ---- the admitted programs ----------------------------------------------------

: CASE-NUMERIC ( -- )
   s" numeric stores through raw cells and accessors are admitted and run" T-LABEL
   CLEAR
   [: s" 5 TS-V !  1 TS-V +!  7 TS-BN !  2 TS-BN +!  65 TS-BC c!  create TS-T6 3 ," evaluate-closed ;]
      0 TTHROWSQ
   TS-V @ 6 T=  TS-BC c@ 65 T=
   s" TS-T6 @" TEST-EVAL:N 3 T= ;

: CASE-REAL ( -- )
   s" a float literal is a real at top level, as in a body" T-LABEL
   [: s" : TS-B9 ( -- ) 0.5 TS-RR ;" evaluate-closed ;] 0 TTHROWSQ
   [: s" 0.5 TS-RR  -1.25 TS-RR" evaluate-closed ;] 0 TTHROWSQ ;

: CASE-CONSTANT-ADMITTED ( -- )
   s" bool and role operands to constant are admitted and kept" T-LABEL
   [: s" true constant TS-KB  5 >LEN constant TS-KL" evaluate-closed ;] 0 TTHROWSQ
   s" TS-KB" TEST-EVAL:N -1 T=
   s" TS-KL" TEST-EVAL:N 5 T= ;

: CASE-DECLARED ( -- )
   s" a PTR-VARIABLE takes a pointer and a TYPED-VARIABLE [ -- ] an xt" T-LABEL
   CLEAR
   [: s" here TS-PV !  ' TS-X TS-HK !" evaluate-closed ;] 0 TTHROWSQ
   TS-PV @ NULL-PTR <> TTRUE ;

: CASE-NESTED-EVAL ( -- )
   s" a nested closed evaluation answers its own cell" T-LABEL
   [: S\" s\" 1 2 +\" TEST-EVAL:N 3 T=" evaluate-closed ;] 0 TTHROWSQ
   s" a word evaluating text while it runs keeps the row its step left" T-LABEL
   [: S\" TRUSTED: TS-EV ( -- ) s\" 1 drop\" str> evaluate ;  ' TS-EV catch TS-NN" evaluate-closed ;]
      0 TTHROWSQ
   s" a segment a finished deeper evaluation left is not selected" T-LABEL
   [: s" TS-Z" evaluate-closed ;] 0 TTHROWSQ ;

: CASE-TICKED ( -- )
   s" ' X catch and ' X execute are admitted" T-LABEL
   [: s" ' TS-X catch drop  ' TS-X execute" evaluate-closed ;] 0 TTHROWSQ ;

: CASE-RETURN-ROW ( -- )
   s" 11 22 2>r 2r> - pops the pushed return cells in order" T-LABEL
   CLEAR
   [: s" 11 22 2>r 2r> - TS-V !" evaluate-closed ;] 0 TTHROWSQ
   TS-V @ -11 T= ;

\ A `str` is one value of two cells: a top-level `!` stores both and `@` fetches
\ both, where a body's fetch and store find them.
: CASE-WIDE-STORAGE ( -- )
   s" a str stored at top level leaves no cell and is a body's whole value" T-LABEL
   CLEAR
   [: S\" s\" abcde\" TS-SV !" evaluate-closed ;] 0 TTHROWSQ
   TS-SV-LEN 5 T=
   s" a str a body stored is fetched whole at top level" T-LABEL
   TS-SV-SET
   [: s" TS-SV @ str> nip TS-V !  TS-SV @ str> drop c@ TS-BC c!" evaluate-closed ;] 0 TTHROWSQ
   TS-V @ 3 T=  TS-BC c@ 120 T= ;

: CASE-ACROSS-COLON ( -- )
   s" a pointer kept across a : definition is stored typed" T-LABEL
   CLEAR
   [: s" here : TS-MID ( -- ) ; TS-PV !" evaluate-closed ;] 0 TTHROWSQ
   TS-PV @ NULL-PTR <> TTRUE ;

: CASE-ACROSS-REQUIRE ( -- )
   s" a pointer kept across require is stored typed" T-LABEL
   CLEAR
   [: s" here require src/lib/le.f TS-PV !" evaluate-closed ;] 0 TTHROWSQ
   TS-PV @ NULL-PTR <> TTRUE ;

: SIBLINGS ( -- )
   [: s" here 9 throw" evaluate-closed ;] catch SIB-RC T=
   s" here TS-PV !" evaluate-closed ;

: CASE-SIBLINGS ( -- )
   s" two sibling closed evaluations in one word, the first throwing" T-LABEL
   CLEAR
   ['] SIBLINGS 0 TTHROWSQ
   TS-PV @ NULL-PTR <> TTRUE ;

\ The first text's row ends `^^n` when it throws; its sibling starts on
\ the same stack base with no cells, so its literals replace that row: a row
\ that kept it would hand `+` two pointers.
: SIBLING-LITERALS ( -- )
   [: s" here here 1 E-DIV-ZERO throw" evaluate-closed ;] catch E-DIV-ZERO T=
   s" 3 4 + TS-V !" evaluate-closed ;

: CASE-SIBLING-LITERALS ( -- )
   s" literals after a sibling's throw drop its row, then a raw store takes their sum" T-LABEL
   CLEAR
   ['] SIBLING-LITERALS 0 TTHROWSQ
   TS-V @ 7 T= ;

: CASE-TRUSTED ( -- )
   s" a TRUSTED: word, the CHECK! probe and a TRUSTED: shim are admitted" T-LABEL
   [: s" TRUSTED: TS-PASSES? ( str -- bool ) drop 0 0= ;" evaluate-closed ;] 0 TTHROWSQ
   [: S\" s\" some source\" TS-PASSES? drop" evaluate-closed ;] 0 TTHROWSQ
   [: S\" s\" : TS-G ( n -- n ) 1 + ;\" CHECK! drop" evaluate-closed ;] 0 TTHROWSQ
   [: s" TRUSTED: TS-SHIM ( n n -- n ) + ;  1 2 TS-SHIM drop" evaluate-closed ;] 0 TTHROWSQ ;

\ A child on a terminal: only the REPL's line loop recovers from a throw and
\ reads the next line.
: STEP ( str -- bool )
   BUF-CLEAR SEND-LINE WAIT-ANSWER ;

: CASE-REPL ( -- )
   s" a refused REPL line names no file or definition" T-LABEL
   ENGINE-CANDIDATE:PATH$ SPAWN-ON-PTY
   s" habu> " WAIT-FOR TTRUE
   s" variable TS-RV  here TS-RV !" STEP TFALSE
   s" error[E-RAW-CELL-PTR]: raw storage cell" IN-BUF? TTRUE
   s" a REPL line that throws, then a fresh line storing a pointer typed" T-LABEL
   s" PTR-VARIABLE TS-RP" STEP TTRUE
   s" here 7 throw" STEP TFALSE
   s" here TS-RP !  42 ." STEP TTRUE
   s\" 42\r\n" IN-BUF? TTRUE
   s" E-" IN-BUF? TFALSE
   4 SEND-BYTE
   REAP {: oc :}
   ENGINE-CANDIDATE:PATH$ BUF$ s" " oc 0 T-ENDING-EXITED=
   CLOSE-MASTER ;

public

: RUN ( -- )
   T-RESET
   CASE-CREATE-XT
   CASE-CREATE-HERE
   CASE-VARIABLE-HERE
   CASE-VARIABLE-XT
   CASE-CONSTANT-PTR
   CASE-CONSTANT-XT
   CASE-CONSTANT-FAMILY
   CASE-TYPED-N-PTR
   CASE-TYPED-N-XT
   CASE-ADD-STORE
   CASE-BYTE-STORE
   CASE-POINTER-SUM
   CASE-REAL-TO-N
   CASE-LEFTOVER
   CASE-SITE
   CASE-EVAL-SITE
   CASE-LABEL
   CASE-LABEL-CANDIDATE
   CASE-EXECUTE-UNDERFLOW
   CASE-CATCH-NON-XT
   CASE-EXECUTE-OPAQUE-TICK
   CASE-EXECUTE-UNKNOWN
   CASE-RETURN-BELOW
   CASE-RETURN-BOUND
   CASE-SHORT-ROW
   CASE-SCOPES
   CASE-NUMERIC
   CASE-CONSTANT-ADMITTED
   CASE-REAL
   CASE-DECLARED
   CASE-NESTED-EVAL
   CASE-TICKED
   CASE-RETURN-ROW
   CASE-WIDE-STORAGE
   CASE-ACROSS-COLON
   CASE-ACROSS-REQUIRE
   CASE-SIBLINGS
   CASE-SIBLING-LITERALS
   CASE-TRUSTED
   CASE-REPL
   T-REPORT ;

;using
;package

TOP-ROW-STORAGE-TEST:RUN
