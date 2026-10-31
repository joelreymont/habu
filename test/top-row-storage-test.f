\ top-row-storage-test.f - the interpret row types top-level storage as a colon
\ body does: a refused top-level store
\ gets its colon-body twin's code, token and reason with rc 70, and the cell
\ keeps its old contents. Programs run through `evaluate-closed`, the row --load
\ keeps in this process; a child runs only the boundaries named at their cases
\ (a crash, a rewired search order or check hook, the file label, the REPL).
require src/lib/errors.f
require src/lib/test.f
require src/lib/string.f
require src/lib/memory.f
require src/lib/test/eval.f
require src/lib/test/outcome.f
require src/lib/test/subject.f
require src/lib/pty-harness.f
require src/lib/engine-candidate.f

\ File-global: the programs compile at run time in the scope a real program
\ writes them in.
variable TS-V
create TS-B 8 allot
: TS-BN ( -- ptr n ) TS-B ;
: TS-BC ( -- ptr u8 ) TS-B ;
PTR-VARIABLE TS-PV
TYPED-VARIABLE TS-HK [ -- ]
: TS-X ( -- ) ;
: TS-F2 ( n -- n n ) dup ;
: TS-RR ( r -- ) drop ;
: TS-NN ( n -- ) drop ;

\ A finished evaluation's segment on a borrowed stack, entered two levels deep
\ (TS-R1 under TS-Z's `evaluate`), and a later one-level segment whose catch
\ runs TS-EVD again: the dead deeper segment must not displace the live one,
\ which holds the 5 TS-NN reads.
PTR-VARIABLE TS-POOL-A
STACK-ABI:PAGE-BYTES MEM:ALLOC-GUARDED constant TS-POOL-CAP  TS-POOL-A !
: TS-POOL ( -- ptr u8 ) TS-POOL-A @ ;
TRUSTED: TS-EVD ( -- ) s" 1 drop" evaluate ;
: TS-R1 ( -- ) ['] TS-EVD TS-POOL TS-POOL-CAP run-in-stack ;
TRUSTED: TS-EVT ( -- ) s" 5 ' TS-EVD catch drop TS-NN" evaluate ;
: TS-R2 ( -- ) ['] TS-EVT TS-POOL TS-POOL-CAP run-in-stack ;
TRUSTED: TS-Z ( -- ) s" TS-R1" evaluate TS-R2 ;

package TOP-ROW-STORAGE-TEST
using PTY-HARNESS

private

70 constant CHECK-RC                 \ the engine refusing what it cannot certify
9 constant SIB-RC                    \ the sibling evaluation's own throw

create DIAG-BUF 8192 allot
8192 constant DIAG-CAP

$4000 constant IO-CAP
10000 constant CHILD-MS
create OUT IO-CAP allot
create ERR IO-CAP allot
variable OUT-U
variable ERR-U

: ARM ( -- )   \ capture the next diagnostic as JSON instead of printing it
   DIAG-BUF DIAG-CAP DIAG-BUFFER!  true DIAG-JSON! ;

: DISARM ( -- )
   false DIAG-JSON!  DIAG-BUFFER-OFF ;

: HAS? ( ptr u8 n -- )
   DIAG-BUFFER$ 2swap CONTAINS? TTRUE ;

: CLEAR ( -- )   \ every cell a refused store would have written
   0 TS-V !  0 TS-BN !  NULL-PTR TS-PV ! ;

: UNTOUCHED ( -- )
   TS-V @ 0 T=  TS-BN @ 0 T=  TS-BC c@ 0 T=  TS-PV @ NULL-PTR - 0 T= ;

: HERE-N ( -- n )
   here NULL-PTR - ;

\ ---- the diagnostics a colon body gives -------------------------------------

: MISMATCH ( -- )
   S\" \"code\":\"E-MISMATCH\"" HAS?  S\" \"repair_class\":\"fix_type\"" HAS? ;

: RAW-PTR ( -- )
   S\" \"code\":\"E-RAW-CELL-PTR\"" HAS?
   S\" \"repair_class\":\"declare_pointer_cell\"" HAS?
   S\" \"reason\":\"raw storage cell: a pointer cannot be stored in or fetched from an undeclared cell\"" HAS? ;

: RAW-XT ( -- )
   S\" \"code\":\"E-RAW-CELL-PTR\"" HAS?
   S\" \"repair_class\":\"declare_xt_cell\"" HAS?
   S\" \"reason\":\"raw storage cell: an undeclared cell cannot hold an execution token / a quotation\"" HAS? ;

: AT-COMMA ( -- )   S\" \"token\":\",\"" HAS? ;
: AT-STORE ( -- )   S\" \"token\":\"!\"" HAS? ;
: AT-ADD ( -- )     S\" \"token\":\"+!\"" HAS? ;
: AT-CSTORE ( -- )  S\" \"token\":\"c!\"" HAS? ;
: GOT-PTR ( -- )    S\" \"actual\":\"ptr " HAS? ;
: GOT-XT ( -- )     S\" \"actual\":\"opaque-xt " HAS? ;

\ ---- the refused programs, each beside its colon-body twin --------------------

: CASE-CREATE-XT ( -- )
   s" create T ' DUP , : the xt is refused as in a body, nothing is compiled" T-LABEL
   ARM
   [: s" : TS-B1 ( -- ) ['] DUP , ;" evaluate-closed ;] CHECK-RC TTHROWSQ
   MISMATCH AT-COMMA GOT-XT
   HERE-N [: s" create TS-T0" evaluate-closed ;] 0 TTHROWSQ HERE-N swap - {: hdr:n :}
   ARM
   HERE-N [: s" create TS-T3 ' DUP ," evaluate-closed ;] CHECK-RC TTHROWSQ
   MISMATCH AT-COMMA GOT-XT
   HERE-N swap - hdr T=
   DISARM ;

: CASE-CREATE-HERE ( -- )
   s" create T here , : the pointer is refused as in a body, nothing is compiled" T-LABEL
   ARM
   [: s" : TS-B2 ( -- ) here , ;" evaluate-closed ;] CHECK-RC TTHROWSQ
   MISMATCH AT-COMMA GOT-PTR
   HERE-N [: s" create TS-T5" evaluate-closed ;] 0 TTHROWSQ HERE-N swap - {: hdr:n :}
   ARM
   HERE-N [: s" create TS-T4 here ," evaluate-closed ;] CHECK-RC TTHROWSQ
   MISMATCH AT-COMMA GOT-PTR
   HERE-N swap - hdr T=
   DISARM ;

: CASE-VARIABLE-HERE ( -- )
   s" variable V here V ! : refused by the raw-cell rule, V stays 0" T-LABEL
   ARM
   [: s" : TS-B3 ( -- ) here TS-V ! ;" evaluate-closed ;] CHECK-RC TTHROWSQ
   RAW-PTR AT-STORE
   ARM
   [: s" variable TS-NV here TS-NV !" evaluate-closed ;] CHECK-RC TTHROWSQ
   RAW-PTR AT-STORE
   DISARM ;                 \ the refused program's TS-NV goes with its rollback

: CASE-VARIABLE-XT ( -- )
   s" variable W ' DUP W ! : refused by the raw-cell xt rule, W stays 0" T-LABEL
   ARM
   [: s" : TS-B4 ( -- ) ['] DUP TS-V ! ;" evaluate-closed ;] CHECK-RC TTHROWSQ
   RAW-XT AT-STORE
   ARM
   [: s" variable TS-NW ' DUP TS-NW !" evaluate-closed ;] CHECK-RC TTHROWSQ
   RAW-XT AT-STORE
   DISARM ;                 \ the refused program's TS-NW goes with its rollback

: CASE-TYPED-N-PTR ( -- )
   s" here BN ! through a ( -- ptr n ) accessor is refused as in a body" T-LABEL
   CLEAR ARM
   [: s" : TS-B5 ( -- ) here TS-BN ! ;" evaluate-closed ;] CHECK-RC TTHROWSQ
   MISMATCH AT-STORE GOT-PTR
   ARM
   [: s" here TS-BN !" evaluate-closed ;] CHECK-RC TTHROWSQ
   MISMATCH AT-STORE GOT-PTR
   DISARM UNTOUCHED ;

: CASE-TYPED-N-XT ( -- )
   s" an xt through the same accessor is refused as in a body" T-LABEL
   CLEAR ARM
   [: s" : TS-B6 ( -- ) ['] DUP TS-BN ! ;" evaluate-closed ;] CHECK-RC TTHROWSQ
   MISMATCH AT-STORE GOT-XT
   ARM
   [: s" ' DUP TS-BN !" evaluate-closed ;] CHECK-RC TTHROWSQ
   MISMATCH AT-STORE GOT-XT
   DISARM UNTOUCHED ;

: CASE-ADD-STORE ( -- )
   s" pointer and xt operands to BN +! are refused as in a body" T-LABEL
   CLEAR ARM
   [: s" : TS-B7 ( -- ) here TS-BN +! ;" evaluate-closed ;] CHECK-RC TTHROWSQ
   MISMATCH AT-ADD GOT-PTR
   ARM
   [: s" here TS-BN +!" evaluate-closed ;] CHECK-RC TTHROWSQ
   MISMATCH AT-ADD GOT-PTR
   ARM
   [: s" : TS-B8 ( -- ) ['] DUP TS-BN +! ;" evaluate-closed ;] CHECK-RC TTHROWSQ
   MISMATCH AT-ADD GOT-XT
   ARM
   [: s" ' DUP TS-BN +!" evaluate-closed ;] CHECK-RC TTHROWSQ
   MISMATCH AT-ADD GOT-XT
   DISARM UNTOUCHED ;

: CASE-BYTE-STORE ( -- )
   s" pointer and xt operands to c! through ( -- ptr u8 ) are refused as in a body" T-LABEL
   CLEAR ARM
   [: s" : TS-B9 ( -- ) here TS-BC c! ;" evaluate-closed ;] CHECK-RC TTHROWSQ
   MISMATCH AT-CSTORE GOT-PTR
   ARM
   [: s" here TS-BC c!" evaluate-closed ;] CHECK-RC TTHROWSQ
   MISMATCH AT-CSTORE GOT-PTR
   ARM
   [: s" : TS-B10 ( -- ) ['] DUP TS-BC c! ;" evaluate-closed ;] CHECK-RC TTHROWSQ
   MISMATCH AT-CSTORE GOT-XT
   ARM
   [: s" ' DUP TS-BC c!" evaluate-closed ;] CHECK-RC TTHROWSQ
   MISMATCH AT-CSTORE GOT-XT
   DISARM UNTOUCHED ;

: CASE-POINTER-SUM ( -- )
   S\" s\" abc\" + . : a byte pointer into a scalar consumer is refused as in a body" T-LABEL
   ARM
   [: S\" : TS-B11 ( -- ) s\" abc\" + . ;" evaluate-closed ;] CHECK-RC TTHROWSQ
   MISMATCH S\" \"token\":\".\"" HAS?  S\" \"actual\":\"ptr u8 " HAS?
   ARM
   [: S\" s\" abc\" + ." evaluate-closed ;] CHECK-RC TTHROWSQ
   MISMATCH S\" \"token\":\".\"" HAS?  S\" \"actual\":\"ptr u8 " HAS?
   DISARM ;

: CASE-REAL-TO-N ( -- )
   s" 0.5 N with N ( n -- ) : a real into a scalar consumer is refused as in a body" T-LABEL
   ARM
   [: s" : TS-B14 ( -- ) 0.5 TS-NN ;" evaluate-closed ;] CHECK-RC TTHROWSQ
   MISMATCH S\" \"token\":\"TS-NN\"" HAS?  S\" \"actual\":\"r " HAS?
   ARM
   [: s" 0.5 TS-NN" evaluate-closed ;] CHECK-RC TTHROWSQ
   MISMATCH S\" \"token\":\"TS-NN\"" HAS?  S\" \"actual\":\"r " HAS?
   DISARM ;

\ Nothing checked the window survivor in: the row rebuilds the window's cells as
\ unknown, and a typed consumer refuses an unknown cell whatever it holds.
: CASE-WINDOW-SURVIVOR ( -- )
   s" a cell pushed inside 0 set-check is refused at its next typed consumer" T-LABEL
   CLEAR ARM
   [: s" check@ 0 set-check here swap set-check TS-PV !" evaluate-closed ;] CHECK-RC TTHROWSQ
   S\" \"code\":\"E-STALE-READ\"" HAS?  AT-STORE
   S\" \"repair_class\":\"drop_unknown_cell\"" HAS?
   S\" \"reason\":\"unknown cell: this reads a cell beneath the cells the interpret row knows" HAS?
   DISARM UNTOUCHED ;

\ A top-level refusal names the file and line, in prose and JSON, and no
\ definition; the JSON has no column or byte. A string `evaluate` runs lies
\ outside the file's bytes, so its refusal names the file alone. The load holds
\ the resolved path, so the assertions match its tail.
: INCLUDE-SITE ( -- )
   [: s" include test/fixtures/top-row-site.f" evaluate-closed ;] CHECK-RC TTHROWSQ ;

: INCLUDE-EVAL ( -- )
   [: s" include test/fixtures/top-row-eval.f" evaluate-closed ;] CHECK-RC TTHROWSQ ;

: CASE-SITE ( -- )
   s" a top-level refusal names its file and line" T-LABEL
   CLEAR  DIAG-BUF DIAG-CAP DIAG-BUFFER!
   INCLUDE-SITE
   s" /test/fixtures/top-row-site.f:4: at '!' raw storage cell" HAS?
   DIAG-BUFFER-OFF
   ARM  INCLUDE-SITE
   S\" /test/fixtures/top-row-site.f\",\"line\":4,\"definition_source\"" HAS?  RAW-PTR
   DISARM UNTOUCHED ;

: CASE-EVAL-SITE ( -- )
   s" a top-level refusal in an evaluated string names its file alone" T-LABEL
   CLEAR  DIAG-BUF DIAG-CAP DIAG-BUFFER!
   INCLUDE-EVAL
   s" /test/fixtures/top-row-eval.f: at '!' raw storage cell" HAS?
   DIAG-BUFFER-OFF
   ARM  INCLUDE-EVAL
   S\" /test/fixtures/top-row-eval.f\",\"definition_source\"" HAS?  RAW-PTR
   DISARM UNTOUCHED ;

\ ---- children -----------------------------------------------------------------

: CHILD ( ptr u8 n -- outcome ) {: src:ptr srcu:n :}
   src srcu OUT IO-CAP >LEN ERR IO-CAP >LEN CHILD-MS >MS SUBJECT:RUN
   {: outu:len erru:len oc :}
   outu LEN>N OUT-U !  erru LEN>N ERR-U !  oc ;

: OUT$ ( -- ptr u8 n )   OUT OUT-U @ ;
: ERR$ ( -- ptr u8 n )   ERR ERR-U @ ;

: EXITS ( ptr u8 n n -- ) {: src:ptr srcu:n want:n :}
   src srcu CHILD {: oc :}
   src srcu OUT$ ERR$ oc want T-OUTCOME-EXITED= ;

: ERR-HAS? ( ptr u8 n -- )   ERR$ 2swap CONTAINS? TTRUE ;
: ERR-LACKS? ( ptr u8 n -- )   ERR$ 2swap CONTAINS? TFALSE ;
: OUT-HAS? ( ptr u8 n -- )   OUT$ 2swap CONTAINS? TTRUE ;

\ A child: unchecked, this program reads below the closed floor or branches to
\ xt 0, which crashes the process.
: CASE-EXECUTE-UNDERFLOW ( -- )
   s" ' F execute with F's input missing is refused as in a body" T-LABEL
   ARM
   [: s" : TS-B12 ( -- ) ['] TS-F2 execute ;" evaluate-closed ;] CHECK-RC TTHROWSQ
   S\" \"code\":\"E-INPUT-UNDERFLOW\"" HAS?  S\" \"token\":\"execute\"" HAS?
   DISARM
   s" true DIAG-JSON! ' TS-F2 execute" CHECK-RC EXITS
   S\" \"code\":\"E-INPUT-UNDERFLOW\"" ERR-HAS?  S\" \"token\":\"execute\"" ERR-HAS? ;

: CASE-CATCH-NON-XT ( -- )
   s" 0 0 catch hands catch a number and is refused as in a body" T-LABEL
   ARM
   [: s" : TS-B13 ( -- ) 0 0 catch ;" evaluate-closed ;] CHECK-RC TTHROWSQ
   S\" \"code\":\"E-MISMATCH\"" HAS?  S\" \"token\":\"catch\"" HAS?
   DISARM
   s" true DIAG-JSON! 0 0 catch" CHECK-RC EXITS
   S\" \"code\":\"E-MISMATCH\"" ERR-HAS?  S\" \"token\":\"catch\"" ERR-HAS?
   S\" \"actual\":\"n \"" ERR-HAS? ;

\ A child: unchecked, this program branches to the address `here` pushed inside
\ the window, which crashes the process.
: CASE-EXECUTE-UNKNOWN ( -- )
   s" execute of a cell a 0 set-check window left names the unknown cell" T-LABEL
   s" true DIAG-JSON! check@ 0 set-check here swap set-check execute" CHECK-RC EXITS
   S\" \"code\":\"E-STALE-READ\"" ERR-HAS?  S\" \"token\":\"execute\"" ERR-HAS? ;

\ A child: unchecked, `evaluate` would read a string from whatever the stack
\ holds. `'` of a word with no stack effect pushes an xt of no known effect.
: CASE-EXECUTE-OPAQUE-TICK ( -- )
   s" execute of ' of a word with no stack effect names the opaque xt" T-LABEL
   s" true DIAG-JSON! ' evaluate execute" CHECK-RC EXITS
   S\" \"code\":\"E-EXEC-OPAQUE-XT\"" ERR-HAS?  S\" \"token\":\"execute\"" ERR-HAS? ;

\ A child: unchecked, the second `2r>` pops into the return frame of the code
\ running the program, which crashes the process. The row carries only the two
\ cells `2>r` pushed, so reading below them is refused with a body's
\ return-stack reason.
: CASE-RETURN-BELOW ( -- )
   s" r> below a body's return frame names the return-stack reason" T-LABEL
   DIAG-BUF DIAG-CAP DIAG-BUFFER!
   [: s" : TS-B15 ( -- ) r> drop ;" evaluate-closed ;] CHECK-RC TTHROWSQ
   s" habu: in ts-b15: at 'drop' return stack: this reads below the return-stack cells pushed here expected: a actual: " HAS?
   DIAG-BUFFER-OFF
   s" 2r> below the saved return cells is refused with the same reason" T-LABEL
   s" 1 2 2>r 2r> 2r> 2drop" CHECK-RC EXITS
   s" at '2r>' return stack: this reads below the return-stack cells pushed here expected: a b actual: " ERR-HAS? ;

\ A child: DIAG-FILE! has no inverse; the label stays on this test file for
\ every later case.
: CASE-LABEL ( -- )
   s" a tool's file label names a top-level refusal's file" T-LABEL
   S\" check@ 0 set-check s\" /subject/label.f\" DIAG-FILE! true DIAG-JSON! set-check here TS-V !"
   CHECK-RC EXITS
   S\" \"file\":\"/subject/label.f\"" ERR-HAS?
   S\" \"code\":\"E-RAW-CELL-PTR\"" ERR-HAS? ;

\ A child: unchecked, `c@` reads through the window's cell, which crashes the
\ process. The window leaves two cells; the string literal, the first checked
\ event after it, lands over no saved row, so the row pads them as unknown. The
\ return's `2drop` removes the string with no event between, so the next load
\ finds the window's cells and `c@` refuses the one it reads.
: CASE-SHORT-ROW ( -- )
   s" a literal over a row shorter than the stack leaves the cells beneath unknown" T-LABEL
   S\" true DIAG-JSON! TRUSTED: TS-EV2 ( ptr u8 n -- ) evaluate 2drop ; s\\\" check@ 0 set-check 0 0 rot set-check s\\\" abc\\\"\" TS-EV2 drop c@ . cr"
   CHECK-RC EXITS
   S\" \"code\":\"E-STALE-READ\"" ERR-HAS?  S\" \"token\":\"c@\"" ERR-HAS? ;

\ A child: `0 set-current` and `using` outlive the program that runs them. With
\ no package context the row still types certified words and still refuses p3;
\ the open package's own words, public and private, bind there as the engine
\ finds them, so a typed use runs and a mistyped one is refused by its type;
\ a used public retired by `undefine` resolves past it, to the global or the
\ live public, as the engine does, and names no scope refusal.
: NOCTX ( -- )   s" package TS-NOCTX 0 set-current 1 2 drop drop " SB-APPEND ;
: PKW ( -- )   s" package TS-PKW public : TS-PKW-N ( -- n ) 5 ; 0 set-current " SB-APPEND ;
: PKW-USE ( -- )   s" TS-PKW-N 1 + drop " SB-APPEND ;
: PKW-MISUSE ( -- )   s" true DIAG-JSON! TS-PKW-N c@ drop " SB-APPEND ;
: PKV ( -- )   s" package TS-PKV variable TS-PKV-C 0 set-current TS-PKV-C @ drop " SB-APPEND ;
: SHADOW ( -- )
   s" package TS-SHADOW public : drop ( n -- ) . ; undefine drop ;package using TS-SHADOW 1 drop " SB-APPEND ;
: AMBIG ( -- )
   s" package TS-AMB-A public : TS-AMB ( n -- ) drop ; ;package " SB-APPEND
   s" package TS-AMB-B public : TS-AMB ( n -- ) drop ; undefine TS-AMB ;package " SB-APPEND
   s" using TS-AMB-A using TS-AMB-B 1 TS-AMB " SB-APPEND ;
: AFTER ( -- )   S\" s\" AFTER\" type cr" SB-APPEND ;
: P3 ( -- )   S\" true DIAG-JSON! s\" abc\" + . cr" SB-APPEND ;

: RUNS-AFTER ( ptr u8 n -- ) {: code:ptr codeu:n :}
   SB$ 0 EXITS  s" AFTER" OUT-HAS?  code codeu ERR-LACKS? ;

: REFUSES-P3 ( ptr u8 n -- ) {: code:ptr codeu:n :}
   SB$ CHECK-RC EXITS
   S\" \"code\":\"E-MISMATCH\"" ERR-HAS?  S\" \"token\":\".\"" ERR-HAS?
   code codeu ERR-LACKS? ;

: CASE-SCOPES ( -- )
   s" no package context: certified words run, rc 0" T-LABEL
   SB-RESET NOCTX AFTER  s" no authenticated package context" RUNS-AFTER
   s" no package context: p3 is refused by its own diagnostic" T-LABEL
   SB-RESET NOCTX P3  s" no authenticated package context" REFUSES-P3
   s" 0 set-current in a package: its public word types, rc 0" T-LABEL
   SB-RESET PKW PKW-USE AFTER  s" stale cell" RUNS-AFTER
   s" 0 set-current in a package: its private variable types, rc 0" T-LABEL
   SB-RESET PKV AFTER  s" stale cell" RUNS-AFTER
   s" 0 set-current in a package: a mistyped use of its word is refused by its type" T-LABEL
   SB-RESET PKW PKW-MISUSE  SB$ CHECK-RC EXITS
   S\" \"code\":\"E-MISMATCH\"" ERR-HAS?  S\" \"token\":\"c@\"" ERR-HAS?
   S\" \"actual\":\"n \"" ERR-HAS?
   s" a retired used public shadowing a global: 1 drop runs, rc 0" T-LABEL
   SB-RESET SHADOW AFTER  s" E-USING-SHADOW-GLOBAL" RUNS-AFTER
   s" a retired used public shadowing a global: p3 is refused by its own diagnostic" T-LABEL
   SB-RESET SHADOW P3  s" E-USING-SHADOW-GLOBAL" REFUSES-P3
   s" two used publics of one tail, one retired: 1 TS-AMB runs, rc 0" T-LABEL
   SB-RESET AMBIG AFTER  s" ambiguous" RUNS-AFTER
   s" two used publics of one tail, one retired: p3 is refused by its own diagnostic" T-LABEL
   SB-RESET AMBIG P3  s" ambiguous" REFUSES-P3 ;

\ A child: these replace the check hook and the tier for the rest of the process.
: CASE-HOOK-ROWS ( -- )
   s" ' H set-check, 0 set-check and 1 set-tier run at top level" T-LABEL
   SB-RESET
   s" : TS-H ( ptr u8 n -- n ) 2drop -1 ; " SB-APPEND
   s" check@ 0 set-check ' TS-H set-check 0 set-check set-check 1 set-tier " SB-APPEND
   AFTER
   SB$ 0 EXITS  s" AFTER" OUT-HAS? ;

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

: CASE-DECLARED ( -- )
   s" a PTR-VARIABLE takes a pointer and a TYPED-VARIABLE [ -- ] an xt" T-LABEL
   CLEAR
   [: s" here TS-PV !  ' TS-X TS-HK !" evaluate-closed ;] 0 TTHROWSQ
   TS-PV @ NULL-PTR - 0 T<> ;

: CASE-NESTED-EVAL ( -- )
   s" a nested closed evaluation answers its own cell" T-LABEL
   [: S\" s\" 1 2 +\" TEST-EVAL:N 3 T=" evaluate-closed ;] 0 TTHROWSQ
   s" a word evaluating text while it runs keeps the row its step left" T-LABEL
   [: S\" TRUSTED: TS-EV ( -- ) s\" 1 drop\" evaluate ;  ' TS-EV catch TS-NN" evaluate-closed ;]
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

: CASE-ACROSS-COLON ( -- )
   s" a pointer kept across a : definition is stored typed" T-LABEL
   CLEAR
   [: s" here : TS-MID ( -- ) ; TS-PV !" evaluate-closed ;] 0 TTHROWSQ
   TS-PV @ NULL-PTR - 0 T<> ;

: CASE-ACROSS-REQUIRE ( -- )
   s" a pointer kept across require is stored typed" T-LABEL
   CLEAR
   [: s" here require src/lib/le.f TS-PV !" evaluate-closed ;] 0 TTHROWSQ
   TS-PV @ NULL-PTR - 0 T<> ;

: SIBLINGS ( -- )
   [: s" here 9 throw" evaluate-closed ;] catch SIB-RC T=
   s" here TS-PV !" evaluate-closed ;

: CASE-SIBLINGS ( -- )
   s" two sibling closed evaluations in one word, the first throwing" T-LABEL
   CLEAR
   ['] SIBLINGS 0 TTHROWSQ
   TS-PV @ NULL-PTR - 0 T<> ;

\ The first text's row ends `ptr ptr n` when `/` throws; its sibling starts on
\ the same stack base with no cells, so its literals replace that row: a row
\ that kept it would hand `+` two pointers.
: SIBLING-LITERALS ( -- )
   [: s" here here 1 0 /" evaluate-closed ;] catch E-DIV-ZERO T=
   s" 3 4 + TS-V !" evaluate-closed ;

: CASE-SIBLING-LITERALS ( -- )
   s" literals after a sibling's throw drop its row, then a raw store takes their sum" T-LABEL
   CLEAR
   ['] SIBLING-LITERALS 0 TTHROWSQ
   TS-V @ 7 T= ;

: CASE-WINDOW ( -- )
   s" a 0 set-check window runs unchecked" T-LABEL
   [: S\" check@ 0 set-check s\" abc\" + drop set-check" evaluate-closed ;] 0 TTHROWSQ
   s" a window inside a nested evaluation leaves the outer row typed" T-LABEL
   [: S\" s\" check@ 0 set-check set-check 5\" TEST-EVAL:N 5 T=" evaluate-closed ;] 0 TTHROWSQ
   s" an evaluate inside a window continues the row it reads" T-LABEL
   [: S\" check@ 0 set-check s\" set-check 5\" evaluate TS-NN" evaluate-closed ;] 0 TTHROWSQ ;

: CASE-TRUSTED ( -- )
   s" a TRUSTED: word, the CHECK! probe and a TRUSTED: shim are admitted" T-LABEL
   [: s" TRUSTED: TS-PASSES? ( ptr u8 n -- bool ) 2drop 0 0= ;" evaluate-closed ;] 0 TTHROWSQ
   [: S\" s\" some source\" TS-PASSES? drop" evaluate-closed ;] 0 TTHROWSQ
   [: S\" s\" : TS-G ( n -- n ) 1 + ;\" CHECK! drop" evaluate-closed ;] 0 TTHROWSQ
   [: s" TRUSTED: TS-SHIM ( n n -- n ) + ;  1 2 TS-SHIM drop" evaluate-closed ;] 0 TTHROWSQ ;

\ A child on a terminal: only the REPL's line loop recovers from a throw and
\ reads the next line.
: STEP ( ptr u8 n -- bool )
   BUF-CLEAR SEND-LINE
   s"  ok" WAIT-FOR if s"  ok" s" habu> " WAIT-AFTER else false then ;

: CASE-REPL ( -- )
   s" a refused REPL line names no file or definition" T-LABEL
   ENGINE-CANDIDATE:PATH$ SPAWN-ON-PTY
   s" habu> " WAIT-FOR TTRUE
   BUF-CLEAR s" variable TS-RV  here TS-RV !" SEND-LINE
   s" habu: at '!' raw storage cell" WAIT-FOR TTRUE
   s" a REPL line that throws, then a fresh line storing a pointer typed" T-LABEL
   s" PTR-VARIABLE TS-RP" STEP TTRUE
   BUF-CLEAR s" here 7 throw" SEND-LINE
   s" habu> " WAIT-FOR TTRUE
   s" here TS-RP !  42 ." STEP TTRUE
   s" 42" IN-BUF? TTRUE
   s" E-" IN-BUF? TFALSE
   4 SEND-BYTE
   REAP {: oc :}
   ENGINE-CANDIDATE:PATH$ BUF$ s" " oc 0 T-OUTCOME-EXITED=
   CLOSE-MASTER ;

public

: RUN ( -- )
   T-RESET
   CASE-CREATE-XT
   CASE-CREATE-HERE
   CASE-VARIABLE-HERE
   CASE-VARIABLE-XT
   CASE-TYPED-N-PTR
   CASE-TYPED-N-XT
   CASE-ADD-STORE
   CASE-BYTE-STORE
   CASE-POINTER-SUM
   CASE-REAL-TO-N
   CASE-WINDOW-SURVIVOR
   CASE-SITE
   CASE-EVAL-SITE
   CASE-LABEL
   CASE-EXECUTE-UNDERFLOW
   CASE-CATCH-NON-XT
   CASE-EXECUTE-OPAQUE-TICK
   CASE-EXECUTE-UNKNOWN
   CASE-RETURN-BELOW
   CASE-SHORT-ROW
   CASE-SCOPES
   CASE-HOOK-ROWS
   CASE-NUMERIC
   CASE-REAL
   CASE-DECLARED
   CASE-NESTED-EVAL
   CASE-TICKED
   CASE-RETURN-ROW
   CASE-ACROSS-COLON
   CASE-ACROSS-REQUIRE
   CASE-SIBLINGS
   CASE-SIBLING-LITERALS
   CASE-WINDOW
   CASE-TRUSTED
   CASE-REPL
   T-REPORT ;

;using
;package

TOP-ROW-STORAGE-TEST:RUN
