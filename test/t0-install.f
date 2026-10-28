\ t0-install.f - T0 as the runtime compiler on arm64: T0:INSTALL stores
\ T0:COMPILE in the cell the tier 1 `:` dispatches through, so every
\ definition this file loads after `1 set-tier`, the test words included, is
\ compiled by T0 through the normal Habu loop. The fixtures cover `:` and
\ TRUSTED: bodies, quotations, strings, locals, loops, case and recursion, and
\ each is run for its result. The does> definers cover a clause, an empty
\ clause whose child keeps its address kind, quotations nested in the parent
\ and one in the clause, clause locals and a definer chained on another, each
\ through a child it made. A CALLBACK: declaration compiled by T0 is handed to
\ libc's qsort, which sorts a row through it, and its C entry lies between its
\ accessor's code and the next definition's: the stub T0 appended to the
\ accessor's emission. The refusal rows evaluate definitions T0 refuses with
\ its own codes, which tier 1 accepts or refuses otherwise: a non-neutral
\ immediate in a TRUSTED: body (E-T0-IMMEDIATE), a counted string past 255
\ bytes in a does> clause (E-T0-LITERAL; the checker refuses a non-neutral
\ immediate in a clause even under TRUSTED:), after which the checker's
\ rollback depth is back where it was and the definer's name defined cleanly
\ compiles and its child runs, so the refusal rolled back the checker frame,
\ and a definition while a target shadow is open (E-T0-SHADOW); a tick of an
\ undefined word is the engine's undefined refusal, as under tier 1; a
\ definition after them proves a refusal leaves the compiler idle.
\
\ PROCESS BOUNDARY. T0:INSTALL replaces the dispatch for the rest of the
\ process and nothing restores it, so this file is its suite row's only file:
\ a file loaded after it would be compiled by T0.

require src/lib/test.f
require src/lib/ffi-abi.f
require src/lib/ffi-callback.f
require src/lib/task.f
require src/compiler/target/model.f
require src/compiler/target/native-binding.f
require src/compiler/native/shadow.f
require src/habu/t0-arm64.f
require src/habu/t0.f

CTARGET-ARCH:AARCH64 T0-EMIT:SELECT
T0:INSTALL
1 set-tier

package T0-INSTALL-TEST
private

: SQUARE ( n -- n ) dup * ;
TRUSTED: TWICE ( n -- n ) 2 * ;
: APPLY ( n [ n -- n ] -- n ) execute ;
: ADD3 ( n -- n ) [: 3 + ;] APPLY ;
: GREETING ( -- ptr u8 n ) s" hello" ;
: COUNTED-LEN ( -- n ) c" abcd" c@ ;

: SPREAD ( n n -- n )
   {: a:n b:n :}
   a 10 * b - ;

: SUM-BELOW ( n -- n ) 0 swap 0 ?do i + loop ;
: EVENS ( n -- n ) 0 swap 0 do 1+ 2 +loop ;

: HALVINGS ( n -- n )
   0 swap
   begin dup 1 > while 2 / swap 1+ swap repeat
   drop ;

: PICK-CODE ( n -- n )
   case
      1 of 10 endof
      2 of 20 endof
      30 swap
   endcase ;

: FACT ( n -- n ) dup 1 > if dup 1- recurse * then ;

\ ---- does> definers -----------------------------------------------------------
: KEEPER ( n -- ) create , does> ( -- n ) @ ;
42 KEEPER K42

: CELLER ( n -- ) create , does> ( -- ptr n ) ;
7 CELLER C7
s" C7" XREF-FIND-CURRENT-INDEX XREF-REQUIRE-INDEX constant C7-IX

: TWICER ( n -- ) [: [: 2 * ;] execute ;] execute create , does> ( -- n ) @ [: 1+ ;] execute ;
5 TWICER T10

: SCALER ( n -- ) create , does> ( n -- n ) @ {: x:n k:n :} x k * ;
6 SCALER S6

: BASE-D ( n -- ) create , does> ( -- n ) @ ;
: CHAINED ( n -- ) BASE-D does> ( -- n ) @ 1+ ;
9 CHAINED N9

\ ---- a clause T0 refuses -----------------------------------------------------
\ A definer whose clause holds a counted string of 256 bytes, one past what a
\ counted length's one byte holds.
512 constant LONG-CAP
LONG-CAP BUFFER: LONG
TYPED-VARIABLE LONG-U len

: LONG+ ( ptr u8 n -- ) LONG LONG-CAP LONG-U BUF-APPEND ;

: LONG-DEFINER ( -- ptr u8 n )
   LONG-U BUF-RESET
   S\" : REDO-D ( n -- ) create , does> ( -- n ) drop c\" " LONG+
   256 0 do [char] x LONG LONG-CAP LONG-U BUF-APPEND-C loop
   S\" \" c@ ;" LONG+
   LONG LONG-U BUF-LEN@ ;

\ ---- a callback C calls ------------------------------------------------------
3 constant ROW-N
create ROW ROW-N cells allot

PROCESS-SYMBOLS
FUNCTION: QSORT qsort ( ptr u8 n n n -- )
   0 24 WRITES-BYTES                 \ ROW-N cells
;FUNCTION

\ The two cells C's comparator arguments point at, small enough to subtract.
: ROW-ORDER ( ptr u8 ptr u8 -- n )
   {: a b :}
   a CELL-VIEW @ b CELL-VIEW @ - ;

CALLBACK: T0-CMP ( ptr u8 ptr u8 -- i32 ) 0 FALLBACK ;CALLBACK
\ A second declaration in a row: it arms only if T0's compile of the first
\ ended the pending stub and cleared NCBSTUB.
CALLBACK: T0-CMP2 ( ptr u8 ptr u8 -- i32 ) 0 FALLBACK ;CALLBACK
' ROW-ORDER T0-CMP-BODY !

\ The accessor, which owns the stub, and the descriptor defined after it.
CAST: XT>N ( [ -- n ] -- n )
CAST: CB-XT>N ( [ -- FFI-CB:callback ] -- n )
TYPED-VARIABLE ACCESSOR [ -- n ]
TYPED-VARIABLE DESCRIPTOR [ -- FFI-CB:callback ]
' T0-CMP-ENTRY ACCESSOR !
' T0-CMP DESCRIPTOR !

public

\ An immediate that is no parse-neutral one. The checker refuses it in a
\ checked body, so the refusal row uses it in a TRUSTED: body. Public: the row
\ evaluates its text at top level.
: NOTHING ( -- ) ; immediate

private

: CALL-CASE ( -- )
   s" a : body runs" T-LABEL  7 SQUARE 49 T=
   s" a TRUSTED: body runs" T-LABEL  21 TWICE 42 T=
   s" a quotation runs through a call" T-LABEL  4 ADD3 7 T=
   s" a string literal" T-LABEL  GREETING s" hello" T$=
   s" a counted string literal" T-LABEL  COUNTED-LEN 4 T=
   s" locals bind deepest first" T-LABEL  5 8 SPREAD 42 T=
   s" ?do loop" T-LABEL  5 SUM-BELOW 10 T=
   s" do +loop" T-LABEL  7 EVENS 4 T=
   s" begin while repeat" T-LABEL  40 HALVINGS 5 T=
   s" case: an of arm" T-LABEL  2 PICK-CODE 20 T=
   s" case: the default arm" T-LABEL  9 PICK-CODE 30 T=
   s" recurse" T-LABEL  5 FACT 120 T= ;

: DOES-CASE ( -- )
   s" a does> child runs its clause" T-LABEL  K42 42 T=
   s" an empty clause leaves the body address" T-LABEL  C7 @ 7 T=
   s" an empty clause's child keeps its address kind" T-LABEL
   C7-IX XREF-REC XREF-FLAGS DKIND:ADDR and 0<> TTRUE
   s" parent quotations, nested, and a clause quotation" T-LABEL  T10 11 T=
   s" clause locals" T-LABEL  7 S6 42 T=
   s" a chained definer's clause replaces the first" T-LABEL  N9 10 T= ;

: CALLBACK-CASE ( -- )
   T0-CMP TASK:SELF-CONTEXT FFI-CB:ENTRY {: fn:n :}
   s" the C entry is the stub T0 appended to the accessor" T-LABEL
   ACCESSOR @ XT>N fn <  fn DESCRIPTOR @ CB-XT>N <  and TTRUE
   s" qsort sorts through the callback" T-LABEL
   3 ROW !  1 ROW cell+ !  2 ROW 2 cells + !
   ROW BYTE-VIEW ROW-N CELL fn QSORT
   ROW @ 1 T=  ROW cell+ @ 2 T=  ROW 2 cells + @ 3 T=
   T0-CMP FFI-CB:UNBIND ;

: REFUSAL-CASE ( -- )
   s" a non-neutral immediate is refused by name" T-LABEL
   s" TRUSTED: USES-NOTHING ( -- ) T0-INSTALL-TEST:NOTHING ;" TEST-EVAL:RC
   T0:E-T0-IMMEDIATE T=
   s" the refusal names the immediate" T-LABEL
   T0:UNSUPPORTED$ s" T0-INSTALL-TEST:NOTHING" T$=
   s" a counted string past 255 bytes in a does> clause is refused" T-LABEL
   RBF-DEPTH @ {: depth:n :}
   LONG-DEFINER TEST-EVAL:RC T0:E-T0-LITERAL T=
   s" the refusal rolled back the checker frame" T-LABEL
   RBF-DEPTH @ depth T=
   s" the refused definer's name defined cleanly compiles and its child runs" T-LABEL
   s" : REDO-D ( n -- ) create , does> ( -- n ) @ ; 3 REDO-D REDO-3 REDO-3" TEST-EVAL:N 3 T=
   s" a definition while a target shadow is open is refused" T-LABEL
   RTARGET:HOST-TARGET RTARGET:NATIVE-BINDING NSHADOW:OPEN
   s" : IN-SHADOW ( -- n ) 1 ;" TEST-EVAL:RC
   NSHADOW:CLOSE
   T0:E-T0-SHADOW T=
   s" a tick of an undefined word is the undefined-word refusal" T-LABEL
   s" TRUSTED: TICKS-NOTHING ( -- n ) ['] T0-INSTALL-NO-SUCH-WORD ;" TEST-EVAL:RC
   70 T=
   s" a definition after the refusals compiles and runs" T-LABEL
   s" : AFTER-REFUSALS ( -- n ) 6 ; AFTER-REFUSALS" TEST-EVAL:N 6 T= ;

public

: RUN ( -- )
   T-RESET
   CALL-CASE
   DOES-CASE
   CALLBACK-CASE
   REFUSAL-CASE
   T-REPORT ;

;package

T0-INSTALL-TEST:RUN
