\ t0-install.f - T0 as the runtime compiler on arm64: T0:INSTALL stores
\ T0:COMPILE in the cell the tier 1 `:` dispatches through, so every
\ definition this file loads after `1 set-tier`, the test words included, is
\ compiled by T0 through the normal Habu loop. The fixtures cover `:` and
\ TRUSTED: bodies, quotations, strings, locals, loops, case and recursion, and
\ each is run for its result. A CALLBACK: declaration compiled by T0 is handed
\ to libc's qsort, which sorts a row through it, and its C entry lies between
\ its accessor's code and the next definition's: the stub T0 appended to the
\ accessor's emission. The refusal rows evaluate definitions T0 refuses
\ with its own codes, which tier 1 accepts or refuses otherwise: a non-neutral
\ immediate in a TRUSTED: body (E-T0-IMMEDIATE), a does> definer
\ (E-T0-UNSUPPORTED) and a definition while a target shadow is open
\ (E-T0-SHADOW); a tick of an undefined word is the engine's undefined
\ refusal, as under tier 1; a definition after them proves a refusal leaves
\ the compiler idle.
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
   s" a does> definer is refused" T-LABEL
   s" : KEEPER ( n -- ) create , does> ( -- n ) @ ;" TEST-EVAL:RC
   T0:E-T0-UNSUPPORTED T=
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
   CALLBACK-CASE
   REFUSAL-CASE
   T-REPORT ;

;package

T0-INSTALL-TEST:RUN
