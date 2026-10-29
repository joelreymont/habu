\ boot-row-test.f - an engine's boot rows (REQUIRE-BOOT-N, src/core/include.f)
\ are honest and answer away from a tree. `require` of a row is a no-op, so a
\ row the image does not carry hides its words: re-including each row must be
\ refused by a collision with what the image holds. A row is a module identity:
\ outside any tree it still answers, and a module the image lacks is refused by
\ name under the CWD.

require src/lib/test.f
require src/lib/string.f
require src/lib/fmt.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/process-cwd.f
require src/lib/engine-candidate.f
require src/lib/test/outcome.f

using SOURCE-ROOT

package BOOT-ROW-TEST
private

$20000 constant DUMP-CAP          \ room for the boot and process row lists
$4000 constant IO-CAP
INCLUDE-PATH-CAP $40 + constant SRC-CAP
PATH-CAP 1+ constant ROOT-CAP
30000 constant TIMEOUT-MS

create DUMP-BUF DUMP-CAP allot
create OUT-BUF IO-CAP allot
create ERR-BUF IO-CAP allot
create SRC-BUF SRC-CAP allot
create OUTSIDE ROOT-CAP allot
create OUTSIDE-HB ROOT-CAP allot

variable DUMP-U
variable DUMP-I
variable OUT-U
variable ERR-U
variable SRC-U
variable OUTSIDE-U
variable OUTSIDE-HB-U
variable RC-N
variable EXITED?
variable WALKED

: ERR$ ( -- ptr u8 n )     ERR-BUF ERR-U @ ;
: SRC$ ( -- ptr u8 n )     SRC-BUF SRC-U @ ;
: OUTSIDE$ ( -- ptr u8 n ) OUTSIDE OUTSIDE-U @ ;
: OUTSIDE-HB$ ( -- ptr u8 n ) OUTSIDE-HB OUTSIDE-HB-U @ ;

\ ---- one run of an engine: source on stdin, the caller's CWD -----------------

: STORE ( ptr u8 n ptr u8 n outcome -- )
   {: src:ptr srcu:n out:ptr outu:n oc :}
   oc MATCH outcome
     exited OF   RC-N ! true  EXITED? ! ENDOF
     signaled OF RC-N ! false EXITED? ! ENDOF
     timeout OF src srcu out outu ERR$ T-TIMED-OUT ENDOF
   ;MATCH ;

: RUN-INTO ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- n )
   {: exe:ptr exeu:n cwd:ptr cwdu:n src:ptr srcu:n out:ptr outcap:n :}
   PROC-ARGV-RESET
   exe exeu >LEN
   cwd cwdu >LEN
   src srcu >LEN
   out outcap >LEN
   ERR-BUF IO-CAP >LEN
   TIMEOUT-MS >MS
   PROC-CWD:RUN-ARGV-ENV-CWD-STDIN-CAPTURE-OUTCOME {: outu:len erru:len oc :}
   erru LEN>N ERR-U !
   src srcu out outu LEN>N oc STORE
   outu LEN>N ;

\ Cases ask the engine under test (src/lib/engine-candidate.f).
: RUN ( ptr u8 n ptr u8 n -- )
   {: cwd:ptr cwdu:n src:ptr srcu:n :}
   ENGINE-CANDIDATE:PATH$ cwd cwdu src srcu OUT-BUF IO-CAP RUN-INTO OUT-U ! ;

: RUN-OUTSIDE ( ptr u8 n -- )
   {: src:ptr srcu:n :}
   OUTSIDE-HB$ OUTSIDE$ src srcu OUT-BUF IO-CAP RUN-INTO OUT-U ! ;

\ ---- the loader line the child is given --------------------------------------

: SRC-C+ ( n -- ) {: c:n :}
   SRC-U @ 1+ SRC-CAP > if s" boot-row: source overflow" T-EX-FAIL die then
   c SRC-BUF SRC-U @ ZBYTE!
   SRC-U @ 1+ SRC-U ! ;

: SRC+ ( ptr u8 n -- ) {: a:ptr u:n :}
   SRC-U @ u + SRC-CAP > if s" boot-row: source overflow" T-EX-FAIL die then
   a SRC-BUF SRC-U @ + u BYTE-COPY
   SRC-U @ u + SRC-U ! ;

: LOADER-SRC! ( ptr u8 n ptr u8 n -- ) {: word:ptr wordu:n a:ptr u:n :}
   0 SRC-U !
   word wordu SRC+
   s"  " SRC+
   a u SRC+
   STR-LF SRC-C+ ;

\ ---- the candidate's own boot rows -------------------------------------------
\ Asked of the candidate, not the running image: a gate may name another engine.
\ The count precedes the rows so a truncated capture cannot shorten the walk.

: DUMP-SRC$ ( -- ptr u8 n )
   S\" : BR-ROW ( n -- ) dup REQUIRE-SLOT swap REQUIRE-LEN@ type CR ; : BR-DUMP ( -- ) REQUIRE-BOOT-N @ . CR REQUIRE-BOOT-N @ 0 ?do i BR-ROW loop ; BR-DUMP\n" ;

: DUMP-ROWS ( -- )
   ENGINE-CANDIDATE:PATH$ CWD$ DUMP-SRC$ DUMP-BUF DUMP-CAP RUN-INTO DUMP-U !
   0 DUMP-I ! ;

: EOL? ( n -- bool ) {: c:n :}
   c STR-LF = c STR-CR = or ;

: DUMP-AT-BLANK? ( -- bool )
   DUMP-I @ DUMP-U @ >= if false exit then
   DUMP-BUF DUMP-I @ ZBYTE@ EOL? ;

: DUMP-AT-TEXT? ( -- bool )
   DUMP-I @ DUMP-U @ >= if false exit then
   DUMP-BUF DUMP-I @ ZBYTE@ EOL? 0= ;

: DUMP-SKIP-BLANK ( -- )
   begin DUMP-AT-BLANK? while DUMP-I @ 1+ DUMP-I ! repeat ;

: DUMP-LINE ( -- ptr u8 n bool )
   DUMP-SKIP-BLANK
   DUMP-I @ DUMP-U @ >= if NULL$ false exit then
   DUMP-BUF DUMP-I @ + DUMP-I @ {: a:ptr start:n :}
   begin DUMP-AT-TEXT? while DUMP-I @ 1+ DUMP-I ! repeat
   a DUMP-I @ start -
   true ;

: DUMP-REWIND ( -- )
   0 DUMP-I !
   DUMP-LINE drop 2drop ;

\ ---- per-row proof -----------------------------------------------------------

: ROW-LABEL ( n -- ) {: ix:n :}
   SB-RESET s" boot row " SB-APPEND ix FMT:SB-U SB$ T-LABEL ;

: ROW-DIAG ( ptr u8 n n -- ) {: a:ptr u:n ix:n :}
   s" boot row " type ix FMT:.INT s" : " type a u type
   s"  exited=" type EXITED? @ FMT:.INT
   s"  rc=" type RC-N @ FMT:.INT cr
   ERR$ type ;

\ `include: cannot open` is excluded by name: a missing file is a different
\ defect and must not read as proof.
: ROW-REFUSED? ( -- bool )
   EXITED? @
   RC-N @ 0 <> and
   ERR$ s" include: cannot open " CONTAINS? 0= and ;

: ROW-CHECK ( ptr u8 n n -- ) {: a:ptr u:n ix:n :}
   ix ROW-LABEL
   a u FS:EXISTS? TTRUE
   s" include" a u LOADER-SRC!
   CWD$ SRC$ RUN
   ROW-REFUSED? {: refused:bool :}
   refused 0= if a u ix ROW-DIAG then
   ix ROW-LABEL
   refused TTRUE ;

: WALK-ROWS ( -- )
   0 WALKED !
   begin DUMP-LINE while
      WALKED @ ROW-CHECK
      WALKED @ 1+ WALKED !
   repeat 2drop ;

: ROWS-HONEST ( -- )
   DUMP-ROWS
   s" the boot-row dump exited" T-LABEL
   EXITED? @ TTRUE
   s" the boot-row dump exited clean" T-LABEL
   RC-N @ 0 T=
   s" the boot-row capture was not truncated" T-LABEL
   DUMP-U @ DUMP-CAP < TTRUE
   s" the dump opens with a row count" T-LABEL
   DUMP-LINE TTRUE {: a:ptr u:n :}
   WALK-ROWS
   s" every row the candidate counted was walked" T-LABEL
   SB-RESET WALKED @ FMT:SB-U SB$ a u T$=
   s" the candidate carries boot rows at all" T-LABEL
   WALKED @ 0 T<> ;

\ ---- the same rows, asked from a directory that is not a Habu tree ------------
\ A fresh temp directory, never the temp base: a stray /tmp/src symlink once
\ decided these cases, because CANONICAL resolves an existing prefix physically.
\ The engine runs as a copy at <outside>/bin/hb: an engine outside a tree takes
\ the tree two levels above its executable as its root.

: PREP-OUTSIDE ( -- )
   FS:CLEANUP-RESET
   s" habu-boot-row" FS:MKTEMP-DIR CANONICAL TTRUE {: a:ptr u:n :}
   a OUTSIDE u BYTE-COPY u OUTSIDE-U !
   OUTSIDE$ FS:CLEANUP-TREE+
   OUTSIDE$ s" bin" OUTSIDE-HB FS:JOIN-PATH {: binu:n :}
   OUTSIDE-HB binu FS:MAKE-DIR
   OUTSIDE$ s" bin/hb" OUTSIDE-HB FS:JOIN-PATH OUTSIDE-HB-U !
   ENGINE-CANDIDATE:PATH$ OUTSIDE-HB$ FS:COPY-FILE-STREAM
   OUTSIDE-HB$ FS:CHMOD-X ;

: EXPECT-MISSING$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   SB-RESET
   s" include: cannot open " SB-APPEND
   OUTSIDE$ SB-APPEND
   s" /" SB-APPEND
   a u SB-APPEND
   SB$ ;

\ src/arch/arm64/icode.f is a tree file the engine does not carry (the builder's
\ AOT assembler); both halves are asserted, so a deletion or a new boot row
\ fails by name.
: PROBE$ ( -- ptr u8 n )
   s" src/arch/arm64/icode.f" ;

: OUTSIDE-REFUSES ( -- )
   s" the probe module is a real file of this tree" T-LABEL
   PROBE$ FS:EXISTS? TTRUE
   s" require" PROBE$ LOADER-SRC! SRC$ RUN-OUTSIDE
   s" a module the image lacks is refused outside a tree" T-LABEL
   EXITED? @ TTRUE
   s" the refusal is the loader's own open failure" T-LABEL
   RC-N @ INCLUDE-IO-RC T=
   s" the refusal names the path under this CWD" T-LABEL
   ERR$ PROBE$ EXPECT-MISSING$ CONTAINS? TTRUE ;

: OUTSIDE-PROVIDES ( -- )
   DUMP-REWIND
   s" the dump carries a first row" T-LABEL
   DUMP-LINE TTRUE {: a:ptr u:n :}
   s" require" a u LOADER-SRC! SRC$ RUN-OUTSIDE
   s" a boot row still answers outside a tree" T-LABEL
   EXITED? @ TTRUE
   s" requiring a boot row outside a tree exits clean" T-LABEL
   RC-N @ 0 T=
   s" requiring a boot row outside a tree says nothing" T-LABEL
   ERR-U @ OUT-U @ + 0 T= ;

: MAIN ( -- )
   T-RESET
   PREP-OUTSIDE
   ROWS-HONEST
   OUTSIDE-REFUSES
   OUTSIDE-PROVIDES
   FS:CLEANUP-RUN
   T-REPORT
   s" boot-row-test: " type WALKED @ FMT:.INT s"  rows, ok" type cr ;

MAIN

;package
;using
