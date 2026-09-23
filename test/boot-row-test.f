\ boot-row-test.f - what a boot row claims, and what it answers away from a tree.
\
\ An engine's boot rows (REQUIRE-BOOT-N, src/core/include.f) are the files the
\ IMAGE carries, recorded portably: each row is the CWD-relative spelling the boot
\ prefix required (97db883a). Two properties keep that registry safe and neither
\ is checked anywhere else.
\
\ THE ROWS HAVE TO BE HONEST. `require` of a row is a no-op, so a row whose words
\ the image does NOT carry turns every require of that file into a silent skip and
\ every later reference into E-UNDEFINED - the hole dot
\ habu-refuse-a-provided-05424fb3 was filed on, and the one a rewind that dropped
\ the definitions without dropping the rows (src/habu/prefix-rewind.f, through
\ REQUIRE-REG:TRUNCATE) would open. The proof needs no table of names, and
\ deliberately so: a name table goes stale in silence and a source scanner has to
\ re-decide what `public`, `private` and a reopened package mean. `include` loads
\ UNCONDITIONALLY, so re-including a row on the engine that claims it must be
\ REFUSED by a collision with what the image already holds - a duplicate
\ definition, a sealed package, a duplicate type family, or a boot-only construct
\ that has already run. A row whose definitions were rewound away compiles cleanly
\ instead, and that clean exit is the failure this reports.
\
\ A ROW IS A MODULE IDENTITY, NOT A PATH. Away from any Habu tree a row still
\ answers, which is what lets a tool run from any directory at all require
\ lib/string.f. A module the image does NOT carry has to be refused there by name,
\ naming the path under THIS CWD - never quietly resolved somewhere else.
\
\ Every case asks the ENGINE UNDER TEST (lib/engine-candidate.f), the row list
\ included: a gate runs this file on one engine while naming another as the
\ candidate, and it is the candidate's registry that is under test.

require lib/test.f
require lib/string.f
require lib/fmt.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/process-cwd.f
require lib/engine-candidate.f

using SOURCE-ROOT

package BOOT-ROW-TEST
private

$20000 constant DUMP-CAP          \ every realistic REQUIRE-MAX row list, with room to spare
$4000 constant IO-CAP
INCLUDE-PATH-CAP $40 + constant SRC-CAP
FS-PATH-CAP 1+ constant ROOT-CAP
30000 constant TIMEOUT-MS

create DUMP-BUF DUMP-CAP allot
create OUT-BUF IO-CAP allot
create ERR-BUF IO-CAP allot
create SRC-BUF SRC-CAP allot
create OUTSIDE ROOT-CAP allot

variable DUMP-U
variable DUMP-I
variable OUT-U
variable ERR-U
variable SRC-U
variable OUTSIDE-U
variable RC-N
variable EXITED?
variable WALKED

: ERR$ ( -- ptr u8 n )     ERR-BUF ERR-U @ ;
: SRC$ ( -- ptr u8 n )     SRC-BUF SRC-U @ ;
: OUTSIDE$ ( -- ptr u8 n ) OUTSIDE OUTSIDE-U @ ;

\ ---- one run of the engine under test: source on stdin, the caller's CWD -----

: STORE ( outcome -- )
   MATCH outcome
     exited OF   RC-N ! true  EXITED? ! ENDOF
     signaled OF RC-N ! false EXITED? ! ENDOF
     timeout OF  0 RC-N ! false EXITED? ! ENDOF
   ;MATCH ;

: RUN-INTO ( ptr u8 n ptr u8 n ptr u8 n -- n )
   {: cwd:ptr cwdu:n src:ptr srcu:n out:ptr outcap:n :}
   PROC-ARGV-RESET
   ENGINE-CANDIDATE:PATH$ >LEN
   cwd cwdu >LEN
   src srcu >LEN
   out outcap >LEN
   ERR-BUF IO-CAP >LEN
   TIMEOUT-MS >MS
   PROC-CWD:RUN-ARGV-ENV-CWD-STDIN-CAPTURE-OUTCOME
   STORE
   LEN>N ERR-U !
   LEN>N ;

: RUN ( ptr u8 n ptr u8 n -- )
   OUT-BUF IO-CAP RUN-INTO OUT-U ! ;

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
\ Asked of the candidate rather than read out of the running image: the two are
\ different engines whenever a gate names a candidate, and only the candidate's
\ registry is under test. The count is printed ahead of the rows so a truncated
\ capture cannot quietly shorten the walk.

: DUMP-SRC$ ( -- ptr u8 n )
   S\" : BR-ROW ( n -- ) dup REQUIRE-SLOT swap REQUIRE-LEN@ type CR ; : BR-DUMP ( -- ) REQUIRE-BOOT-N @ . CR REQUIRE-BOOT-N @ 0 ?do i BR-ROW loop ; BR-DUMP\n" ;

: DUMP-ROWS ( -- )
   CWD$ DUMP-SRC$ DUMP-BUF DUMP-CAP RUN-INTO DUMP-U !
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

\ The next non-empty line, or false once the capture is spent.
: DUMP-LINE ( -- ptr u8 n bool )
   DUMP-SKIP-BLANK
   DUMP-I @ DUMP-U @ >= if NULL$ false exit then
   DUMP-BUF DUMP-I @ + DUMP-I @ {: a:ptr start:n :}
   begin DUMP-AT-TEXT? while DUMP-I @ 1+ DUMP-I ! repeat
   a DUMP-I @ start -
   true ;

: DUMP-REWIND ( -- )   \ back to the first row line, past the count
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

\ A row is honest when the image refuses to take its definitions a second time.
\ `include: cannot open` is excluded by name: that is the file MISSING, a different
\ defect that must not read as proof.
: ROW-REFUSED? ( -- bool )
   EXITED? @
   RC-N @ 0 <> and
   ERR$ s" include: cannot open " CONTAINS? 0= and ;

: ROW-CHECK ( ptr u8 n n -- ) {: a:ptr u:n ix:n :}
   ix ROW-LABEL
   a u EXISTS? TTRUE
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
\ A FRESH temp directory, never the temp base itself: a stray `src` symlink left
\ behind in a shared temp directory would otherwise decide these cases. Measured
\ on this machine - /tmp/src pointed into an unrelated checkout, and the refusal
\ below named that checkout instead of the CWD, because CANONICAL resolves an
\ existing prefix physically and a symlink is an existing prefix.

: PREP-OUTSIDE ( -- )
   CLEANUP-RESET
   s" habu-boot-row" HB-TMP-MKDIR CANONICAL TTRUE {: a:ptr u:n :}
   a OUTSIDE u BYTE-COPY u OUTSIDE-U !
   OUTSIDE$ CLEANUP-TREE+ ;

: EXPECT-MISSING$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   SB-RESET
   s" include: cannot open " SB-APPEND
   OUTSIDE$ SB-APPEND
   s" /" SB-APPEND
   a u SB-APPEND
   SB$ ;

\ src/arch/arm64/icode.f is a real file of this tree that the engine does NOT
\ carry - the AOT assembler the BUILDER uses, not the compiler the engine runs.
\ Both halves of that are asserted, so if it is ever deleted or ever becomes a
\ boot row the case fails by name instead of quietly proving nothing.
: PROBE$ ( -- ptr u8 n )
   s" src/arch/arm64/icode.f" ;

: OUTSIDE-REFUSES ( -- )
   s" the probe module is a real file of this tree" T-LABEL
   PROBE$ EXISTS? TTRUE
   OUTSIDE$ s" require" PROBE$ LOADER-SRC! SRC$ RUN
   s" a module the image lacks is refused outside a tree" T-LABEL
   EXITED? @ TTRUE
   s" the refusal is the loader's own open failure" T-LABEL
   RC-N @ INCLUDE-IO-RC T=
   s" the refusal names the path under this CWD" T-LABEL
   ERR$ PROBE$ EXPECT-MISSING$ CONTAINS? TTRUE ;

\ The other half of the same contract: a row answers by module identity with no
\ file below CWD to find. Losing this breaks every tool run from outside a tree.
: OUTSIDE-PROVIDES ( -- )
   DUMP-REWIND
   s" the dump carries a first row" T-LABEL
   DUMP-LINE TTRUE {: a:ptr u:n :}
   OUTSIDE$ s" require" a u LOADER-SRC! SRC$ RUN
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
   CLEANUP-RUN
   T-REPORT
   s" boot-row-test: " type WALKED @ FMT:.INT s"  rows, ok" type cr ;

MAIN

;package
;using
