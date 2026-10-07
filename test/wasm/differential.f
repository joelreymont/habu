\ differential.f - W31: each row's program run natively by bin/hb and built by
\ tools/wasm-build.f into a module that wasm-tools validates and bun runs
\ (test/wasm/harness.f), both sides held to the row. Both tools must be on
\ PATH, so this is no row of the ordinary gate: test/wasm/device.f runs it, as
\ does `bin/hb --load test/wasm/differential.f` from the tree's root.
\
\ A row is a NAME, a SOURCE that defines it and the EXPECTED bytes it prints
\ (test/wasm/numeric-rows.f). Natively `bin/hb --load` loads SOURCE and then a
\ file that calls NAME; for Wasm SOURCE is built with entry NAME. Each side
\ prints EXPECTED and ends as the row says. ROW's program returns: it exits 0
\ with nothing on stderr, and its run answers status 0. THROW-ROW's throws a
\ code uncaught: natively the engine's uncaught throw names it, and the run
\ answers status 1 with the code whole. TRAP-ROW's traps: natively through the
\ crash handler, and in Wasm as a trap, status 2, which no catch takes.
\
\ The rows: N01-N06 with the three NaN print rows, and W32's three ?do rows,
\ from test/wasm/numeric-rows.f; W01's cyclic edge copies and nested label
\ depths; W03's 144 signatures; W06's 16 and 17 lanes each way, called
\ directly and through execute; W07's full-width throw and its trap; and W31's
\ own storage. No Habu control word builds the cycle entered at two blocks
\ W02 needs, so its refusal stays structural (test/wasm/structure.f).
\
\ The sources and modules stay in the printed directory.

require lib/string.f
require lib/fmt.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/test.f
require test/wasm/harness.f

package WASM-DIFFERENTIAL
private

60000 constant DEADLINE-MS
4096 constant CAP
134 constant CRASH-RC                  \ native's crash handler exits so (src/habu/crash.f)
CAP BUFFER: OUT
CAP BUFFER: ERR
FS-PATH-CAP BUFFER: DIR
variable DIR-U
FS-PATH-CAP BUFFER: SRC
variable SRC-U
FS-PATH-CAP BUFFER: CALLER
variable CALLER-U
FS-PATH-CAP BUFFER: MODULE
variable MODULE-U
PTR-VARIABLE NAME-A
variable NAME-U
DYNAMIC-BUFFER GEN u8                  \ W03's source
variable GEN-U

: ARG+ ( ptr u8 n -- )  >LEN PROC-ARGV+ ;

: NAME$ ( -- ptr u8 n )  NAME-A @ NAME-U @ ;

: LABEL ( -- )  NAME$ T-LABEL ;

\ DIR's file named NAME and then suffix, written into buf; answers its length.
: PATH ( ptr u8 n ptr u8 -- n )
   {: sfx:ptr su:n buf:ptr :}
   SB-RESET  NAME$ SB-APPEND  sfx su SB-APPEND
   DIR DIR-U @ SB$ buf JOIN-PATH ;

\ The row's files: SOURCE as NAME.f, NAME's call as NAME-call.f, and its
\ module's path, NAME.wasm.
: FILES ( ptr u8 n ptr u8 n -- )
   {: name:ptr nu:n src:ptr su:n :}
   name NAME-A !  nu NAME-U !
   s" .f" SRC PATH SRC-U !
   s" -call.f" CALLER PATH CALLER-U !
   s" .wasm" MODULE PATH MODULE-U !
   SRC SRC-U @ src su WRITE-ALL
   SB-RESET  NAME$ SB-APPEND  S\" \n" SB-APPEND
   CALLER CALLER-U @ SB$ WRITE-ALL ;

\ bin/hb run on the arguments staged after PROC-ARGV-RESET: the lengths of its
\ stdout and stderr, and its exit status.
: HB ( -- n n n )
   s" bin/hb" >LEN  OUT CAP >LEN  ERR CAP >LEN  DEADLINE-MS >MS
   RUN-ARGV-CAPTURE-OUTCOME PROC-OUTCOME>RC RC>N {: o:len e:len rc:n :}
   o LEN>N  e LEN>N  rc ;

\ The engine's line for an uncaught throw of code.
: UNCAUGHT$ ( n -- ptr u8 n )
   {: code:n :}
   SB-RESET  s" hb: uncaught throw code " SB-APPEND  code FMT:SB-INT  SB$ ;

\ SOURCE then NAME, natively: they print want and exit rc, with nothing on
\ stderr for 0 and the uncaught throw of code for UNCAUGHT-RC.
: NATIVE ( ptr u8 n n n -- )
   {: want:ptr wu:n rc:n code:n :}
   PROC-ARGV-RESET
   s" --load" ARG+  SRC SRC-U @ ARG+  CALLER CALLER-U @ ARG+
   HB {: o:n e:n r:n :}
   LABEL  r rc T=
   LABEL  OUT o want wu T$=
   rc 0= if  LABEL  ERR e s" " T$=  then
   rc UNCAUGHT-RC = if  LABEL  ERR e RTRIM code UNCAUGHT$ T$=  then ;

\ SOURCE built with entry NAME, valid and run: it prints want and answers
\ status, a throw's code whole. A build that fails prints why.
: WASM ( ptr u8 n n n -- )
   {: want:ptr wu:n status:n code:n :}
   PROC-ARGV-RESET
   s" --load" ARG+  s" tools/wasm-build.f" ARG+  s" --" ARG+
   SRC SRC-U @ ARG+  NAME$ ARG+  MODULE MODULE-U @ ARG+
   HB {: o:n e:n r:n :}
   LABEL  r 0 T=
   r 0<> if  ERR e type  exit  then
   LABEL  MODULE MODULE-U @ WASM-HARNESS:VALID? TTRUE
   LABEL  MODULE MODULE-U @ WASM-HARNESS:RUN status T=
   LABEL  WASM-HARNESS:OUT$ want wu T$=
   status 1 = if  LABEL  WASM-HARNESS:THROW-CODE code T=  then ;

\ ---- W03's program ---------------------------------------------------------------
\ Word k takes k mod 12 cells and answers k / 12, for k below 144, so the
\ module has a type per word and its type and function indices pass 127, the
\ most a one-byte LEB holds. Word k answers the sum of its cells and k. Group g
\ calls the twelve words that answer g cells, each on 1, 2 and so on, and adds
\ every answer into the cell it takes; W03 runs the groups from 0.
12 constant SHAPES

: G+ ( ptr u8 n -- )
   {: a:ptr u:n :}
   GEN-U @ u + GEN-RESERVE
   a  GEN-U @ GEN  u BYTE-COPY
   u GEN-U +! ;

: G# ( n -- )  SB-RESET FMT:SB-U SB$ G+ ;

: NL ( -- )  S\" \n" G+ ;

: CELLS+ ( n -- )  0 ?do  s"  n" G+  loop ;

: WORD+ ( n -- )
   {: k:n :}
   k SHAPES mod {: a:n :}
   k SHAPES / {: b:n :}
   s" : W03-" G+ k G#  s"  (" G+ a CELLS+ s"  --" G+ b CELLS+ s"  )" G+
   a 0= if
      s"  " G+ k G#
   else
      a 1- 0 ?do  s"  +" G+  loop  s"  " G+ k G# s"  +" G+
   then
   b 0= if  s"  drop" G+  else  b 1- 0 ?do  s"  dup" G+  loop  then
   s"  ;" G+ NL ;

: CALL+ ( n -- )
   {: k:n :}
   k SHAPES mod 0 ?do  s"  " G+ i 1+ G#  loop
   s"  W03-" G+ k G#
   k SHAPES / 0 ?do  s"  +" G+  loop
   NL ;

: GROUP+ ( n -- )
   {: g:n :}
   s" : W03-G" G+ g G#  s"  ( n -- n )" G+ NL
   SHAPES 0 ?do  g SHAPES * i + CALL+  loop
   s"  ;" G+ NL ;

public

: W03$ ( -- ptr u8 n )
   0 GEN-U !
   SHAPES SHAPES * 0 ?do  i WORD+  loop
   SHAPES 0 ?do  i GROUP+  loop
   s" : W03 ( -- ) 0" G+
   SHAPES 0 ?do  s"  W03-G" G+ i G#  loop
   s"  depth . .s drop ;" G+ NL
   0 GEN GEN-U @ ;

\ ---- the rows --------------------------------------------------------------------
: SETUP ( -- )
   s" wasm-differential" HB-TMP-MKDIR {: a:ptr u:n :}
   a DIR u BYTE-COPY  u DIR-U !
   s" wasm differential: sources and modules in " type  DIR u type cr ;

\ A row whose program returns.
: ROW ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: name:ptr nu:n src:ptr su:n want:ptr wu:n :}
   name nu src su FILES
   want wu 0 0 NATIVE
   want wu 0 0 WASM ;

\ A row whose program throws code uncaught.
: THROW-ROW ( ptr u8 n ptr u8 n ptr u8 n n -- )
   {: name:ptr nu:n src:ptr su:n want:ptr wu:n code:n :}
   name nu src su FILES
   want wu UNCAUGHT-RC code NATIVE
   want wu 1 code WASM ;

\ A row whose program traps.
: TRAP-ROW ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: name:ptr nu:n src:ptr su:n want:ptr wu:n :}
   name nu src su FILES
   want wu CRASH-RC 0 NATIVE
   want wu 2 0 WASM ;

\ W05's generated conversion requires saturating float-to-int validation.
: W05-ROW ( -- )
   s" W05-FEATURE"
   s" : W05-FEATURE ( -- ) 1 s>f f>s . ;"
   S\" 1\n" ROW
   LABEL  MODULE MODULE-U @ WASM-HARNESS:VALID-WITHOUT-SAT? TFALSE ;

;package

T-RESET
WASM-DIFFERENTIAL:SETUP
using WASM-DIFFERENTIAL
W05-ROW
include test/wasm/numeric-rows.f

\ ---- W01 a loop's edge copies form a cycle; nested exits keep their depths -------
\ Each turn swaps two cells, or rotates three, so the back edge copies cyclically.
s" W01-CYCLE"
s" : W01-CYCLE ( -- ) 1 2 5 0 do swap loop 3 4 5 4 0 do rot loop depth . .s 2drop 2drop drop ;"
s\" 5\n2\n1\n4\n5\n3\n" ROW

\ The Collatz steps of 1 to 10: an if inside a while inside a do loop.
s" W01-NEST"
s" : W01-NEST ( -- ) 0 11 1 do i begin dup 1 <> while dup 1 and 0<> if 3 * 1+ else 2 / then swap 1+ swap repeat drop loop depth . .s drop ;"
s\" 1\n67\n" ROW

\ leave from an if inside the inner of two do loops.
s" W01-LEAVE"
s" : W01-LEAVE ( -- ) 0 5 0 do 5 0 do i j + 4 > if leave then 1+ loop loop depth . .s drop ;"
s\" 1\n15\n" ROW

\ ---- W03 type and function indices past a one-byte LEB ---------------------------
s" W03" W03$ s\" 1\n96096\n" ROW

\ ---- W06 16 lanes each way are direct, 17 take the frame; execute adapts both ----
\ Each FOLD weights its arguments by position, so a reordered pass or result fails.
s" W06"
s" : W06-FOLD16 ( n n n n n n n n n n n n n n n n -- n ) 2 * + 2 * + 2 * + 2 * + 2 * + 2 * + 2 * + 2 * + 2 * + 2 * + 2 * + 2 * + 2 * + 2 * + 2 * + ; : W06-FOLD17 ( n n n n n n n n n n n n n n n n n -- n ) W06-FOLD16 2 * + ; : W06-COUNT16 ( n -- n n n n n n n n n n n n n n n n ) dup 1+ dup 1+ dup 1+ dup 1+ dup 1+ dup 1+ dup 1+ dup 1+ dup 1+ dup 1+ dup 1+ dup 1+ dup 1+ dup 1+ dup 1+ ; : W06-COUNT17 ( n -- n n n n n n n n n n n n n n n n n ) W06-COUNT16 dup 1+ ; : W06 ( -- ) 1 W06-COUNT16 W06-FOLD16 . 1 W06-COUNT17 W06-FOLD17 . 1 ['] W06-COUNT16 execute ['] W06-FOLD16 execute . 1 ['] W06-COUNT17 execute ['] W06-FOLD17 execute . depth . ;"
s\" 983041\n2097153\n983041\n2097153\n0\n" ROW

\ ---- W07 a full-width throw is a status, a trap is not ---------------------------
\ MIN-N + 1, which a double rounds, caught and printed, then thrown uncaught.
s" W07-THROW"
s" : W07-THROW ( -- ) [: $8000000000000001 throw ;] catch . $8000000000000001 throw ;"
s\" -9223372036854775807\n" $8000000000000001 THROW-ROW

\ A byte read in the null region traps through the catch around it.
s" W07-TRAP"
s" package W07-TRAP-CAST private CAST: >BYTE ( n -- ptr u8 ) public : PEEK ( -- ) $FFFF >BYTE c@ drop ; ;package : W07-TRAP ( -- ) 2 . [: W07-TRAP-CAST:PEEK ;] catch . ;"
s\" 2\n" TRAP-ROW

\ ---- W31 a program's own storage -------------------------------------------------
\ A variable, a created buffer and a constant link as data; ! and c! store
\ through checked pointers.
s" W31-STORAGE"
s" variable W31-STORAGE-V create W31-STORAGE-B 2 allot 5 constant W31-STORAGE-K : W31-STORAGE ( -- ) W31-STORAGE-K W31-STORAGE-V ! W31-STORAGE-V @ 7 + W31-STORAGE-V ! W31-STORAGE-V @ . 65 W31-STORAGE-B c! 66 W31-STORAGE-B 1 + c! W31-STORAGE-B 2 type cr depth . ;"
s\" 12\nAB\n0\n" ROW

;using
T-REPORT
