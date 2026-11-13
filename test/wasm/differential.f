\ differential.f - W31: a program runs natively under bin/hb and as a module
\ `hb-build --target wasm` builds, wasm-tools validates and bun runs
\ (test/wasm/harness.f), both held to the same bytes. The rows that return are
\ one program, built once; a throw ends a run and a native fault writes a line
\ on stderr, so each such row is a program of its own. Both tools must be on
\ PATH, so test/wasm/device.f runs it, as does
\ `bin/hb --load test/wasm/differential.f` from the tree's root.
\ Sources, modules and the expected and actual output stay in the printed
\ directory.

require src/lib/string.f
require src/lib/fmt.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/test.f
require test/wasm/harness.f

package WASM-DIFFERENTIAL
private

60000 constant DEADLINE-MS
65536 constant CAP
CAP BUFFER: OUT
CAP BUFFER: ERR
PATH-CAP BUFFER: DIR
variable DIR-U
PATH-CAP BUFFER: SRC
variable SRC-U
PATH-CAP BUFFER: CALLER
variable CALLER-U
PATH-CAP BUFFER: MODULE
variable MODULE-U
PATH-CAP BUFFER: FILE
PTR-VARIABLE NAME-A
variable NAME-U
DYNAMIC-BUFFER GEN u8                  \ W03's source
variable GEN-U

\ The returning rows' program: their sources, MAIN's body and what it prints.
CAP BUFFER: ALL
variable ALL-U
CAP BUFFER: CALLS
variable CALLS-U
CAP BUFFER: WANT
variable WANT-U
CAP BUFFER: PROG                       \ the program being written
variable PROG-U

: ARG+ ( ptr u8 n -- )  >LEN PROC-ARGV+ ;

: TEXT+ ( ptr u8 n ptr u8 ptr n -- )
   {: a:ptr u:n buf:ptr len:ptr :}
   len @ u + CAP > if E-STR-CAPACITY throw then
   a  buf len @ +  u BYTE-COPY
   u len +! ;

: ALL+ ( ptr u8 n -- )  ALL ALL-U TEXT+ ;
: CALLS+ ( ptr u8 n -- )  CALLS CALLS-U TEXT+ ;
: WANT+ ( ptr u8 n -- )  WANT WANT-U TEXT+ ;
: PROG+ ( ptr u8 n -- )  PROG PROG-U TEXT+ ;

: NAME$ ( -- ptr u8 n )  NAME-A @ NAME-U @ ;

: LABEL ( -- )  NAME$ T-LABEL ;

\ DIR's file named NAME and then suffix, written into buf; answers its length.
: PATH ( ptr u8 n ptr u8 -- n )
   {: sfx:ptr su:n buf:ptr :}
   SB-RESET  NAME$ SB-APPEND  sfx su SB-APPEND
   DIR DIR-U @ SB$ buf FS:JOIN-PATH ;

\ DIR's file named NAME and then suffix holds the bytes.
: KEEP ( ptr u8 n ptr u8 n -- )
   {: a:ptr u:n sfx:ptr su:n :}
   sfx su FILE PATH {: fu:n :}
   FILE fu a u FS:WRITE-ALL ;

\ The program's files: the source and a MAIN of the body as NAME.f, MAIN's call
\ as NAME-call.f, and its module's path, NAME.wasm.
: FILES ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: name:ptr nu:n src:ptr su:n body:ptr bu:n :}
   name NAME-A !  nu NAME-U !
   s" .f" SRC PATH SRC-U !
   s" -call.f" CALLER PATH CALLER-U !
   s" .wasm" MODULE PATH MODULE-U !
   0 PROG-U !
   src su PROG+  S\" \n: MAIN ( -- ) " PROG+  body bu PROG+  S\"  ;\n" PROG+
   SRC SRC-U @ PROG PROG-U @ FS:WRITE-ALL
   CALLER CALLER-U @ S\" MAIN\n" FS:WRITE-ALL ;

\ bin/hb run on the arguments staged after PROC-ARGV-RESET: the lengths of its
\ stdout and stderr, and its exit status.
: HB ( -- n n n )
   s" bin/hb" >LEN  OUT CAP >LEN  ERR CAP >LEN  DEADLINE-MS >MS
   RUN-ARGV-CAPTURE-OUTCOME PROCESS:ENDING>RC RC>N {: o:len e:len rc:n :}
   o LEN>N  e LEN>N  rc ;

\ The engine's line for an uncaught throw of code.
: UNCAUGHT$ ( n -- ptr u8 n )
   {: code:n :}
   SB-RESET  s" hb: uncaught throw code " SB-APPEND  code FMT:SB-INT  SB$ ;

\ The program natively: it prints want and exits rc; answers its stderr's
\ length.
: NATIVE-RUN ( ptr u8 n n -- n )
   {: want:ptr wu:n rc:n :}
   want wu s" -expected.txt" KEEP
   PROC-ARGV-RESET
   s" --load" ARG+  SRC SRC-U @ ARG+  CALLER CALLER-U @ ARG+
   HB {: o:n e:n r:n :}
   OUT o s" -native.txt" KEEP
   LABEL  r rc T=
   LABEL  OUT o want wu T$=
   e ;

\ NATIVE-RUN with nothing on stderr for 0 and the uncaught throw of code for
\ UNCAUGHT-RC.
: NATIVE ( ptr u8 n n n -- )
   {: want:ptr wu:n rc:n code:n :}
   want wu rc NATIVE-RUN {: e:n :}
   rc 0= if  LABEL  ERR e s" " T$=  then
   rc UNCAUGHT-RC = if  LABEL  ERR e RTRIM code UNCAUGHT$ T$=  then ;

\ The program as a module, valid and run: it prints want and answers status, a
\ throw's code whole.
: WASM ( ptr u8 n n n -- )
   {: want:ptr wu:n status:n code:n :}
   SRC SRC-U @ MODULE MODULE-U @ WASM-HARNESS:BUILD {: r:n :}
   LABEL  r 0 T=
   r 0<> if  exit  then
   LABEL  MODULE MODULE-U @ WASM-HARNESS:VALID? TTRUE
   LABEL  MODULE MODULE-U @ WASM-HARNESS:RUN status T=
   WASM-HARNESS:OUT$ s" -wasm.txt" KEEP
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
   s" wasm-differential" FS:MKTEMP-DIR {: a:ptr u:n :}
   a DIR u BYTE-COPY  u DIR-U !
   s" wasm differential: sources and modules in " type  DIR u type cr ;

\ A row's NAME, the SOURCE defining it and the bytes it prints (numeric-rows.f) join
\ RETURNING's program.
: ROW ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: name:ptr nu:n src:ptr su:n want:ptr wu:n :}
   src su ALL+  S\" \n" ALL+
   S\"  s\" " CALLS+  name nu CALLS+  S\" \" type cr " CALLS+  name nu CALLS+  S\" \n" CALLS+
   name nu WANT+  S\" \n" WANT+  want wu WANT+ ;

\ The returning rows natively and as one module: exit 0, empty stderr, status 0;
\ W05-FEATURE's f>s makes the module need saturating float-to-int to validate.
: RETURNING ( -- )
   s" rows" ALL ALL-U @ CALLS CALLS-U @ FILES
   WANT WANT-U @ 0 0 NATIVE
   WANT WANT-U @ 0 0 WASM
   LABEL  MODULE MODULE-U @ WASM-HARNESS:VALID-WITHOUT-SAT? TFALSE ;

\ THROW-ROW's program throws a code uncaught: natively the engine's uncaught
\ throw names it, and the run answers status 1 with the code whole.
: THROW-ROW ( ptr u8 n ptr u8 n ptr u8 n n -- )
   {: name:ptr nu:n src:ptr su:n want:ptr wu:n code:n :}
   name nu src su name nu FILES
   want wu UNCAUGHT-RC code NATIVE
   want wu 1 code WASM ;

\ Stderr is one line, the native engine's report of a caught memory fault.
: FAULT-LINE? ( ptr u8 n -- bool )
   RTRIM {: a:ptr u:n :}
   a u 10 INDEX-OF MATCH option
      some OF drop false ENDOF
      none OF a u s" E-MEMORY-FAULT habu: memory fault at " STARTS-WITH? ENDOF
   ;MATCH ;

\ FAULT-ROW's program catches a memory fault and ends with status 0; natively
\ the engine reports the fault on stderr, the module reports nothing.
: FAULT-ROW ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: name:ptr nu:n src:ptr su:n want:ptr wu:n :}
   name nu src su name nu FILES
   want wu 0 NATIVE-RUN {: e:n :}
   LABEL  ERR e FAULT-LINE? TTRUE
   want wu 0 0 WASM ;

;package

T-RESET
WASM-DIFFERENTIAL:SETUP
using WASM-DIFFERENTIAL
s" W05-FEATURE" s" : W05-FEATURE ( -- ) 1 s>f f>s . ;" s\" 1\n" ROW
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

\ ---- W07 a full-width throw is a status, a memory fault is a throw --------------
\ MIN-N + 1, which a double rounds, caught and printed, then thrown uncaught.
s" W07-THROW"
s" : W07-THROW ( -- ) [: $8000000000000001 throw ;] catch . $8000000000000001 throw ;"
s\" -9223372036854775807\n" $8000000000000001 THROW-ROW

\ A byte read in the null region throws E-MEMORY-FAULT through the catch around it.
s" W07-FAULT"
s" package W07-FAULT-CAST private CAST: >BYTE ( n -- ptr u8 ) public : PEEK ( -- ) $FFFF >BYTE c@ drop ; ;package : W07-FAULT ( -- ) 2 . [: W07-FAULT-CAST:PEEK ;] catch . ;"
s\" 2\n110\n" FAULT-ROW

\ ---- W31 a program's own storage -------------------------------------------------
\ A variable, a created buffer and a constant link as data; ! and c! store
\ through checked pointers.
s" W31-STORAGE"
s" variable W31-STORAGE-V create W31-STORAGE-B 2 allot 5 constant W31-STORAGE-K : W31-STORAGE ( -- ) W31-STORAGE-K W31-STORAGE-V ! W31-STORAGE-V @ 7 + W31-STORAGE-V ! W31-STORAGE-V @ . 65 W31-STORAGE-B c! 66 W31-STORAGE-B 1 + c! W31-STORAGE-B 2 type cr depth . ;"
s\" 12\nAB\n0\n" ROW

\ true and false, which the map answers with kernel words, as conditions and as
\ cells.
s" W31-BOOLEANS"
s" : W31-BOOLEANS ( -- ) true if 1 else 0 then . false if 1 else 0 then . true false depth . .s 2drop ;"
s\" 1\n0\n2\n-1\n0\n" ROW

\ f<= and then f>= on equal, lesser and greater reals, a NaN on either side and
\ the signed zeros both ways: prelude words, which the map answers with kernel
\ words.
s" W31-ORDERS"
s" : W31-ORDERS-NAN ( -- r ) 0 s>f 0 s>f f/ ; : W31-ORDERS-NEG0 ( -- r ) 0 s>f fnegate ; : W31-ORDERS ( -- ) 1 s>f 1 s>f f<= 1 s>f 2 s>f f<= 2 s>f 1 s>f f<= W31-ORDERS-NAN 1 s>f f<= 1 s>f W31-ORDERS-NAN f<= W31-ORDERS-NEG0 0 s>f f<= 0 s>f W31-ORDERS-NEG0 f<= 1 s>f 1 s>f f>= 1 s>f 2 s>f f>= 2 s>f 1 s>f f>= W31-ORDERS-NAN 1 s>f f>= 1 s>f W31-ORDERS-NAN f>= W31-ORDERS-NEG0 0 s>f f>= 0 s>f W31-ORDERS-NEG0 f>= depth . .s 2drop 2drop 2drop 2drop 2drop 2drop 2drop ;"
s\" 14\n-1\n-1\n0\n0\n0\n-1\n-1\n-1\n0\n-1\n0\n0\n-1\n-1\n" ROW

\ src/lib/fmath.f, whose words call f<= and f>=: sin and cos of 1 and of 10, past
\ pi/4, by their bits.
s" W31-FMATH"
s" require src/lib/fmath.f : W31-FMATH ( -- ) 1 s>f FMATH:SIN 1 s>f FMATH:COS 10 s>f FMATH:SIN 10 s>f FMATH:COS depth . .s fdrop fdrop fdrop fdrop ;"
s\" 4\n4605754516372524270\n4603041830072026764\n-4620296710764933294\n-4617639132858127585\n" ROW

\ JSON fields, UTF-8 bytes and the signed minimum's text through the library's
\ writer.
s" WJSON:STEP"
S\" require src/lib/json.f\npackage WJSON\nprivate\nTYPED-VARIABLE W JSON:writer\n128 BUFFER: B\npublic\n: STEP ( -- )\n   W B 128 JSON:WRITE-OPEN JSON:WRITE-OBJECT-START\n   s\" name\" s\" é\" JSON:WRITE-FIELD-S\n   JSON:WRITE-COMMA\n   s\" min\" STR-MIN-I64 JSON:WRITE-FIELD-INT\n   JSON:WRITE-OBJECT-END JSON:WRITTEN$ type cr ;\n;package\n"
S\" {\"name\":\"é\",\"min\":-9223372036854775808}\n" ROW

\ Core byte copying is forward, including when the destination overlaps the
\ source by less than a cell. A memmove-style provider prints different bytes.
s" WBYTES:STEP"
S\" package WBYTES\nprivate\n16 BUFFER: B\npublic\n: STEP ( -- )\n   s\" 0123456789\" drop B 10 BYTE-COPY\n   B B 2 + 8 BYTE-COPY\n   B 10 type cr ;\n;package\n"
S\" 0101010101\n" ROW

\ Camera numeric text reaches FMT's DATA scratch, STR>FLOAT's decimal gate
\ and JSON:READ-FLOAT through the same checked entry on native and Wasm.
s" WNUM:STEP"
s" require test/wasm/number-text.f"
S\" 1.2500000000000000e0\n-0.0000000000000000e0\n1.2500e-1\n-2.5000e-3\n-1\n-1\n0\n0\n0\n0\n-1\n-1\n-1\n-1\n-1\n" ROW

RETURNING
;using
T-REPORT
