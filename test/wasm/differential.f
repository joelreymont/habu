\ differential.f - W31: a program runs natively under bin/hb and as a module
\ `hb-build --target wasm` builds, wasm-tools validates and bun runs
\ (test/wasm/harness.f), both held to the same bytes. The rows that return are
\ one program, built once; a throw ends a run and a native fault writes a line
\ on stderr, so each such row is a program of its own. Both tools must be on
\ PATH, so test/wasm/device.f runs it, as does
\ `bin/hb --load test/wasm/differential.f` from the tree's root.
\ Sources, modules and the expected and actual output stay in the printed
\ directory.

require src/lib/error.f
require src/lib/string.f
require src/lib/fmt.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/test/assert.f
require src/lib/test/eval.f
require test/wasm/harness.f
require test/wasm/numeric-rows.f

package WASM-DIFFERENTIAL
private

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
CAP BUFFER: THROWN                     \ the record THROW-ROW expects in ERR
variable THROWN-U

: ARG+ ( str -- )  PROC-ARGV+ ;

: TEXT+ ( str ^u8 ^n -- )
   {: s:str buf:^ len:^ :}
   len @ s STR:LEN + CAP > if E-STR-CAPACITY throw then
   s STR:DATA  buf len @ +  s STR:LEN BYTE-COPY
   s STR:LEN len +! ;

: ALL+ ( str -- )  ALL ALL-U TEXT+ ;
: CALLS+ ( str -- )  CALLS CALLS-U TEXT+ ;
: WANT+ ( str -- )  WANT WANT-U TEXT+ ;
: PROG+ ( str -- )  PROG PROG-U TEXT+ ;
: THROWN+ ( str -- )  THROWN THROWN-U TEXT+ ;

: NAME$ ( -- str )  NAME-A @ NAME-U @ >str ;

: LABEL ( -- )  NAME$ T-LABEL ;

\ DIR's file named NAME and then suffix, written into buf; answers its length.
: PATH ( str ^u8 -- n )
   {: sfx:str buf:^ :}
   SB-RESET  NAME$ SB-APPEND  sfx SB-APPEND
   DIR DIR-U @ >str SB$ buf FS:JOIN-PATH ;

\ DIR's file named NAME and then suffix holds the bytes.
: KEEP ( str str -- )
   {: s:str sfx:str :}
   sfx FILE PATH {: fu:n :}
   FILE fu >str s FS:WRITE-ALL ;

\ The program's files: the source and a MAIN of the body as NAME.f, MAIN's call
\ as NAME-call.f, and its module's path, NAME.wasm.
: FILES ( str str str -- )
   {: name:str src:str body:str :}
   name STR:DATA NAME-A !  name STR:LEN NAME-U !
   s" .f" SRC PATH SRC-U !
   s" -call.f" CALLER PATH CALLER-U !
   s" .wasm" MODULE PATH MODULE-U !
   0 PROG-U !
   src PROG+  S\" \n: MAIN ( -- ) " PROG+  body PROG+  S\"  ;\n" PROG+
   SRC SRC-U @ >str PROG PROG-U @ >str FS:WRITE-ALL
   CALLER CALLER-U @ >str s" require " FS:WRITE-ALL
   CALLER CALLER-U @ >str SRC SRC-U @ >str FS:APPEND-FILE
   CALLER CALLER-U @ >str S\" \nMAIN\n" FS:APPEND-FILE ;

\ bin/hb run on the arguments staged after PROC-ARGV-RESET: the lengths of its
\ stdout and stderr, and its exit status.
: HB ( -- n n n )
   s" bin/hb"  OUT CAP >LEN  ERR CAP >LEN  PROCESS:WAIT-FOREVER
   RUN-ARGV-CAPTURE-OUTCOME PROCESS:ENDING>RC RC>N {: o:len e:len rc:n :}
   o LEN>N  e LEN>N  rc ;

\ The first line of the engine's record of an uncaught throw of code c, with its
\ line feed.
: THROWN-LINE$ ( n -- str )
   SB-RESET TEST-EVAL:THROWN-HEAD$ SB-APPEND  10 SB-APPEND-C  SB$ ;

\ The program natively: it prints want and exits rc; answers its stderr's
\ length.
: NATIVE-RUN ( str n -- n )
   {: want:str rc:n :}
   want s" -expected.txt" KEEP
   PROC-ARGV-RESET
   s" --load" ARG+  SRC SRC-U @ >str ARG+  CALLER CALLER-U @ >str ARG+
   HB {: o:n e:n r:n :}
   OUT o >str s" -native.txt" KEEP
   LABEL  r rc T=
   LABEL  OUT o >str want T$=
   e ;

\ NATIVE-RUN with nothing on stderr for 0, and for 85 the
\ record of the uncaught throw of code first.
: NATIVE ( str n n -- )
   {: want:str rc:n code:n :}
   want rc NATIVE-RUN {: e:n :}
   rc 0= if  LABEL  ERR e >str s" " T$=  then
   rc EXIT-STATUS:UNCAUGHT = if  LABEL  ERR e >str code THROWN-LINE$ STR:STARTS-WITH? TTRUE  then ;

\ The program as a module, valid and run: it prints want and answers status, a
\ throw's code whole; a run that returns writes nothing to ERR.
: WASM ( str n n -- )
   {: want:str status:n code:n :}
   SRC SRC-U @ >str  MODULE MODULE-U @ >str
   WASM-HARNESS:BUILD {: r:n :}
   LABEL  r 0 T=
   r 0<> if  exit  then
   LABEL  MODULE MODULE-U @ >str WASM-HARNESS:VALID? TTRUE
   LABEL  MODULE MODULE-U @ >str WASM-HARNESS:RUN status T=
   WASM-HARNESS:OUT$ s" -wasm.txt" KEEP
   LABEL  WASM-HARNESS:OUT$ want T$=
   status 0= if  LABEL  WASM-HARNESS:ERR$ s" " T$=  then
   status 1 = if  LABEL  WASM-HARNESS:THROW-CODE code T=  then ;

\ ---- W03's program ---------------------------------------------------------------
\ Word k takes k mod 12 cells and answers k / 12, for k below 144, so the
\ module has a type per word and its type and function indices pass 127, the
\ most a one-byte LEB holds. Word k answers the sum of its cells and k. Group g
\ calls the twelve words that answer g cells, each on 1, 2 and so on, and adds
\ every answer into the cell it takes; W03 runs the groups from 0.
12 constant SHAPES

: G+ ( str -- )
   {: s:str :}
   GEN-U @ s STR:LEN + GEN-RESERVE
   s STR:DATA  GEN-U @ GEN  s STR:LEN BYTE-COPY
   s STR:LEN GEN-U +! ;

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

: W03$ ( -- str )
   0 GEN-U !
   SHAPES SHAPES * 0 ?do  i WORD+  loop
   SHAPES 0 ?do  i GROUP+  loop
   s" : W03 ( -- ) 0" G+
   SHAPES 0 ?do  s"  W03-G" G+ i G#  loop
   s"  depth . .s drop ;" G+ NL
   0 GEN GEN-U @ >str ;

\ ---- the rows --------------------------------------------------------------------
: SETUP ( -- )
   s" wasm-differential" FS:MKTEMP-DIR {: s:str :}
   s STR:DATA DIR s STR:LEN BYTE-COPY  s STR:LEN DIR-U !
   s" wasm differential: sources and modules in " type  DIR s STR:LEN >str type cr ;

\ A row's NAME, the SOURCE defining it and the bytes it prints (numeric-rows.f) join
\ RETURNING's program.
: ROW ( str str str -- )
   {: name:str src:str want:str :}
   src ALL+  S\" \n" ALL+
   S\"  s\" " CALLS+  name CALLS+  S\" \" type cr " CALLS+  name CALLS+  S\" \n" CALLS+
   name WANT+  S\" \n" WANT+  want WANT+ ;

\ The returning rows natively and as one module: exit 0, empty stderr, status 0;
\ W05-FEATURE's f>s makes the module need saturating float-to-int to validate.
: RETURNING ( -- )
   s" rows" ALL ALL-U @ >str CALLS CALLS-U @ >str FILES
   WANT WANT-U @ >str 0 0 NATIVE
   WANT WANT-U @ >str 0 0 WASM
   LABEL  MODULE MODULE-U @ >str WASM-HARNESS:VALID-WITHOUT-SAT? TFALSE ;

\ THROW-ROW's program throws a code uncaught: natively the engine's record names
\ it; the module writes the record, with no place, to ERR and throws
\ 85.
: THROW-ROW ( str str str n -- )
   {: name:str src:str want:str code:n :}
   name src name FILES
   want EXIT-STATUS:UNCAUGHT code NATIVE
   0 THROWN-U !
   code THROWN-LINE$ THROWN+
   s"   = code: " THROWN+  SB-RESET code FMT:SB-INT SB$ THROWN+
   S\" \n  = help: catch the code where the program can go on, or remove its cause\n\n" THROWN+
   want 1 EXIT-STATUS:UNCAUGHT WASM
   LABEL  WASM-HARNESS:ERR$ THROWN THROWN-U @ >str T$= ;

false [if] \ off until dot habu-add-the-checks-a49d054d
\ Stderr is the native engine's record of a caught memory fault.
: FAULT-RECORD? ( str -- bool )
   S\" error[E-MEMORY-FAULT]: a read or write faulted\n" STR:STARTS-WITH? ;

\ FAULT-ROW's program catches a memory fault and ends with status 0; natively
\ the engine reports the fault on stderr, the module reports nothing.
: FAULT-ROW ( str str str -- )
   {: name:str src:str want:str :}
   name src name FILES
   want 0 NATIVE-RUN {: e:n :}
   LABEL  ERR e >str FAULT-RECORD? TTRUE
   want 0 0 WASM ;
[then]

;package

T-RESET
WASM-DIFFERENTIAL:SETUP
s" W05-FEATURE" s" : W05-FEATURE ( -- ) 1 s>f f>s . ;" s\" 1\n" WASM-DIFFERENTIAL:ROW
' WASM-DIFFERENTIAL:ROW WASM-NUMERIC-ROWS:EACH

\ ---- W01 a loop's edge copies form a cycle; nested exits keep their depths -------
\ Each turn swaps two cells, or rotates three, so the back edge copies cyclically.
s" W01-CYCLE"
s" : W01-CYCLE ( -- ) 1 2 5 0 do swap loop 3 4 5 4 0 do rot loop depth . .s 2drop 2drop drop ;"
s\" 5\n2\n1\n4\n5\n3\n" WASM-DIFFERENTIAL:ROW

\ The Collatz steps of 1 to 10: an if inside a while inside a do loop.
s" W01-NEST"
s" : W01-NEST ( -- ) 0 11 1 do i begin dup 1 <> while dup 1 and 0<> if 3 * 1+ else 2 / then swap 1+ swap repeat drop loop depth . .s drop ;"
s\" 1\n67\n" WASM-DIFFERENTIAL:ROW

\ leave from an if inside the inner of two do loops.
s" W01-LEAVE"
s" : W01-LEAVE ( -- ) 0 5 0 do 5 0 do i j + 4 > if leave then 1+ loop loop depth . .s drop ;"
s\" 1\n15\n" WASM-DIFFERENTIAL:ROW

\ ---- W03 type and function indices past a one-byte LEB ---------------------------
s" W03" WASM-DIFFERENTIAL:W03$ s\" 1\n96096\n" WASM-DIFFERENTIAL:ROW

\ ---- W06 16 lanes each way are direct, 17 take the frame; execute adapts both ----
\ Each FOLD weights its arguments by position, so a reordered pass or result fails.
s" W06"
s" : W06-FOLD16 ( n n n n n n n n n n n n n n n n -- n ) 2 * + 2 * + 2 * + 2 * + 2 * + 2 * + 2 * + 2 * + 2 * + 2 * + 2 * + 2 * + 2 * + 2 * + 2 * + ; : W06-FOLD17 ( n n n n n n n n n n n n n n n n n -- n ) W06-FOLD16 2 * + ; : W06-COUNT16 ( n -- n n n n n n n n n n n n n n n n ) dup 1+ dup 1+ dup 1+ dup 1+ dup 1+ dup 1+ dup 1+ dup 1+ dup 1+ dup 1+ dup 1+ dup 1+ dup 1+ dup 1+ dup 1+ ; : W06-COUNT17 ( n -- n n n n n n n n n n n n n n n n n ) W06-COUNT16 dup 1+ ; : W06 ( -- ) 1 W06-COUNT16 W06-FOLD16 . 1 W06-COUNT17 W06-FOLD17 . 1 ['] W06-COUNT16 execute ['] W06-FOLD16 execute . 1 ['] W06-COUNT17 execute ['] W06-FOLD17 execute . depth . ;"
s\" 983041\n2097153\n983041\n2097153\n0\n" WASM-DIFFERENTIAL:ROW

\ ---- W07 an uncaught throw prints its record, a memory fault is a throw ----------
\ MIN-N + 1, which a double rounds, caught and printed, then thrown uncaught.
s" W07-THROW"
s" : W07-THROW ( -- ) [: $8000000000000001 throw ;] catch . $8000000000000001 throw ;"
s\" -9223372036854775807\n" $8000000000000001 WASM-DIFFERENTIAL:THROW-ROW

false [if] \ off until dot habu-add-the-checks-a49d054d
\ A byte read in the null region throws E-MEMORY-FAULT through the catch around it.
s" W07-FAULT"
s" package W07-FAULT-CAST private CAST: >BYTE ( n -- ^u8 ) public : PEEK ( -- ) $FFFF >BYTE c@ drop ; ;package : W07-FAULT ( -- ) 2 . [: W07-FAULT-CAST:PEEK ;] catch . ;"
s\" 2\n110\n" WASM-DIFFERENTIAL:FAULT-ROW
[then]

\ ---- W31 a program's own storage -------------------------------------------------
\ A variable, a created buffer and a constant link as data; ! and c! store
\ through checked pointers.
s" W31-STORAGE"
s" variable W31-STORAGE-V create W31-STORAGE-B 2 allot 5 constant W31-STORAGE-K : W31-STORAGE ( -- ) W31-STORAGE-K W31-STORAGE-V ! W31-STORAGE-V @ 7 + W31-STORAGE-V ! W31-STORAGE-V @ . 65 W31-STORAGE-B c! 66 W31-STORAGE-B 1 + c! W31-STORAGE-B 2 >str type cr depth . ;"
s\" 12\nAB\n0\n" WASM-DIFFERENTIAL:ROW

\ true and false, which the map answers with kernel words, as conditions and as
\ cells.
s" W31-BOOLEANS"
s" : W31-BOOLEANS ( -- ) true if 1 else 0 then . false if 1 else 0 then . true false depth . .s 2drop ;"
s\" 1\n0\n2\n-1\n0\n" WASM-DIFFERENTIAL:ROW

\ f<= and then f>= on equal, lesser and greater reals, a NaN on either side and
\ the signed zeros both ways.
s" W31-ORDERS"
s" : W31-ORDERS-NAN ( -- r ) 0 s>f 0 s>f f/ ; : W31-ORDERS-NEG0 ( -- r ) 0 s>f fnegate ; : W31-ORDERS ( -- ) 1 s>f 1 s>f f<= 1 s>f 2 s>f f<= 2 s>f 1 s>f f<= W31-ORDERS-NAN 1 s>f f<= 1 s>f W31-ORDERS-NAN f<= W31-ORDERS-NEG0 0 s>f f<= 0 s>f W31-ORDERS-NEG0 f<= 1 s>f 1 s>f f>= 1 s>f 2 s>f f>= 2 s>f 1 s>f f>= W31-ORDERS-NAN 1 s>f f>= 1 s>f W31-ORDERS-NAN f>= W31-ORDERS-NEG0 0 s>f f>= 0 s>f W31-ORDERS-NEG0 f>= depth . .s 2drop 2drop 2drop 2drop 2drop 2drop 2drop ;"
s\" 14\n-1\n-1\n0\n0\n0\n-1\n-1\n-1\n0\n-1\n0\n0\n-1\n-1\n" WASM-DIFFERENTIAL:ROW

\ src/lib/fmath.f, whose words call f<= and f>=: sin and cos of 1 and of 10, past
\ pi/4, by their bits.
s" W31-FMATH"
s" require src/lib/fmath.f : W31-FMATH ( -- ) 1 s>f FMATH:SIN 1 s>f FMATH:COS 10 s>f FMATH:SIN 10 s>f FMATH:COS depth . .s fdrop fdrop fdrop fdrop ;"
s\" 4\n4605754516372524270\n4603041830072026764\n-4620296710764933294\n-4617639132858127585\n" WASM-DIFFERENTIAL:ROW

\ JSON fields, UTF-8 bytes and the signed minimum's text through the library's
\ writer.
s" WJSON:STEP"
S\" require src/lib/heap.f\nrequire src/lib/vector.f\nrequire src/lib/json.f\nrequire src/lib/json/write.f\npackage WJSON\nprivate\nTYPED-VARIABLE W JSON:writer\npublic\n: STEP ( -- )\n   HEAP:ALLOCATOR 0 [type] u8 VEC:NEW {: v:^ :}\n   W v JSON:OPEN JSON:WRITE-OBJECT-START\n   s\" name\" s\" é\" JSON:WRITE-FIELD-S\n   JSON:WRITE-COMMA\n   s\" min\" STR:MIN-I64 JSON:WRITE-FIELD-INT\n   JSON:WRITE-OBJECT-END JSON:WRITTEN$ type cr\n   W JSON:CLOSE  v VEC:DELETE ;\n;package\n"
S\" {\"name\":\"é\",\"min\":-9223372036854775808}\n" WASM-DIFFERENTIAL:ROW

\ MAP's keyed words, held to one order on native and wasm: INIT-KEYED under a
\ fixed key, SET, GET and EACH's visit order.
s" WMAP:STEP"
S\" require src/lib/map-core.f\npackage WMAP\nprivate\n8 constant ROOM\ncreate M ROOM >COUNT MAP:CELL-COUNT COUNT>N cells allot\n: SHOW ( str n -- ) {: s:str v:n :}\n   s type space v . ;\npublic\n: STEP ( -- )\n   M ROOM >COUNT $5EED $C0DE MAP:INIT-KEYED\n   1 M s\" one\" MAP:SET\n   2 M s\" two\" MAP:SET\n   3 M s\" three\" MAP:SET\n   M s\" two\" MAP:GET MATCH option\n     none OF -1 . ENDOF\n     some OF . ENDOF\n   ;MATCH\n   M ['] SHOW MAP:EACH ;\n;package\n"
S\" 2\ntwo 2\nthree 3\none 1\n" WASM-DIFFERENTIAL:ROW

\ Core byte copying is forward, including when the destination overlaps the
\ source by less than a cell. A memmove-style provider prints different bytes.
s" WBYTES:STEP"
S\" package WBYTES\nprivate\n16 BUFFER: B\npublic\n: STEP ( -- )\n   s\" 0123456789\" str> drop B 10 BYTE-COPY\n   B B 2 + 8 BYTE-COPY\n   B 10 >str type cr ;\n;package\n"
S\" 0101010101\n" WASM-DIFFERENTIAL:ROW

\ Camera numeric text reaches FMT's DATA scratch, STR>FLOAT's decimal gate
\ and JSON:READ-FLOAT through the same checked entry on native and Wasm.
s" WNUM:STEP"
s" require test/wasm/number-text.f"
S\" 1.2500000000000000e0\n-0.0000000000000000e0\n1.2500e-1\n-2.5000e-3\n-1\n-1\n0\n0\n0\n0\n-1\n-1\n-1\n-1\n-1\n" WASM-DIFFERENTIAL:ROW

WASM-DIFFERENTIAL:RETURNING
T-REPORT
