\ hb-build.f - `hb-build --target wasm` run by its command line, each module it
\ writes validated by wasm-tools and run by bun (test/wasm/harness.f). A program
\ whose build status, output or trap is under test has its own build process; the
\ six that only run to status 0 share one. test/wasm/device.f runs it, as does
\ `bin/hb --load test/wasm/hb-build.f` from the tree's root. The sources and
\ modules stay in the printed directory.
\
\ A generated program fails check k by reading the byte at address k, below
\ WPROF:CTX-BASE, which traps. In the module the six share, OUT's letters name
\ the program a failed check ran in, and fault-addr is the check's number.

require src/lib/string.f
require src/lib/fmt.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/test.f
require src/core/engine-error.f
require src/arch/wasm/profile.f
require src/arch/wasm/leb.f
require test/wasm/harness.f

package WASM-HB-BUILD-TEST
private

60000 constant DEADLINE-MS
4096 constant CAP
CAP BUFFER: OUT
CAP BUFFER: ERR
variable ERR-U
PATH-CAP BUFFER: DIR
variable DIR-U
PATH-CAP BUFFER: SRC
variable SRC-U
PATH-CAP BUFFER: MOD
variable MOD-U

: SETUP ( -- )
   s" wasm-hb-build" FS:MKTEMP-DIR {: a:ptr u:n :}
   a DIR u BYTE-COPY  u DIR-U !
   s" wasm hb-build: sources and modules in " type  DIR u type cr ;

: ARG+ ( ptr u8 n -- )  >LEN PROC-ARGV+ ;

: DRIVER ( -- n )
   s" bin/hb" >LEN  OUT CAP >LEN  ERR CAP >LEN  DEADLINE-MS >MS
   RUN-ARGV-CAPTURE-OUTCOME PROCESS:ENDING>RC RC>N {: outu:len erru:len rc:n :}
   erru LEN>N ERR-U !
   rc ;

: SOURCE! ( ptr u8 n ptr u8 n -- )
   {: text:ptr tu:n name:ptr nu:n :}
   DIR DIR-U @ name nu SRC FS:JOIN-PATH SRC-U !
   SRC SRC-U @ text tu FS:WRITE-ALL ;

: START ( ptr u8 n -- )
   {: name:ptr nu:n :}
   DIR DIR-U @ name nu MOD FS:JOIN-PATH MOD-U !
   PROC-ARGV-RESET
   s" --load" ARG+  s" tools/hb-build.f" ARG+  s" --" ARG+ ;

: WASM+ ( -- )  s" --target" ARG+  s" wasm" ARG+ ;
: SRC+ ( -- )  SRC SRC-U @ ARG+ ;
: MOD+ ( -- )  s" -o" ARG+  MOD MOD-U @ ARG+ ;

\ The source's module at the name, by `hb-build --target wasm`; answers its exit status.
: BUILD ( ptr u8 n -- n )
   START  WASM+  SRC+  MOD+  DRIVER ;

\ `hb-build --engine --target wasm`, and `--repl --target wasm` of the source,
\ each writing to the name and answering its exit status.
: BUILD-ENGINE ( ptr u8 n -- n )
   START  s" --engine" ARG+  WASM+  MOD+  DRIVER ;

: BUILD-REPL ( ptr u8 n -- n )
   START  s" --repl" ARG+  WASM+  SRC+  MOD+  DRIVER ;

: RAN ( -- n )
   MOD MOD-U @ WASM-HARNESS:VALID? TTRUE
   MOD MOD-U @ WASM-HARNESS:RUN ;


\ m.f, and a file of requires whose paths hold DIR, may outgrow SB-CAP: their
\ text is built here.
CAP BUFFER: TEXT
variable TEXT-U

: TEXT+ ( ptr u8 n -- )
   {: a:ptr u:n :}
   TEXT-U @ u + CAP > if E-STR-CAPACITY throw then
   a TEXT TEXT-U @ + u BYTE-COPY  u TEXT-U +! ;

: REQUIRE+ ( ptr u8 n -- )
   {: name:ptr nu:n :}
   s" require " TEXT+  DIR DIR-U @ TEXT+  s" /" TEXT+  name nu TEXT+  S\" \n" TEXT+ ;


: EMIT-CASE ( -- )
   s" : MAIN ( -- ) 53 emit ;" s" emit.f" SOURCE!
   s" `53 emit` builds into a module that validates, runs to status 0 and prints 5" T-LABEL
   s" emit.wasm" BUILD 0 T=
   RAN 0 T=
   WASM-HARNESS:OUT$ s" 5" T$= ;

: EMITS! ( n ptr u8 n -- )
   {: reps:n name:ptr nu:n :}
   SB-RESET
   s" : MAIN ( -- ) " SB-APPEND  reps FMT:SB-U  s"  0 do 120 emit loop ;" SB-APPEND
   SB$ name nu SOURCE! ;

: FULL-CASE ( -- )
   WPROF:OUT-CAP s" full.f" EMITS!
   s" emit fills OUT to its last byte" T-LABEL
   s" full.wasm" BUILD 0 T=
   RAN 0 T=
   WASM-HARNESS:OUT$ nip WPROF:OUT-CAP T=
   WPROF:OUT-CAP 1+ s" over.f" EMITS!
   s" an emit past OUT traps" T-LABEL
   s" over.wasm" BUILD 0 T=
   RAN 2 T= ;


\ Package K at the name, its first test comparing 5 FACT with want.
: K! ( n ptr u8 n -- )
   {: want:n name:ptr nu:n :}
   SB-RESET
   S\" package K\nprivate\nCAST: >BYTES ( n -- ptr u8 )\nvariable SEED\n9 SEED !\n" SB-APPEND
   S\" variable ACC\n8 BUFFER: SPOT\n" SB-APPEND
   S\" : FACT ( n -- n ) dup 1 > if dup 1 - recurse * else drop 1 then ;\n" SB-APPEND
   S\" : SUM-TO ( n -- n ) 0 swap 0 ?do i + loop ;\n" SB-APPEND
   S\" : BUMP ( n -- ) ACC @ + ACC ! ;\npublic\n: MAIN ( -- )\n   5 FACT " SB-APPEND
   want FMT:SB-U
   S\"  <> if 1 >BYTES c@ drop then\n   10 SUM-TO 45 <> if 2 >BYTES c@ drop then\n" SB-APPEND
   S\"    SEED @ 9 <> if 3 >BYTES c@ drop then\n" SB-APPEND
   S\"    0 ACC !  7 BUMP 8 BUMP  ACC @ 15 <> if 4 >BYTES c@ drop then\n" SB-APPEND
   S\"    65 SPOT c!  SPOT c@ 65 <> if 5 >BYTES c@ drop then\n" SB-APPEND
   S\"    [: 1 ;] drop ;\n;package\n" SB-APPEND
   SB$ name nu SOURCE! ;

: PROGRAM-CASES ( -- )
   121 s" k-bad.f" K!
   0 TEXT-U !  s" k-bad.f" REQUIRE+  S\" : MAIN ( -- ) K:MAIN ;\n" TEXT+
   TEXT TEXT-U @ s" k-fail.f" SOURCE!
   s" the same program with a failing first test throws E-MEMORY-FAULT" T-LABEL
   s" k-fail.wasm" BUILD 0 T=
   RAN 1 T=
   WASM-HARNESS:THROW-CODE ENGINE-ERROR:MEMORY-FAULT T=
   WASM-HARNESS:FAULT-KIND WPROF:CRASH-RC T=
   WASM-HARNESS:FAULT-ADDR 1 T=
   S\" package K private CAST: >BYTES ( n -- ptr u8 ) public : MAIN ( -- ) 65 8 >BYTES c! ; ;package : MAIN ( -- ) K:MAIN ;"
   s" low.f" SOURCE!
   s" a c! below WPROF:CTX-BASE faults and throws E-MEMORY-FAULT" T-LABEL
   s" low.wasm" BUILD 0 T=
   RAN 1 T=
   WASM-HARNESS:THROW-CODE ENGINE-ERROR:MEMORY-FAULT T=
   WASM-HARNESS:FAULT-KIND WPROF:CRASH-RC T=
   WASM-HARNESS:FAULT-ADDR 8 T= ;


\ Package D in d.f: division, max, invert and rshift, atomics and views through
\ words whose operands are not literals at the call.
: D! ( -- )
   SB-RESET
   S\" package D\nprivate\nCAST: >BYTES ( n -- ptr u8 )\nvariable CELL0\nvariable SPOT\n" SB-APPEND
   S\" : DIV ( n n -- n ) / ;\n: REM ( n n -- n ) mod ;\n: BIG ( n n -- n ) max ;\n" SB-APPEND
   S\" : MIN-N ( -- n ) 1 63 lshift ;\n: FAIL ( n -- ) >BYTES c@ drop ;\n" SB-APPEND
   S\" : FLIP ( n -- n ) invert 3 rshift ;\npublic\n" SB-APPEND
   S\" : MAIN ( -- )\n   17 5 DIV 3 <> if 1 FAIL then  17 5 REM 2 <> if 2 FAIL then\n" SB-APPEND
   S\"    -17 5 DIV -3 <> if 3 FAIL then  -17 5 REM -2 <> if 4 FAIL then\n" SB-APPEND
   S\"    MIN-N -1 DIV MIN-N <> if 5 FAIL then  MIN-N -1 REM 0 <> if 6 FAIL then\n" SB-APPEND
   S\"    -3 7 BIG 7 <> if 7 FAIL then  7 -3 BIG 7 <> if 8 FAIL then\n" SB-APPEND
   S\"    5 CELL0 atomic!  CELL0 @ 5 <> if 9 FAIL then\n" SB-APPEND
   S\"    5 9 CELL0 atomic-cas 5 <> if 10 FAIL then  CELL0 @ 9 <> if 11 FAIL then\n" SB-APPEND
   S\"    5 1 CELL0 atomic-cas 9 <> if 12 FAIL then  CELL0 @ 9 <> if 13 FAIL then\n" SB-APPEND
   S\"    77 SPOT !  SPOT byte-view c@ 77 <> if 14 FAIL then\n" SB-APPEND
   S\"    SPOT cell-view @ 77 <> if 15 FAIL then\n" SB-APPEND
   S\"    5 FLIP $1FFFFFFFFFFFFFFF <> if 16 FAIL then ;\n;package\n" SB-APPEND
   SB$ s" d.f" SOURCE! ;

: DIV0-CASE ( -- )
   s" : DIV ( n n -- n ) / ;  : MAIN ( -- ) 1 0 DIV drop ;" s" div0.f" SOURCE!
   s" a zero divisor runs to status 1 and throws E-DIV-ZERO" T-LABEL
   s" div0.wasm" BUILD 0 T=
   RAN 1 T=
   WASM-HARNESS:THROW-CODE E-DIV-ZERO T= ;


\ The program f.f: each float word through a word whose operands are not
\ literals at the call, and a double out of an if's two arms, printed by `f.`
\ and `.`.
: F! ( -- )
   SB-RESET
   S\" : ADD ( r r -- r ) f+ ;\n: SUB ( r r -- r ) f- ;\n: MUL ( r r -- r ) f* ;\n" SB-APPEND
   S\" : DIV ( r r -- r ) f/ ;\n: ROOT ( r -- r ) fsqrt ;\n: FLIP ( r -- r ) fnegate ;\n" SB-APPEND
   S\" : SIZE ( r -- r ) fabs ;\n: LESS ( r r -- ) f< bool>n . ;\n" SB-APPEND
   S\" : MORE ( r r -- ) f> bool>n . ;\n: SAME ( r r -- ) f= bool>n . ;\n" SB-APPEND
   S\" : BELOW0 ( r -- ) f0< bool>n . ;\n: AT0 ( r -- ) f0= bool>n . ;\n" SB-APPEND
   S\" : FLOAT ( n -- r ) s>f ;\n: TRUNC ( r -- n ) f>s ;\n" SB-APPEND
   S\" : CHOOSE ( n -- r ) 0<> if 1.5 else 2.5 then ;\n" SB-APPEND
   S\" : BIG ( -- r ) 1000000000000000.0 dup MUL ;\n: MAIN ( -- )\n" SB-APPEND
   S\"    1.5 2.25 ADD f.  1.5 2.25 SUB f.  1.5 2.25 MUL f.  1.5 0.25 DIV f.\n" SB-APPEND
   S\"    2.0 ROOT f.  -4.0 ROOT dup TRUNC .  dup SAME\n   -2.5 FLIP f.  -2.5 SIZE f.\n" SB-APPEND
   S\"    1.0 2.0 LESS  2.0 1.0 LESS  2.0 1.0 MORE  1.0 2.0 MORE  1.0 1.0 SAME  1.0 2.0 SAME\n" SB-APPEND
   S\"    0.0 FLIP BELOW0  0.0 BELOW0  -1.0 BELOW0  0.0 FLIP AT0  0.0 AT0  -1.0 AT0\n" SB-APPEND
   S\"    -7 FLOAT f.  2.9 TRUNC .  -2.9 TRUNC .  BIG TRUNC .  BIG FLIP TRUNC .\n" SB-APPEND
   S\"    1 CHOOSE f.  0 CHOOSE f. ;\n" SB-APPEND
   SB$ s" f.f" SOURCE! ;

\ What F!'s MAIN prints on the host: the square root of -4 is a NaN, which
\ truncates to 0 and equals nothing; 1e30 and -1e30 truncate to MAX-N and MIN-N.
: F-HOST$ ( -- ptr u8 n )
   SB-RESET
   S\" 3.750000\n-0.750000\n3.375000\n6.000000\n1.414213\n0\n0\n2.500000\n2.500000\n" SB-APPEND
   S\" -1\n0\n-1\n0\n-1\n0\n0\n0\n-1\n-1\n-1\n0\n" SB-APPEND
   S\" -7.000000\n2\n-2\n9223372036854775807\n-9223372036854775808\n" SB-APPEND
   S\" 1.500000\n2.500000\n" SB-APPEND
   SB$ ;

: FLOAT-CASE ( -- )
   F!
   s" the float words build into a module that prints what the host prints" T-LABEL
   s" f.wasm" BUILD 0 T=
   RAN 0 T=
   WASM-HARNESS:OUT$ F-HOST$ T$= ;


\ The program loops.f: +loop up, down and not entered, j, r@, leave, until and again.
: LOOPS! ( -- )
   SB-RESET
   S\" : STEP3 ( -- n ) 0 10 0 ?do i + 3 +loop ;\n: DOWN ( -- n ) 0 0 5 ?do i + -1 +loop ;\n" SB-APPEND
   S\" : NONE ( -- n ) 0 5 5 ?do i + 1 +loop ;\n" SB-APPEND
   S\" : OUTER ( -- n ) 0 3 0 do 4 0 do j + loop loop ;\n: PEEK ( n -- n ) >r r@ r> + ;\n" SB-APPEND
   S\" : QUIT5 ( -- n ) 0 10 0 do i 5 = if leave then 1 + loop ;\n" SB-APPEND
   S\" : UNTIL3 ( -- n ) 0 begin 1 + dup 3 = until ;\n" SB-APPEND
   S\" : AGAIN4 ( -- n ) 0 begin 1 + dup 4 = if exit then again ;\n" SB-APPEND
   S\" : MAIN ( -- ) STEP3 . DOWN . NONE . OUTER . 7 PEEK . QUIT5 . UNTIL3 . AGAIN4 . ;\n" SB-APPEND
   SB$ s" loops.f" SOURCE! ;

: LOOPS-HOST$ ( -- ptr u8 n )  S\" 18\n15\n0\n12\n14\n5\n3\n4\n" ;

: LOOP-CASE ( -- )
   LOOPS!
   s" +loop up, down and not entered, j, r@, leave, until and again print what the host prints" T-LABEL
   s" loops.wasm" BUILD 0 T=
   RAN 0 T=
   WASM-HARNESS:OUT$ LOOPS-HOST$ T$= ;


\ The program nan.f prints the bits of a made NaN, then of a NaN operand on the
\ left, on the right, the left of two, and under a unary word.
: NAN! ( -- )
   SB-RESET
   S\" package N\nprivate\nCAST: BITS ( r -- n )\nCAST: >FLOAT ( n -- r )\n" SB-APPEND
   S\" : ADD ( r r -- r ) f+ ;\n: MUL ( r r -- r ) f* ;\n: ROOT ( r -- r ) fsqrt ;\n" SB-APPEND
   S\" : ODD ( n -- r ) $7FF8000000000000 + >FLOAT ;\npublic\n: MAIN ( -- )\n" SB-APPEND
   S\"    -4.0 ROOT BITS .\n" SB-APPEND
   S\"    1 ODD 1.5 ADD BITS .  1.5 2 ODD ADD BITS .  1 ODD 2 ODD MUL BITS .\n" SB-APPEND
   S\"    1 ODD ROOT BITS . ;\n;package\n: MAIN ( -- ) N:MAIN ;\n" SB-APPEND
   SB$ s" nan.f" SOURCE! ;

: NAN-HOST$ ( -- ptr u8 n )
   SB-RESET
   S\" 9221120237041090560\n9221120237041090561\n9221120237041090562\n" SB-APPEND
   S\" 9221120237041090561\n9221120237041090561\n" SB-APPEND
   SB$ ;

: NAN-CASE ( -- )
   NAN!
   s" a made NaN is the one NaN, and a NaN operand is the answer, the left before the right" T-LABEL
   s" nan.wasm" BUILD 0 T=
   RAN 0 T=
   WASM-HARNESS:OUT$ NAN-HOST$ T$= ;


\ Package M in m.f: map-anon and munmap through words.
: M! ( -- )
   0 TEXT-U !
   S\" package M\nprivate\nCAST: >BYTES ( n -- ptr u8 )\nCAST: >N ( ptr u8 -- n )\n" TEXT+
   S\" : FAIL ( n -- ) >BYTES c@ drop ;\n" TEXT+
   S\" : CHECK ( bool n -- ) swap if drop else FAIL then ;\n: ONE-PAGE ( -- n ) 65536 ;\n" TEXT+
   S\" : MAP ( n -- ptr u8 n ) map-anon ;\n: UNMAP ( ptr u8 n -- n ) munmap ;\n" TEXT+
   S\" : AT ( ptr u8 n -- bool ) swap >N swap = ;\n" TEXT+
   S\" : ALIGNED ( ptr u8 -- bool ) >N 65535 and 0= ;\n" TEXT+
   S\" : FRESH ( n n -- ptr u8 ) {: u:n k:n :} u MAP 0= k CHECK ;\n" TEXT+
   S\" : FREE-RUN ( ptr u8 n n -- ) {: k:n :} UNMAP 0= k CHECK ;\n" TEXT+
   S\" : TWO ( -- ptr u8 )\n   ONE-PAGE 1 FRESH {: a:ptr :}  ONE-PAGE 2 FRESH {: b:ptr :}\n" TEXT+
   S\"    a >N b >N <> 3 CHECK  a ALIGNED 4 CHECK  b ALIGNED 5 CHECK\n" TEXT+
   S\"    a c@ 0= 6 CHECK  b 65535 + c@ 0= 7 CHECK  b ONE-PAGE 8 FREE-RUN  a ;\n" TEXT+
   S\" : REUSE ( ptr u8 -- )\n   {: a:ptr :}  99 a 100 + c!  a ONE-PAGE 9 FREE-RUN\n" TEXT+
   S\"    ONE-PAGE 10 FRESH {: c:ptr :}  c a >N AT 11 CHECK  c 100 + c@ 0= 12 CHECK\n" TEXT+
   S\"    c ONE-PAGE 13 FREE-RUN ;\n" TEXT+
   S\" : JOIN ( -- )\n   ONE-PAGE 14 FRESH {: d:ptr :}  ONE-PAGE 15 FRESH {: e:ptr :}\n" TEXT+
   S\"    ONE-PAGE 16 FRESH {: f:ptr :}\n" TEXT+
   S\"    e d >N ONE-PAGE + AT 17 CHECK  f d >N ONE-PAGE 2 * + AT 18 CHECK\n" TEXT+
   S\"    d ONE-PAGE 19 FREE-RUN  f ONE-PAGE 20 FREE-RUN  e ONE-PAGE 21 FREE-RUN\n" TEXT+
   S\"    ONE-PAGE 3 * 22 FRESH {: g:ptr :}  g d >N AT 23 CHECK  g ONE-PAGE 3 * 24 FREE-RUN\n" TEXT+
   S\"    ONE-PAGE 2 * 25 FRESH {: h:ptr :}  h d >N AT 26 CHECK\n" TEXT+
   S\"    ONE-PAGE 27 FRESH {: t:ptr :}  t d >N ONE-PAGE 2 * + AT 28 CHECK\n" TEXT+
   S\"    h ONE-PAGE 2 * 29 FREE-RUN  t ONE-PAGE 30 FREE-RUN ;\n" TEXT+
   S\" : REFUSALS ( -- )\n   0 MAP -1 = 31 CHECK  0 AT 32 CHECK\n" TEXT+
   S\"    ONE-PAGE 33 FRESH {: p:ptr :}  p 1 + ONE-PAGE UNMAP -1 = 34 CHECK  p ONE-PAGE 35 FREE-RUN\n" TEXT+
   S\"    NULL-PTR ONE-PAGE UNMAP -1 = 36 CHECK\n" TEXT+
   S\"    ONE-PAGE 37 FRESH {: q:ptr :}  q ONE-PAGE 38 FREE-RUN  NULL-PTR ONE-PAGE UNMAP -1 = 39 CHECK\n" TEXT+
   S\"    ONE-PAGE 40 FRESH {: w:ptr :}  w q >N AT 41 CHECK  w ONE-PAGE 42 FREE-RUN\n" TEXT+
   S\"    -1 MAP -1 = 43 CHECK  0 AT 44 CHECK  1 32 lshift 1 + MAP -1 = 45 CHECK  0 AT 46 CHECK\n" TEXT+
   S\"    1 32 lshift MAP -1 = 47 CHECK  0 AT 48 CHECK\n" TEXT+
   S\"    ONE-PAGE 49 FRESH {: z:ptr :}  z 0 UNMAP -1 = 50 CHECK  z ONE-PAGE 51 FREE-RUN ;\n" TEXT+
   S\" public\n: MAIN ( -- ) TWO REUSE JOIN REFUSALS ;\n;package\n" TEXT+
   TEXT TEXT-U @ s" m.f" SOURCE! ;

: TWICE-CASE ( -- )
   S\" : MAIN ( -- ) 65536 map-anon . {: p:ptr :} p 65536 munmap . p 65536 munmap . ;"
   s" twice.f" SOURCE!
   s" a run unmapped twice traps after both earlier answers" T-LABEL
   s" twice.wasm" BUILD 0 T=
   RAN 2 T=
   WASM-HARNESS:OUT$ S\" 0\n0\n" T$= ;


: ERR-HAS ( ptr u8 n -- )
   {: a:ptr u:n :}
   ERR ERR-U @ a u CONTAINS? TTRUE ;

: NO-MODULE ( -- )  MOD MOD-U @ FS:EXISTS? TFALSE ;

: REFUSAL-CASES ( -- )
   s" : MAIN ( -- ) here drop align ;" s" kernel.f" SOURCE!
   s" kernel words with no routine are each refused by name with 74 and no module" T-LABEL
   s" kernel.wasm" BUILD 74 T=
   s" link: a Wasm module has no here" ERR-HAS
   s" link: a Wasm module has no align" ERR-HAS
   s" link: the program reaches kernel words a Wasm module does not provide" ERR-HAS
   NO-MODULE
   s" defer HOOK ( n -- n )  : TWICE ( n -- n ) 2 * ;  : MAIN ( -- ) [: TWICE ;] is HOOK  3 HOOK drop ;"
   s" defer.f" SOURCE!
   s" a defer is refused by name with 74 and no module" T-LABEL
   s" defer.wasm" BUILD 74 T=
   s" link: the defer HOOK needs a trailer, which a Wasm module cannot hold" ERR-HAS
   s" link: a Wasm module cannot hold a defer" ERR-HAS
   NO-MODULE
   s" require src/lib/span.f 8 SPAN-BUFFER: B : MAIN ( -- ) B SPAN:LEN . ;" s" does.f" SOURCE!
   s" a does> clause is refused by name with 74 and no module" T-LABEL
   s" does.wasm" BUILD 74 T=
   s" link: B has a does> clause, which a Wasm module cannot hold" ERR-HAS
   s" link: a Wasm module cannot hold a does> clause" ERR-HAS
   NO-MODULE
   SB-RESET
   S\" package ADV\nprivate\nvariable ACTION\npublic\n" SB-APPEND
   S\" : MAKER ( -- ) create 0 , does> ( -- ) drop ;\n: MAIN ( -- ) ACTION @ drop ;\n" SB-APPEND
   S\" ' MAKER;does ACTION xt!\n;package\n: MAIN ( -- ) ADV:MAIN ;\n" SB-APPEND
   SB$ s" clause-cell.f" SOURCE!
   \ The cell places its definer's whole emission, and every does> definer calls
   \ does-patch, which a Wasm module has no function for.
   s" a cell naming a does> clause is refused with 74 and no module" T-LABEL
   s" clause-cell.wasm" BUILD 74 T=
   s" link: a Wasm module has no does-patch" ERR-HAS
   s" link: the program reaches kernel words a Wasm module does not provide" ERR-HAS
   NO-MODULE
   s" : MAIN ( -- ) 1 ;" s" unchecked.f" SOURCE!
   s" an unchecked MAIN is refused with 74 and no module" T-LABEL
   s" unchecked.wasm" BUILD 74 T=
   s" hook: non-certified definition: main" ERR-HAS
   s" threw 70" ERR-HAS
   NO-MODULE
   s" require src/lib/span.f : MAIN ( -- ) 8 SPAN-BUFFER: ;" s" reached-definer.f" SOURCE!
   s" a definer MAIN reaches is refused with 74 and no module" T-LABEL
   s" reached-definer.wasm" BUILD 74 T=
   s" link: a stripped program reaches a word it cannot run" ERR-HAS
   NO-MODULE
   s" : MAIN ( n -- ) drop ;" s" cells.f" SOURCE!
   s" a MAIN that takes a cell is refused with 74 and no module" T-LABEL
   s" cells.wasm" BUILD 74 T=
   s" APP-ENTRY:XT!" ERR-HAS
   s" expected: [ -- ] actual: [ n -- ]" ERR-HAS
   NO-MODULE
   s" require src/lib/fs.f" s" native-only.f" SOURCE!
   s" a file that does not build for Wasm stops its load with 74 and no module" T-LABEL
   s" native-only.wasm" BUILD 74 T=
   s" E-TARGET habu: " ERR-HAS
   s" src/lib/ffi-abi.f:" ERR-HAS
   s" : the file does not build for wasm" ERR-HAS
   NO-MODULE
   s" : MAIN ( -- ) ;" s" unwritten.f" SOURCE!
   s" an output path in a missing directory is refused with 64 and no module" T-LABEL
   s" missing/unwritten.wasm" BUILD 64 T=
   s" hb-build: no such output directory" ERR-HAS
   NO-MODULE
   s" an engine build for Wasm is refused with 64 and no module" T-LABEL
   s" engine.wasm" BUILD-ENGINE 64 T=
   s" hb-build: a Wasm target builds a program, not an engine or a REPL" ERR-HAS
   NO-MODULE
   s" : MAIN ( -- ) ;" s" repl.f" SOURCE!
   s" a REPL build for Wasm is refused with 64 and no module" T-LABEL
   s" repl.wasm" BUILD-REPL 64 T=
   s" hb-build: a Wasm target builds a program, not an engine or a REPL" ERR-HAS
   NO-MODULE ;


\ Package X in execute.f: execute of a named word's xt, of a quotation and of a
\ quotation kept in a variable.
: X! ( -- )
   SB-RESET
   S\" package X\nprivate\nCAST: >BYTES ( n -- ptr u8 )\n" SB-APPEND
   S\" TYPED-VARIABLE HOOK [ n -- n ]\n: DOUBLE ( n -- n ) 2 * ;\npublic\n" SB-APPEND
   S\" : MAIN ( -- )\n   3 ['] DOUBLE execute 6 <> if 1 >BYTES c@ drop then\n" SB-APPEND
   S\"    4 [: 1 + ;] execute 5 <> if 2 >BYTES c@ drop then\n" SB-APPEND
   S\"    [: 10 * ;] HOOK !  7 HOOK @ execute 70 <> if 3 >BYTES c@ drop then ;\n" SB-APPEND
   S\" ;package\n" SB-APPEND
   SB$ s" execute.f" SOURCE! ;

: THROW-CASES ( -- )
   s" : MAIN ( -- ) 9 throw ;" s" throw.f" SOURCE!
   s" an uncaught `9 throw` ends run with status 1 and throw-code 9" T-LABEL
   s" throw.wasm" BUILD 0 T=
   RAN 1 T=
   WASM-HARNESS:THROW-CODE 9 T=
   s" : MAIN ( -- ) 0 throw ;" s" throw-zero.f" SOURCE!
   s" `0 throw` ends run with status 1 and throw-code ENGINE-ERROR:THROW-ZERO" T-LABEL
   s" throw-zero.wasm" BUILD 0 T=
   RAN 1 T=
   WASM-HARNESS:THROW-CODE ENGINE-ERROR:THROW-ZERO T= ;

\ run.mjs shows OUT after a trap but not the throw code, which it prints only
\ for status 1.
: DIE-CASE ( -- )
   S\" : MAIN ( -- ) s\" boom\" 3 die ;" s" die.f" SOURCE!
   s" die writes its message to OUT and traps" T-LABEL
   s" die.wasm" BUILD 0 T=
   RAN 2 T=
   WASM-HARNESS:OUT$ s" boom" T$= ;


\ Package C in catch.f: catch, finally and the depths a caught throw restores,
\ which LEAK and LEAK-R overflow without.
: CATCH! ( -- )
   SB-RESET
   S\" package C\nprivate\nCAST: >BYTES ( n -- ptr u8 )\nvariable DONE\n" SB-APPEND
   S\" : FAIL ( n -- ) >BYTES c@ drop ;\n" SB-APPEND
   S\" : WANT ( n n n -- ) {: a:n b:n k:n :} a b <> if k FAIL then ;\n" SB-APPEND
   S\" : DEEP ( -- ) 11 throw ;\n: MID ( -- ) DEEP ;\n: CLEAN ( -- ) 1 DONE ! ;\n" SB-APPEND
   S\" : FIVE ( -- ) 4 [: 1 + ;] catch 0= if 5 3 WANT else drop 2 FAIL then ;\n" SB-APPEND
   S\" : LEAK ( -- ) 100000 0 do [: 10 0 do 1 throw loop ;] catch drop loop ;\n" SB-APPEND
   S\" : LEAK-R ( -- ) 100000 0 do [: 5 >r 1 throw ;] catch drop loop ;\n" SB-APPEND
   S\" : NEST ( -- n ) 0 3 0 do 4 0 do 1 + loop loop ;\n: PARK ( n -- n ) >r 1 r> + ;\n" SB-APPEND
   \ MAIN's lines go unindented to keep the text within SB-CAP.
   S\" public\n: MAIN ( -- )\n" SB-APPEND
   S\" [: 7 throw ;] catch 7 1 WANT  FIVE  [: 0 throw ;] catch 109 4 WANT\n" SB-APPEND
   S\" 1 2 [: MID ;] catch 11 5 WANT  2 6 WANT  1 7 WANT\n" SB-APPEND
   S\" LEAK  LEAK-R  NEST 12 8 WANT  6 PARK 7 9 WANT\n" SB-APPEND
   S\" [: 42 ;] [: CLEAN ;] finally 42 10 WANT  DONE @ 1 11 WANT\n" SB-APPEND
   S\" 0 DONE !  [: [: 7 throw ;] [: CLEAN ;] finally ;] catch 7 12 WANT\n" SB-APPEND
   S\" DONE @ 1 13 WANT  [: [: 7 throw ;] [: 8 throw ;] finally ;] catch 8 14 WANT\n" SB-APPEND
   S\" [: [: 7 throw ;] [: [: 9 throw ;] catch drop ;] finally ;] catch 7 15 WANT ;\n" SB-APPEND
   S\" ;package\n" SB-APPEND
   SB$ s" catch.f" SOURCE! ;


\ The frames are wide: a recursion of one cell a level exhausts bun's call
\ stack before a 64 KiB region.
: BOUNDS-CASES ( -- )
   SB-RESET
   S\" : P8 ( n -- n n n n n n n n n ) dup dup dup dup dup dup dup dup ;\n" SB-APPEND
   S\" : D8 ( n n n n n n n n -- ) 2drop 2drop 2drop 2drop ;\n" SB-APPEND
   S\" : DEEP ( n -- ) P8 P8 P8 P8 recurse D8 D8 D8 D8 ;\n: MAIN ( -- ) 0 DEEP ;\n" SB-APPEND
   SB$ s" sdata.f" SOURCE!
   s" a data stack past its region faults with STACK-BOUNDS above its end and traps" T-LABEL
   s" sdata.wasm" BUILD 0 T=
   RAN 2 T=
   WASM-HARNESS:FAULT-KIND ENGINE-ERROR:STACK-BOUNDS T=
   WASM-HARNESS:FAULT-ADDR WPROF:STACK-END > TTRUE
   SB-RESET
   S\" : DEEP ( n -- ) dup >r dup >r dup >r dup >r dup >r dup >r dup >r dup >r " SB-APPEND
   S\" dup >r dup >r dup >r dup >r dup >r dup >r dup >r dup >r recurse " SB-APPEND
   S\" r> drop r> drop r> drop r> drop r> drop r> drop r> drop r> drop " SB-APPEND
   S\" r> drop r> drop r> drop r> drop r> drop r> drop r> drop r> drop ;\n" SB-APPEND
   S\" : MAIN ( -- ) 0 DEEP ;\n" SB-APPEND
   SB$ s" sret.f" SOURCE!
   s" a return stack past its region faults with STACK-BOUNDS at its end and traps" T-LABEL
   s" sret.wasm" BUILD 0 T=
   RAN 2 T=
   WASM-HARNESS:FAULT-KIND ENGINE-ERROR:STACK-BOUNDS T=
   WASM-HARNESS:FAULT-ADDR WPROF:LOOP-BASE T=
   S\" : DEEP ( -- ) 1 0 do recurse loop ;\n: MAIN ( -- ) DEEP ;\n" s" sloop.f" SOURCE!
   s" a loop stack past its region faults with STACK-BOUNDS at its end and traps" T-LABEL
   s" sloop.wasm" BUILD 0 T=
   RAN 2 T=
   WASM-HARNESS:FAULT-KIND ENGINE-ERROR:STACK-BOUNDS T=
   WASM-HARNESS:FAULT-ADDR WPROF:DATA-BASE T= ;


\ Package G in grow.f: a cell row reserved, then reserved larger.
: GROW! ( -- )
   SB-RESET
   S\" package G\nprivate\nCAST: >BYTES ( n -- ptr u8 )\nDYNAMIC-BUFFER ROW n\n" SB-APPEND
   S\" : FAIL ( n -- ) >BYTES c@ drop ;\n" SB-APPEND
   S\" : WANT ( n n n -- ) {: a:n b:n k:n :} a b <> if k FAIL then ;\n" SB-APPEND
   S\" : AT ( n -- n ) ROW NULL-PTR - ;\npublic\n: MAIN ( -- )\n" SB-APPEND
   S\"    4 ROW-RESERVE  11 0 ROW !  22 3 ROW !  0 AT {: old:n :}\n" SB-APPEND
   S\"    100000 ROW-RESERVE  0 ROW @ 11 1 WANT  3 ROW @ 22 2 WANT  0 AT old = bool>n 0 3 WANT\n" SB-APPEND
   S\"    33 99999 ROW !  99999 ROW @ 33 4 WANT\n" SB-APPEND
   S\"    65536 map-anon 0 5 WANT  NULL-PTR - old 6 WANT  ROW-RELEASE ;\n" SB-APPEND
   S\" ;package\n" SB-APPEND
   SB$ s" grow.f" SOURCE! ;


: PASS-CASE ( -- )
   120 s" k.f" K!  D!  M!  X!  CATCH!  GROW!
   0 TEXT-U !
   s" k.f" REQUIRE+  s" d.f" REQUIRE+  s" m.f" REQUIRE+
   s" execute.f" REQUIRE+  s" catch.f" REQUIRE+  s" grow.f" REQUIRE+
   S\" : MAIN ( -- ) 75 emit K:MAIN 68 emit D:MAIN 77 emit M:MAIN " TEXT+
   S\" 88 emit X:MAIN 67 emit C:MAIN 71 emit G:MAIN ;\n" TEXT+
   TEXT TEXT-U @ s" pass.f" SOURCE!
   s" six programs in one module each run to their end: recursion, a loop, variables and a quotation; division, max, invert and rshift, atomics and views; map-anon and munmap; execute; catch and finally; a DYNAMIC-BUFFER reserved larger" T-LABEL
   s" pass.wasm" BUILD 0 T=
   RAN 0 T=
   WASM-HARNESS:FAULT-ADDR 0 T=
   WASM-HARNESS:OUT$ s" KDMXCG" T$= ;


: SEGMENTS-CASE ( -- )
   s" create T  : LAY ( -- ) 100001 0 do 1 , 0 , 0 , loop ; LAY  : MAIN ( -- ) T @ . ;"
   s" segments.f" SOURCE!
   s" data of 100001 separated cells builds into a module of at most 100000 segments, which bun loads" T-LABEL
   s" segments.wasm" BUILD 0 T=
   RAN 0 T=
   WASM-HARNESS:OUT$ S\" 1\n" T$= ;


: UNUSED-CASE ( -- )
   s" : DEAD ( -- ) here drop ; : MAIN ( -- ) 7 . ;" s" unused.f" SOURCE!
   s" an unreachable definition with an unsupported call is omitted" T-LABEL
   s" unused.wasm" BUILD 0 T=
   RAN 0 T=
   WASM-HARNESS:OUT$ S\" 7\n" T$= ;

: FATAL-CASE ( -- )
   S\" : FATAL ( -- ) s\" fatal\" 77 die ; : MAIN ( -- ) ['] FATAL catch . ;"
   s" fatal.f" SOURCE!
   s" die prints its message and traps through catch, with no continuation" T-LABEL
   s" fatal.wasm" BUILD 0 T=
   RAN 2 T=
   WASM-HARNESS:OUT$ s" fatal" T$= ;

\ `bin/hb --load` of the source; answers its exit status.
: NATIVE ( -- n )
   PROC-ARGV-RESET  s" --load" ARG+  SRC+  DRIVER ;

: NEGATIVE-BYTE-CASE ( -- )
   S\" 8 BUFFER: B : BAD ( -- ) B B -1 BYTE-COPY ; : MAIN ( -- ) ['] BAD catch . ; MAIN"
   s" negative-native.f" SOURCE!
   s" native BYTE-COPY dies through catch on a negative length" T-LABEL
   NATIVE 76 T=
   ERR ERR-U @ RTRIM s" byte: negative length" T$=
   S\" 8 BUFFER: B : BAD ( -- ) B B -1 BYTE-COPY ; : MAIN ( -- ) ['] BAD catch . ;"
   s" negative-wasm.f" SOURCE!
   s" Wasm BYTE-COPY prints the same diagnostic and traps through catch" T-LABEL
   s" negative.wasm" BUILD 0 T=
   RAN 2 T=
   WASM-HARNESS:OUT$ s" byte: negative length" T$= ;


\ ACTION-XT gives execute's own xt, which has no stored effect, as this
\ fixture's type; its table slot is execute's kernel routine.
: EXECUTE-CELL! ( -- )
   SB-RESET
   S\" package PROBE\nprivate\n: CODE-CELL: ( -- ) create 0 , does> ( -- ptr [ [ -- ] -- ] ) ;\n" SB-APPEND
   S\" CODE-CELL: ACTION\nTRUSTED: ACTION-XT ( -- [ [ -- ] -- ] ) ['] execute ;\npublic\n: TARGET ( -- ) 31 . ;\n" SB-APPEND
   S\" : MAIN ( -- ) ['] TARGET ACTION @ execute ;\nACTION-XT ACTION xt!\n;package\n" SB-APPEND
   S\" : MAIN ( -- ) PROBE:MAIN ;\n" SB-APPEND
   SB$ s" execute-cell.f" SOURCE! ;

: CATCH-CELL! ( -- )
   SB-RESET
   S\" package PROBE\nprivate\n: CODE-CELL: ( -- ) create 0 , does> ( -- ptr [ [ -- ] -- n ] ) ;\n" SB-APPEND
   S\" CODE-CELL: ACTION\nTRUSTED: ACTION-XT ( -- [ [ -- ] -- n ] ) ['] catch ;\npublic\n: TARGET ( -- ) -37 throw ;\n" SB-APPEND
   S\" : MAIN ( -- ) ['] TARGET ACTION @ execute . ;\nACTION-XT ACTION xt!\n;package\n" SB-APPEND
   S\" : MAIN ( -- ) PROBE:MAIN ;\n" SB-APPEND
   SB$ s" catch-cell.f" SOURCE! ;

: WORD-CELL! ( -- )
   SB-RESET
   S\" package PROBE\nprivate\n: CODE-CELL: ( -- ) create 0 , does> ( -- ptr [ -- ] ) ;\n" SB-APPEND
   S\" CODE-CELL: ACTION\npublic\n: TARGET ( -- ) 31 . ;\n" SB-APPEND
   S\" : MAIN ( -- ) ACTION @ execute ;\n' TARGET ACTION xt!\n;package\n" SB-APPEND
   S\" : MAIN ( -- ) PROBE:MAIN ;\n" SB-APPEND
   SB$ s" word-cell.f" SOURCE! ;

: CELL-CASES ( -- )
   EXECUTE-CELL!
   s" a word run through a code cell holding execute's xt prints 31, as natively" T-LABEL
   s" execute-cell.wasm" BUILD 0 T=
   RAN 0 T=
   WASM-HARNESS:OUT$ S\" 31\n" T$=
   WORD-CELL!
   s" a word run through a code cell holding its own xt prints 31, as natively" T-LABEL
   s" word-cell.wasm" BUILD 0 T=
   RAN 0 T=
   WASM-HARNESS:OUT$ S\" 31\n" T$=
   CATCH-CELL!
   s" catch through a code cell holding catch's xt prints the code -37, as natively" T-LABEL
   s" catch-cell.wasm" BUILD 0 T=
   RAN 0 T=
   WASM-HARNESS:OUT$ S\" -37\n" T$= ;


: FETCH! ( -- )
   SB-RESET
   S\" package PROBE\npublic\nENUM shade dark light grey ;ENUM\nTYPED-VARIABLE V shade\n" SB-APPEND
   S\" private\nCAST: >CELL ( ptr shade -- ptr n )\npublic\n: TONE@ ( ptr shade -- shade ) @ ;\n" SB-APPEND
   S\" : CODE ( -- n ) V TONE@ MATCH shade dark OF 0 ENDOF light OF 1 ENDOF grey OF 2 ENDOF ;MATCH ;\n" SB-APPEND
   S\" : MAIN ( -- ) construct shade grey V ! CODE .  7 V >CELL ! CODE . ;\n;package\n" SB-APPEND
   S\" : MAIN ( -- ) PROBE:MAIN ;\n" SB-APPEND
   SB$ s" fetch.f" SOURCE! ;

: FETCH-CASES ( -- )
   FETCH!
   s" an ENUM fetched through a pointer passes its tag check, then a stored 7 fails it" T-LABEL
   s" fetch.wasm" BUILD 0 T=
   RAN 2 T=
   WASM-HARNESS:OUT$ S\" 2\nE-BAD-TAG habu: layout tag 7 outside 0..2" T$= ;


65536 constant PAGE-BYTES
5 constant SEC-MEMORY
variable POS                         \ the byte offset in MODULE read next
DYNAMIC-BUFFER MODULE u8

: ACCESS! ( n bool ptr u8 n -- )
   {: a:n cell:bool name:ptr nu:n :}
   SB-RESET
   S\" package WASM-MEMORY\nprivate\nCAST: >CELL ( n -- ptr n )\n" SB-APPEND
   S\" CAST: >BYTE ( n -- ptr u8 )\npublic\n: ACCESS ( -- ) " SB-APPEND
   a FMT:SB-U
   cell if s"  >CELL @" else s"  >BYTE c@" then SB-APPEND
   S\"  drop ;\n;package\n: MAIN ( -- ) WASM-MEMORY:ACCESS ;\n" SB-APPEND
   SB$ name nu SOURCE! ;

: ACCESS ( ptr u8 n -- n )
   BUILD 0 T=
   RAN ;

: U32> ( ptr u8 n -- n )
   {: m:ptr u:n :}
   m POS @ +  u POS @ -  WLEB:U32@ POS +! ;

\ The module's memory size in bytes: past the 8-byte header, section by section
\ to the memory section, then its count and limits flag to the minimum pages.
: MEMORY-END ( -- n )
   MOD MOD-U @ FS:FILE-SIZE {: u:n :}
   u MODULE-RESERVE
   MOD MOD-U @ 0 MODULE u FS:READ-ALL u T=
   0 MODULE {: m:ptr :}
   8 POS !
   begin m POS @ + c@ SEC-MEMORY <> while
      1 POS +!  m u U32> POS +!
   repeat
   1 POS +!  m u U32> drop  m u U32> drop  1 POS +!
   m u U32> PAGE-BYTES * ;

\ Memory ends at the memory section's minimum pages, not the image; the four
\ sources differ only in the access, so they share that end.
: MEMORY-CASES ( -- )
   s" a cell at an upper-bit pointer, $100031000, fails its check and throws" T-LABEL
   $100031000 true s" upper.f" ACCESS!
   s" upper.wasm" ACCESS 1 T=
   WASM-HARNESS:THROW-CODE ENGINE-ERROR:MEMORY-FAULT T=
   MEMORY-END {: end:n :}
   s" a byte in the null region, $FFFF, fails its check and throws" T-LABEL
   $FFFF false s" null.f" ACCESS!
   s" null.wasm" ACCESS 1 T=
   WASM-HARNESS:THROW-CODE ENGINE-ERROR:MEMORY-FAULT T=
   s" the last byte of the module's memory reads" T-LABEL
   end 1- false s" last.f" ACCESS!
   s" last.wasm" ACCESS 0 T=
   s" a cell whose last byte is one past the memory's end traps in the engine" T-LABEL
   end 7 - true s" past.f" ACCESS!
   s" past.wasm" ACCESS 2 T= ;

public

: RUN ( -- )
   T-RESET
   SETUP
   EMIT-CASE
   FULL-CASE
   PASS-CASE
   PROGRAM-CASES
   DIV0-CASE
   FLOAT-CASE
   LOOP-CASE
   NAN-CASE
   TWICE-CASE
   REFUSAL-CASES
   THROW-CASES
   DIE-CASE
   BOUNDS-CASES
   SEGMENTS-CASE
   UNUSED-CASE
   FATAL-CASE
   NEGATIVE-BYTE-CASE
   CELL-CASES
   FETCH-CASES
   MEMORY-CASES
   T-REPORT ;

;package

WASM-HB-BUILD-TEST:RUN
