\ hb-build.f - `hb-build --target wasm` run by its command line, each module it
\ writes validated by wasm-tools and run by bun (test/wasm/harness.f); each case
\ starts a build process, since the build's command line and exit status are
\ under test. test/wasm/device.f runs it, as does `bin/hb --load
\ test/wasm/hb-build.f` from the tree's root. The sources and modules stay in the
\ printed directory.

require src/lib/string.f
require src/lib/fmt.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/test.f
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

\ The source's module at the name, by `hb-build --target wasm`; answers its exit status.
: BUILD ( ptr u8 n -- n )
   {: name:ptr nu:n :}
   DIR DIR-U @ name nu MOD FS:JOIN-PATH MOD-U !
   PROC-ARGV-RESET
   s" --load" ARG+  s" tools/hb-build.f" ARG+  s" --" ARG+
   s" --target" ARG+  s" wasm" ARG+  SRC SRC-U @ ARG+  s" -o" ARG+  MOD MOD-U @ ARG+
   DRIVER ;

: RAN ( -- n )
   MOD MOD-U @ WASM-HARNESS:VALID? TTRUE
   MOD MOD-U @ WASM-HARNESS:RUN ;


: EMIT-CASE ( -- )
   s" : MAIN ( -- ) 53 emit ;" s" emit.f" SOURCE!
   s" `53 emit` builds into a module that validates, runs to status 0 and prints 5" T-LABEL
   s" emit.wasm" BUILD 0 T=
   RAN 0 T=
   WASM-HARNESS:OUT$ s" 5" T$= ;


\ The program k.f, its first test comparing 5 FACT with want; a failed test
\ reads below WPROF:CTX-BASE, which traps.
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
   S\"    [: 1 ;] drop ;\n;package\n: MAIN ( -- ) K:MAIN ;\n" SB-APPEND
   SB$ name nu SOURCE! ;

: PROGRAM-CASES ( -- )
   120 s" k.f" K!
   s" a program of recursion, a loop, variables, stores and a quotation runs to status 0" T-LABEL
   s" k.wasm" BUILD 0 T=
   RAN 0 T=
   121 s" k-fail.f" K!
   s" the same program with a failing first test runs to its trap" T-LABEL
   s" k-fail.wasm" BUILD 0 T=
   RAN 2 T=
   S\" package K private CAST: >BYTES ( n -- ptr u8 ) public : MAIN ( -- ) 65 8 >BYTES c! ; ;package : MAIN ( -- ) K:MAIN ;"
   s" low.f" SOURCE!
   s" a c! below WPROF:CTX-BASE faults and traps" T-LABEL
   s" low.wasm" BUILD 0 T=
   RAN 2 T= ;


\ The program d.f: division, max, atomics and views through words whose operands
\ are not literals at the call; a failed test reads below WPROF:CTX-BASE.
: D! ( -- )
   SB-RESET
   S\" package D\nprivate\nCAST: >BYTES ( n -- ptr u8 )\nvariable CELL0\nvariable SPOT\n" SB-APPEND
   S\" : DIV ( n n -- n ) / ;\n: REM ( n n -- n ) mod ;\n: BIG ( n n -- n ) max ;\n" SB-APPEND
   S\" : MIN-N ( -- n ) 1 63 lshift ;\n: FAIL ( n -- ) >BYTES c@ drop ;\npublic\n" SB-APPEND
   S\" : MAIN ( -- )\n   17 5 DIV 3 <> if 1 FAIL then  17 5 REM 2 <> if 2 FAIL then\n" SB-APPEND
   S\"    -17 5 DIV -3 <> if 3 FAIL then  -17 5 REM -2 <> if 4 FAIL then\n" SB-APPEND
   S\"    MIN-N -1 DIV MIN-N <> if 5 FAIL then  MIN-N -1 REM 0 <> if 6 FAIL then\n" SB-APPEND
   S\"    -3 7 BIG 7 <> if 7 FAIL then  7 -3 BIG 7 <> if 8 FAIL then\n" SB-APPEND
   S\"    5 CELL0 atomic!  CELL0 @ 5 <> if 9 FAIL then\n" SB-APPEND
   S\"    5 9 CELL0 atomic-cas 5 <> if 10 FAIL then  CELL0 @ 9 <> if 11 FAIL then\n" SB-APPEND
   S\"    5 1 CELL0 atomic-cas 9 <> if 12 FAIL then  CELL0 @ 9 <> if 13 FAIL then\n" SB-APPEND
   S\"    77 SPOT !  SPOT byte-view c@ 77 <> if 14 FAIL then\n" SB-APPEND
   S\"    SPOT cell-view @ 77 <> if 15 FAIL then ;\n;package\n: MAIN ( -- ) D:MAIN ;\n" SB-APPEND
   SB$ s" d.f" SOURCE! ;

: ARITH-CASES ( -- )
   D!
   s" division, max, atomics and views build into a module that runs to status 0" T-LABEL
   s" d.wasm" BUILD 0 T=
   RAN 0 T=
   s" : DIV ( n n -- n ) / ;  : MAIN ( -- ) 1 0 DIV drop ;" s" div0.f" SOURCE!
   s" a zero divisor runs to status 1 and throws E-DIV-ZERO" T-LABEL
   s" div0.wasm" BUILD 0 T=
   RAN 1 T=
   WASM-HARNESS:THROW-CODE E-DIV-ZERO T= ;


: REFUSAL-CASE ( -- )
   s" : MAIN ( -- ) depth drop ;" s" depth.f" SOURCE!
   s" a kernel word with no routine is refused by name with 74 and no module" T-LABEL
   s" depth.wasm" BUILD 74 T=
   ERR ERR-U @ s" link: a Wasm module has no depth" CONTAINS? TTRUE
   MOD MOD-U @ FS:EXISTS? TFALSE ;

public

: RUN ( -- )
   T-RESET
   SETUP
   EMIT-CASE
   PROGRAM-CASES
   ARITH-CASES
   REFUSAL-CASE
   T-REPORT ;

;package

WASM-HB-BUILD-TEST:RUN
