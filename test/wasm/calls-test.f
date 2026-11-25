\ calls-test.f - test/wasm/calls.f's CHECKS run in this process, then as the
\ MAIN of a module `hb-build --target wasm` builds, validated by wasm-tools and
\ run by bun. A trap or an uncaught throw ends a run, so each such entry is the
\ MAIN of a module of its own. test/wasm/device.f runs it. The sources and
\ modules stay in the printed directory.

require src/lib/string.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/test.f
require test/wasm/harness.f
require test/wasm/calls.f

package WASM-CALLS-TEST
private

2 constant TRAPPED                     \ test/wasm/run.mjs's status for a trap
PATH-CAP BUFFER: DIR
variable DIR-U
PATH-CAP BUFFER: MOD
variable MOD-U
PATH-CAP BUFFER: SRC
variable SRC-U

: SETUP ( -- )
   s" wasm-calls" FS:MKTEMP-DIR {: d:str :}
   d STR:DATA DIR d STR:LEN BYTE-COPY  d STR:LEN DIR-U !
   s" wasm calls: sources and modules in " type  DIR d STR:LEN >str type cr ;

: RAN ( str str -- n )
   {: entry:str name:str :}
   SB-RESET name SB-APPEND s" .wasm" SB-APPEND
   DIR DIR-U @ >str SB$ MOD FS:JOIN-PATH MOD-U !
   SB-RESET name SB-APPEND s" .f" SB-APPEND
   DIR DIR-U @ >str SB$ SRC FS:JOIN-PATH SRC-U !
   SB-RESET S\" require test/wasm/calls.f\n: MAIN ( -- ) WCALLS:" SB-APPEND
   entry SB-APPEND  S\"  ;\n" SB-APPEND
   SRC SRC-U @ >str SB$ FS:WRITE-ALL
   SRC SRC-U @ >str MOD MOD-U @ >str WASM-HARNESS:BUILD 0 T=
   MOD MOD-U @ >str WASM-HARNESS:VALID? TTRUE
   MOD MOD-U @ >str WASM-HARNESS:RUN ;

: TRAPS ( str str -- )  RAN TRAPPED T= ;

false [if] \ off until dot habu-add-the-checks-a49d054d
: FAULTED-CASE ( -- )
   s" a memory fault inside a quotation is caught and thrown again" T-LABEL
   s" FAULTED" s" faulted" RAN 1 T=
   WASM-HARNESS:THROW-CODE 110 T= ;
[then]

public

: RUN ( -- )
   T-RESET
   SETUP
   s" every entry throws its native code in this process" T-LABEL
   [: WCALLS:CHECKS ;] catch 0 T=
   s" every entry throws its native code in a module; a throw code numbers the first that does not" T-LABEL
   s" CHECKS" s" checks" RAN 0 T=
   WASM-HARNESS:THROW-CODE 0 T=
   s" execute of xt 0 traps" T-LABEL
   s" ZERO" s" zero" TRAPS
   s" catch of xt 0 traps" T-LABEL
   s" ZERO-CAUGHT" s" zero-caught" TRAPS
   s" finally of cleanup xt 0 traps before its body runs" T-LABEL
   s" ZERO-CLEANUP" s" zero-cleanup" TRAPS
   WASM-HARNESS:OUT$ s" " T$=
   s" a record its throw carried survives finally's cleanup that throws and catches" T-LABEL
   s" FINALLY-CARRIED" s" finally-carried" RAN 1 T=
   WASM-HARNESS:THROW-CODE 85 T=
   WASM-HARNESS:ERR$ s" error[E-PROBE]: probe text" STR:COUNT-SUB 1 T=
   WASM-HARNESS:ERR$ s" error[" STR:COUNT-SUB 1 T=
   s" a caught code thrown on after a cleanup that throws and catches keeps its record" T-LABEL
   s" RETHROW-CARRIED" s" rethrow-carried" RAN 1 T=
   WASM-HARNESS:THROW-CODE 85 T=
   WASM-HARNESS:ERR$ s" error[E-PROBE]: probe text" STR:COUNT-SUB 1 T=
   WASM-HARNESS:ERR$ s" error[" STR:COUNT-SUB 1 T=
   s" execute of an xt whose upper 32 bits are set traps, though its low half is MARK's slot" T-LABEL
   s" HIGH" s" high" TRAPS
   s" execute of a cell whose upper half is not exactly a record's traps" T-LABEL
   s" HIGH-TAGGED" s" high-tagged" TRAPS
   T-REPORT ;

;package

WASM-CALLS-TEST:RUN
