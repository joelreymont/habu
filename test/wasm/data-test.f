\ The checked build's DATA boundary, exercised under a foreign Wasm peer.
\ Run from the repository root: bin/hb --load test/wasm/data-test.f
require src/lib/string.f
require src/lib/fmt.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/test.f
require src/arch/wasm/profile.f
require test/wasm/harness.f

package WASM-DATA-TEST
private

60000 constant DEADLINE-MS
4096 constant CAP
CAP BUFFER: OUT
CAP BUFFER: ERR
FS-PATH-CAP BUFFER: DIR
FS-PATH-CAP BUFFER: MOD
FS-PATH-CAP BUFFER: EXE
variable MOD-U

: ARG+ ( ptr u8 n -- ) >LEN PROC-ARGV+ ;

: NUM-ARG ( n -- )
   SB-RESET FMT:SB-U SB$ ARG+ ;

: SPAWN ( ptr u8 n -- n )
   {: exe:ptr eu:n :}
   exe eu >LEN EXE RESOLVE-EXECUTABLE {: xu:len :}
   EXE xu OUT CAP >LEN ERR CAP >LEN DEADLINE-MS >MS
   RUN-ARGV-CAPTURE-OUTCOME PROC-OUTCOME>RC RC>N
   {: ou:len er:len rc:n :}
   rc 0<> if ERR er LEN>N type cr then
   rc ;

: SETUP ( -- )
   s" wasm-data" HB-TMP-MKDIR {: dir:ptr u:n :}
   dir DIR u BYTE-COPY
   DIR u s" data-region.wasm" MOD JOIN-PATH MOD-U ! ;

: BUILD ( -- n )
   PROC-ARGV-RESET
   s" --load" ARG+ s" tools/wasm-build.f" ARG+ s" --" ARG+
   s" test/wasm/data-region.f" ARG+ s" WASM-DATA-REGION:MAIN" ARG+
   MOD MOD-U @ ARG+
   s" bin/hb" SPAWN ;

: PEER ( -- n )
   PROC-ARGV-RESET
   s" test/wasm/data-peer.mjs" ARG+ MOD MOD-U @ ARG+
   WPROF:DATA-BASE NUM-ARG
   WPROF:DATA-BYTES NUM-ARG
   WPROF:IMAGE-BASE NUM-ARG
   WPROF:STACK-BASE NUM-ARG
   WPROF:STACK-BYTES NUM-ARG
   STACK-ABI:BASE-CELL NUM-ARG
   STACK-ABI:CAP-CELL NUM-ARG
   s" bun" SPAWN ;

public

: RUN ( -- )
   T-RESET
   SETUP
   s" tools/wasm-build compiles the checked DATA and owner program" T-LABEL
   BUILD 0 T=
   MOD MOD-U @ WASM-HARNESS:VALID? TTRUE
   s" the foreign peer proves the disjoint DATA, static owner and dynamic page" T-LABEL
   PEER 0 T=
   T-REPORT ;

;package

WASM-DATA-TEST:RUN
