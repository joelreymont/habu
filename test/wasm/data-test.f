\ The checked build's DATA boundary under a foreign Wasm host. The peer,
\ test/wasm/data-peer.mjs, only dumps the module's memory before and after its
\ run and writes OUT; this file makes every check from those bytes.
\ Run from the repository root: bin/hb --load test/wasm/data-test.f
require src/lib/string.f
require src/lib/fmt.f
require src/lib/le.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/test.f
require src/arch/wasm/profile.f
require src/habu/stack-abi.f
require test/wasm/harness.f

package WASM-DATA-TEST
private

60000 constant DEADLINE-MS
4096 constant CAP
CAP BUFFER: OUT
CAP BUFFER: ERR
FS-PATH-CAP BUFFER: DIR
FS-PATH-CAP BUFFER: MOD
FS-PATH-CAP BUFFER: BEFORE-PATH
FS-PATH-CAP BUFFER: AFTER-PATH
FS-PATH-CAP BUFFER: EXE
variable OUT-U
variable MOD-U
variable BEFORE-PATH-U
variable AFTER-PATH-U

\ The byte the peer stores in the page it grows before the run, and where.
7 constant HOST-OFFSET
173 constant HOST-BYTE

\ test/wasm/data-region.f's dynamic buffer: its size, and the byte the program
\ stores at its last index.
204800 constant DYN-BYTES
137 constant DYN-BYTE

\ The module's memory as the peer dumped it before and after the run.
DYNAMIC-BUFFER BEFORE u8
DYNAMIC-BUFFER AFTER u8

: ARG+ ( ptr u8 n -- ) >LEN PROC-ARGV+ ;

: NUM-ARG ( n -- )
   SB-RESET FMT:SB-U SB$ ARG+ ;

: SPAWN ( ptr u8 n -- n )
   {: exe:ptr eu:n :}
   exe eu >LEN EXE RESOLVE-EXECUTABLE {: xu:len :}
   EXE xu OUT CAP >LEN ERR CAP >LEN DEADLINE-MS >MS
   RUN-ARGV-CAPTURE-OUTCOME PROC-OUTCOME>RC RC>N
   {: ou:len er:len rc:n :}
   ou LEN>N OUT-U !
   rc 0<> if ERR er LEN>N type cr then
   rc ;

: OUT$ ( -- ptr u8 n ) OUT OUT-U @ ;

: SETUP ( -- )
   s" wasm-data" TMPDIR-MKDIR {: dir:ptr u:n :}
   dir DIR u BYTE-COPY
   DIR u s" data-region.wasm" MOD JOIN-PATH MOD-U !
   DIR u s" before.bin" BEFORE-PATH JOIN-PATH BEFORE-PATH-U !
   DIR u s" after.bin" AFTER-PATH JOIN-PATH AFTER-PATH-U ! ;

: BUILD ( -- n )
   PROC-ARGV-RESET
   s" --load" ARG+ s" tools/wasm-build.f" ARG+ s" --" ARG+
   s" test/wasm/data-region.f" ARG+ s" WASM-DATA-REGION:MAIN" ARG+
   MOD MOD-U @ ARG+
   s" bin/hb" SPAWN ;

: PEER ( -- n )
   PROC-ARGV-RESET
   s" test/wasm/data-peer.mjs" ARG+ MOD MOD-U @ ARG+
   BEFORE-PATH BEFORE-PATH-U @ ARG+ AFTER-PATH AFTER-PATH-U @ ARG+
   HOST-OFFSET NUM-ARG HOST-BYTE NUM-ARG
   s" bun" SPAWN ;

\ Reads the file at the path into bytes that hold exactly its size.
: READ-DUMP ( ptr u8 n ptr u8 n -- )
   {: pa:ptr pu:n dst:ptr u:n :}
   pa pu dst u READ-ALL u <> if E-FS-IO throw then ;

\ Reads both dumps; answers the first one's size, the base of the page the peer
\ grew.
: LOAD-DUMPS ( -- n )
   BEFORE-PATH BEFORE-PATH-U @ FILE-SIZE {: bu:n :}
   AFTER-PATH AFTER-PATH-U @ FILE-SIZE {: au:n :}
   bu BEFORE-RESERVE
   au AFTER-RESERVE
   BEFORE-PATH BEFORE-PATH-U @ 0 BEFORE bu READ-DUMP
   AFTER-PATH AFTER-PATH-U @ 0 AFTER au READ-DUMP
   bu ;

\ The text after the last newline once trailing blanks are trimmed.
: LAST-LINE ( ptr u8 n -- ptr u8 n )
   RTRIM {: a:ptr u:n :}
   u begin dup 0 > while
      a over 1- + c@ STR-LF = if a over + u rot - exit then
      1-
   repeat drop a u ;

: NO-BASE ( ptr u8 n -- )
   type cr
   s" wasm data test: OUT's last line is not the dynamic buffer's address" T-EX-FAIL die ;

\ data-region.f prints the dynamic buffer's address last.
: DYN-BASE ( -- n )
   OUT$ LAST-LINE {: a:ptr u:n :}
   a u STR>NUMBER? MATCH option
      some OF ENDOF
      none OF a u NO-BASE ENDOF
   ;MATCH ;

public

: RUN ( -- )
   T-RESET
   SETUP
   s" tools/wasm-build compiles the checked DATA and owner program" T-LABEL
   BUILD 0 T=
   MOD MOD-U @ WASM-HARNESS:VALID? TTRUE
   s" the module runs to status 0 under the foreign peer" T-LABEL
   PEER 0 T=
   LOAD-DUMPS {: host:n :}
   s" the run leaves the static owner's first image page as instantiated" T-LABEL
   WPROF:IMAGE-BASE BEFORE WPROF:PAGE-BYTES  WPROF:IMAGE-BASE AFTER WPROF:PAGE-BYTES
   STR= TTRUE
   s" the task header's stack base cell holds STACK-BASE" T-LABEL
   WPROF:DATA-BASE STACK-ABI:BASE-CELL + AFTER LE:U64@ WPROF:STACK-BASE T=
   s" the task header's stack capacity cell holds STACK-BYTES" T-LABEL
   WPROF:DATA-BASE STACK-ABI:CAP-CELL + AFTER LE:U64@ WPROF:STACK-BYTES T=
   s" the DATA region holds the task header's last cell" T-LABEL
   WPROF:DATA-BYTES STACK-ABI:CAP-CELL 8 + >= TTRUE
   s" the run keeps the byte the peer stored in the page it grew" T-LABEL
   host HOST-OFFSET + AFTER c@ HOST-BYTE T=
   s" OUT starts with the stack depth line" T-LABEL
   OUT$ s\" stack depth ok\n" STARTS-WITH? TTRUE
   s" OUT holds the static owner line, the stored byte and the bounds refusal" T-LABEL
   OUT$ s\" static owner intact\n137\nbounds ok\n" CONTAINS? TTRUE
   DYN-BASE {: b:n :}
   s" the dynamic buffer lies past the page the peer grew" T-LABEL
   b host WPROF:PAGE-BYTES + >= TTRUE
   s" the dynamic buffer's last byte holds what the program stored" T-LABEL
   b DYN-BYTES 1- + AFTER c@ DYN-BYTE T=
   T-REPORT ;

;package

WASM-DATA-TEST:RUN
