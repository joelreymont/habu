\ A module `hb-build --target wasm` builds under a foreign Wasm host that grows
\ its memory before the run. The peer,
\ test/wasm/data-peer.mjs, only dumps the module's memory before and after its
\ run and writes OUT; this file makes every check from those bytes.
require src/lib/string.f
require src/lib/fmt.f
require src/lib/le.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/test/assert.f
require src/arch/wasm/profile.f
require src/habu/stack-abi.f
require test/wasm/harness.f
require src/core/exit-status.f

package WASM-DATA-TEST
private

4096 constant CAP
CAP BUFFER: OUT
CAP BUFFER: ERR
PATH-CAP BUFFER: DIR
PATH-CAP BUFFER: MOD
PATH-CAP BUFFER: BEFORE-PATH
PATH-CAP BUFFER: AFTER-PATH
PATH-CAP BUFFER: EXE
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

\ The module's memory as the peer dumped it after the run.
DYNAMIC-BUFFER AFTER u8

: ARG+ ( str -- ) PROC-ARGV+ ;

: NUM-ARG ( n -- )
   SB-RESET FMT:SB-U SB$ ARG+ ;

: SPAWN ( str -- n )
   {: exe:str :}
   exe EXE RESOLVE-EXECUTABLE {: xu:n :}
   EXE xu >str OUT CAP >LEN ERR CAP >LEN PROCESS:WAIT-FOREVER
   RUN-ARGV-CAPTURE-OUTCOME PROCESS:ENDING>RC RC>N
   {: ou:len er:len rc:n :}
   ou LEN>N OUT-U !
   rc 0<> if ERR er LEN>N >str type cr then
   rc ;

: OUT$ ( -- str ) OUT OUT-U @ >str ;

: SETUP ( -- )
   s" wasm-data" FS:MKTEMP-DIR {: dir:str :}
   dir STR:DATA DIR dir STR:LEN BYTE-COPY
   DIR dir STR:LEN >str s" data-region.wasm" MOD FS:JOIN-PATH MOD-U !
   DIR dir STR:LEN >str s" before.bin" BEFORE-PATH FS:JOIN-PATH BEFORE-PATH-U !
   DIR dir STR:LEN >str s" after.bin" AFTER-PATH FS:JOIN-PATH AFTER-PATH-U ! ;

: BUILD ( -- n )
   s" test/wasm/data-region.f" MOD MOD-U @ >str WASM-HARNESS:BUILD ;

: PEER ( -- n )
   PROC-ARGV-RESET
   s" test/wasm/data-peer.mjs" ARG+ MOD MOD-U @ >str ARG+
   BEFORE-PATH BEFORE-PATH-U @ >str ARG+ AFTER-PATH AFTER-PATH-U @ >str ARG+
   HOST-OFFSET NUM-ARG HOST-BYTE NUM-ARG
   s" bun" SPAWN ;

: READ-DUMP ( str str -- )
   {: ps:str dst:str :}
   ps dst STR:DATA dst STR:LEN FS:READ-ALL dst STR:LEN <> if E-FS-IO throw then ;

\ Reads the dump after the run; answers the size of the one before it, the
\ base of the page the peer grew.
: LOAD-DUMPS ( -- n )
   AFTER-PATH AFTER-PATH-U @ >str FS:FILE-SIZE {: au:n :}
   au AFTER-RESERVE
   AFTER-PATH AFTER-PATH-U @ >str 0 AFTER au >str READ-DUMP
   BEFORE-PATH BEFORE-PATH-U @ >str FS:FILE-SIZE ;

: LAST-LINE ( str -- str )
   STR:RTRIM {: t:str :}
   t STR:LEN begin dup 0 > while
      t STR:DATA over 1- + c@ STR:LF = if t STR:DATA over + t STR:LEN rot - >str exit then
      1-
   repeat drop t ;

: NO-BASE ( str -- )
   type cr
   s" wasm data test: OUT's last line is not the dynamic buffer's address" EXIT-STATUS:FAILURE die ;

\ data-region.f prints the dynamic buffer's address last.
: DYN-BASE ( -- n )
   OUT$ LAST-LINE {: line:str :}
   line STR:>NUMBER? MATCH option
      some OF ENDOF
      none OF line NO-BASE ENDOF
   ;MATCH ;

public

: RUN ( -- )
   T-RESET
   SETUP
   s" test/wasm/data-region.f builds into a module that validates" T-LABEL
   BUILD 0 T=
   MOD MOD-U @ >str WASM-HARNESS:VALID? TTRUE
   s" the module runs to status 0 under the foreign peer" T-LABEL
   PEER 0 T=
   LOAD-DUMPS {: host:n :}
   s" the task header's stack base cell holds STACK-BASE" T-LABEL
   WPROF:DATA-VA STACK-ABI:BASE-CELL + AFTER LE:U64@ WPROF:STACK-BASE T=
   s" the task header's stack capacity cell holds STACK-BYTES" T-LABEL
   WPROF:DATA-VA STACK-ABI:CAP-CELL + AFTER LE:U64@ WPROF:STACK-BYTES T=
   s" the run keeps the byte the peer stored in the page it grew" T-LABEL
   host HOST-OFFSET + AFTER c@ HOST-BYTE T=
   s" OUT starts with the stack depth line" T-LABEL
   OUT$ s\" stack depth ok\n" STR:STARTS-WITH? TTRUE
   s" OUT holds the static owner line and the stored byte" T-LABEL
   OUT$ s\" static owner intact\n137\n" STR:CONTAINS? TTRUE
   DYN-BASE {: b:n :}
   s" the dynamic buffer lies past the page the peer grew" T-LABEL
   b host WPROF:PAGE-BYTES + >= TTRUE
   s" the dynamic buffer's last byte holds what the program stored" T-LABEL
   b DYN-BYTES 1- + AFTER c@ DYN-BYTE T=
   T-REPORT ;

;package

WASM-DATA-TEST:RUN
