\ storage-test.f - test/browser/storage.f's module under bun: its dynamic buffer
\ beside the pages src/lib/browser/host-cli.mjs adds (docs/browser-host.md). The
\ module is built by tools/wasm-build.f's command line and validated by
\ wasm-tools. bun and wasm-tools must be on PATH, so this is no row of the
\ ordinary gate: test/wasm/device.f runs it, as does
\ `bin/hb --load test/browser/storage-test.f` from the tree's root.
\
\ The fetch is answered with a file larger than the module's image, so the
\ host's bytes take several pages between the buffer's first two and the ones
\ it grows into. The run answers hello, "loading", the fetch, the checksum of
\ the host's bytes, which this test computes over the file, and "3,4" for one
\ pointer; a check the module failed shows as its throw code instead. The
\ module, the file, the expected and the actual output stay in the printed
\ directory.

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

package BROWSER-STORAGE-TEST
private

60000 constant DEADLINE-MS
4096 constant CAP
CAP BUFFER: OUT
CAP BUFFER: ERR
variable OUT-U
FS-PATH-CAP BUFFER: EXE
FS-PATH-CAP BUFFER: DIR
variable DIR-U
FS-PATH-CAP BUFFER: MOD
variable MOD-U
FS-PATH-CAP BUFFER: FILE
variable FILE-U
DYNAMIC-BUFFER PAYLOAD u8

: ARG+ ( ptr u8 n -- ) >LEN PROC-ARGV+ ;

: SETUP ( -- )
   s" browser-storage" HB-TMP-MKDIR {: a:ptr u:n :}
   a DIR u BYTE-COPY u DIR-U !
   DIR u s" storage.wasm" MOD JOIN-PATH MOD-U !
   DIR u s" payload.bin" FILE JOIN-PATH FILE-U !
   s" browser storage: module, payload and output in " type DIR u type cr ;

: SPAWN ( ptr u8 len -- n )
   OUT CAP >LEN ERR CAP >LEN DEADLINE-MS >MS
   RUN-ARGV-CAPTURE-OUTCOME PROC-OUTCOME>RC RC>N
   {: outu:len erru:len rc:n :}
   outu LEN>N OUT-U !
   rc 0<> if ERR erru LEN>N type cr then
   rc ;

: BUILD ( -- n )
   PROC-ARGV-RESET
   s" --load" ARG+ s" tools/wasm-build.f" ARG+ s" --" ARG+
   s" test/browser/storage.f" ARG+ s" STORAGE-TURN:TURN" ARG+ MOD MOD-U @ ARG+
   s" bin/hb" >LEN SPAWN ;

: SUM ( ptr u8 n -- n )
   {: a:ptr u:n :}
   0 u 0 ?do 31 * a i + c@ + $FFFFFFFF and loop ;

: PAYLOAD! ( -- n )
   WPROF:IMAGE-BASE MOD MOD-U @ FILE-SIZE + WPROF:PAGE-BYTES + 13 + {: u:n :}
   u PAYLOAD-RESERVE
   u 0 ?do i i 509 / + $FF and i PAYLOAD c! loop
   FILE FILE-U @ 0 PAYLOAD u WRITE-ALL
   0 PAYLOAD u SUM ;

: WANT ( n -- ptr u8 n )
   {: sum:n :}
   SB-RESET
   S\" hello\ntext loading\nfetch fixture\ntext " SB-APPEND
   sum FMT:SB-U 10 SB-APPEND-C
   S\" text 3,4\n" SB-APPEND
   SB$ ;

: HOST ( bool -- n )
   {: repeated:bool :}
   PROC-ARGV-RESET
   s" src/lib/browser/host-cli.mjs" ARG+ MOD MOD-U @ ARG+
   s" --answer" ARG+ s" 200" ARG+ FILE FILE-U @ ARG+
   s" --pointer" ARG+ s" 3" ARG+ s" 4" ARG+ s" 1" ARG+ s" 0" ARG+
   repeated if
      s" --repeat-typed" ARG+ s" a" ARG+ s" 500" ARG+
      s" --repeat-typed" ARG+ s" b" ARG+ s" 500" ARG+
   then
   s" --memory" ARG+
   s" bun" >LEN EXE RESOLVE-EXECUTABLE {: u:len :}
   EXE u SPAWN ;

: MEMORY-BYTES ( n -- n )
   {: prefix:n :}
   OUT-U @ prefix - {: u:n :}
   OUT prefix + u s" memory " STARTS-WITH? 0= if E-PROC-OUTPUT throw then
   OUT prefix + 7 + u 7 - TRIM STR>NUMBER? MATCH option
      none OF E-PROC-OUTPUT throw ENDOF
      some OF ENDOF
   ;MATCH ;

public

: RUN ( -- )
   T-RESET
   SETUP
   s" storage browser module builds" T-LABEL
   BUILD {: rc:n :}
   rc 0 T=
   rc 0<> if T-REPORT exit then
   s" it validates" T-LABEL
   MOD MOD-U @ WASM-HARNESS:VALID? TTRUE
   PAYLOAD! {: sum:n :}
   sum WANT {: want:ptr wu:n :}
   s" the buffer grows past the host's pages, never takes them, and its freed pages return cleared" T-LABEL
   false HOST {: status:n :}
   status 0 T=
   OUT wu want wu T$=
   wu MEMORY-BYTES {: base:n :}
   DIR DIR-U @ s" payload.bin" FILE JOIN-PATH FILE-U !
   s" fetched bytes and two typed batches survive module growth without a page each" T-LABEL
   true HOST 0 T=
   sum WANT drop drop
   s\" text retained 1000\n" SB-APPEND
   SB$ {: all:ptr allu:n :}
   DIR DIR-U @ s" actual.txt" FILE JOIN-PATH FILE-U !
   FILE FILE-U @ OUT OUT-U @ WRITE-ALL
   DIR DIR-U @ s" expected.txt" FILE JOIN-PATH FILE-U !
   FILE FILE-U @ all allu WRITE-ALL
   OUT allu all allu T$=
   allu MEMORY-BYTES base 16 WPROF:PAGE-BYTES * + <= TTRUE
   T-REPORT ;

;package

BROWSER-STORAGE-TEST:RUN
