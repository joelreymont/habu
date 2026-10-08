\ storage-test.f - test/browser/storage.f's module under bun: its dynamic buffer
\ beside the pages lib/browser/host-cli.mjs adds (docs/browser-host.md). The
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

require lib/string.f
require lib/fmt.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/test.f
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
   WPROF:DATA-BASE MOD MOD-U @ FILE-SIZE + WPROF:PAGE-BYTES + 13 + {: u:n :}
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

: HOST ( -- n )
   PROC-ARGV-RESET
   s" lib/browser/host-cli.mjs" ARG+ MOD MOD-U @ ARG+ FILE FILE-U @ ARG+
   s" --pointer" ARG+ s" 3" ARG+ s" 4" ARG+
   s" bun" >LEN EXE RESOLVE-EXECUTABLE {: u:len :}
   EXE u SPAWN ;

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
   PAYLOAD! WANT {: want:ptr wu:n :}
   DIR DIR-U @ s" expected.txt" FILE JOIN-PATH FILE-U !
   FILE FILE-U @ want wu WRITE-ALL
   DIR DIR-U @ s" payload.bin" FILE JOIN-PATH FILE-U !
   s" the buffer grows past the host's pages, never takes them, and its freed pages return cleared" T-LABEL
   HOST {: status:n :}
   status 0 T=
   DIR DIR-U @ s" actual.txt" FILE JOIN-PATH FILE-U !
   FILE FILE-U @ OUT OUT-U @ WRITE-ALL
   OUT OUT-U @ want wu T$=
   T-REPORT ;

;package

BROWSER-STORAGE-TEST:RUN
