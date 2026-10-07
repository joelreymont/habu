\ echo-test.f - the browser host's turns under bun: lib/browser/host-cli.mjs
\ runs test/browser/echo.f's module through lib/browser/turn.js
\ (docs/browser-host.md). The module is built by tools/wasm-build.f's command
\ line and validated by wasm-tools. bun and wasm-tools must be on PATH, so this
\ is no row of the ordinary gate: test/wasm/device.f runs it, as does
\ `bin/hb --load test/browser/echo-test.f` from the tree's root.
\
\ Start answers hello, `fetch fixture` and the text "loading" in the buffer the
\ bytes run writes its text to, and the host shows that text before it answers
\ the fetch. The fixture's bytes answer the text of the checksum the module
\ computes over the copy in its memory, which this test computes over the
\ file, and each pointer answers "x,y". One run answers
\ the fetch with a small file. The other answers it with a file larger than
\ the module's whole memory, which is at most its data base plus the module
\ file's bytes rounded up to a page, so the host grows the memory past what the
\ module had and the module reads every byte it copied there.
\
\ The module and the files stay in the printed directory.

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

package BROWSER-ECHO-TEST
private

60000 constant DEADLINE-MS
$10000 constant PAGE-BYTES
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

: SETUP ( -- )
   s" browser-echo" HB-TMP-MKDIR {: a:ptr u:n :}
   a DIR u BYTE-COPY  u DIR-U !
   s" browser echo: module and files in " type  DIR u type cr
   DIR u s" echo.wasm" MOD JOIN-PATH MOD-U ! ;

: ARG+ ( ptr u8 n -- )  >LEN PROC-ARGV+ ;

\ The program at the path, run with the arguments staged after
\ PROC-ARGV-RESET; answers its exit status, its stdout in OUT. Its stderr is
\ shown when it fails.
: SPAWN ( ptr u8 len -- n )
   OUT CAP >LEN  ERR CAP >LEN  DEADLINE-MS >MS
   RUN-ARGV-CAPTURE-OUTCOME PROC-OUTCOME>RC RC>N {: outu:len erru:len rc:n :}
   outu LEN>N OUT-U !
   rc 0<> if  ERR erru LEN>N type cr  then
   rc ;

: BUILD ( -- )
   PROC-ARGV-RESET
   s" --load" ARG+  s" tools/wasm-build.f" ARG+  s" --" ARG+
   s" test/browser/echo.f" ARG+  s" ECHO:TURN" ARG+  MOD MOD-U @ ARG+
   s" test/browser/echo.f builds into a module that validates" T-LABEL
   s" bin/hb" >LEN SPAWN 0 T=
   MOD MOD-U @ WASM-HARNESS:VALID? TTRUE ;

\ The bytes written as the file name in DIR, the file the next run answers
\ the fetch with.
: FILE! ( ptr u8 n ptr u8 n -- )
   {: a:ptr u:n name:ptr nu:n :}
   DIR DIR-U @ name nu FILE JOIN-PATH FILE-U !
   FILE FILE-U @ a u WRITE-ALL ;

\ The checksum echo.f's module answers for bytes.
: SUM ( ptr u8 n -- n )
   {: a:ptr u:n :}
   0  u 0 ?do  31 *  a i + c@ +  $FFFFFFFF and  loop ;

\ The lines of a run whose fixture's bytes sum to n, before its pointers'.
: WANT ( n -- )
   SB-RESET
   S\" hello\ntext loading\nfetch fixture\ntext " SB-APPEND  FMT:SB-U  10 SB-APPEND-C ;

: WANT-POINTER ( n n -- )
   {: x:n y:n :}
   s" text " SB-APPEND  x FMT:SB-U  44 SB-APPEND-C  y FMT:SB-U  10 SB-APPEND-C ;

\ host-cli.mjs on the module and the file, the pointer arguments staged after
\ it by POINTER+; answers its exit status.
: HOST-OPEN ( -- )
   PROC-ARGV-RESET
   s" lib/browser/host-cli.mjs" ARG+  MOD MOD-U @ ARG+  FILE FILE-U @ ARG+ ;

: POINTER+ ( n n -- )
   {: x:n y:n :}
   s" --pointer" ARG+
   SB-RESET x FMT:SB-U SB$ ARG+
   SB-RESET y FMT:SB-U SB$ ARG+ ;

: HOST ( -- n )
   s" bun" >LEN EXE RESOLVE-EXECUTABLE {: u:len :}
   EXE u SPAWN ;

: SMALL-CASE ( -- )
   s" the fixture's bytes" {: a:ptr u:n :}
   a u s" small.bin" FILE!
   HOST-OPEN  3 4 POINTER+
   s" a small file: hello, loading, fetch fixture, the copy's checksum, then 3,4" T-LABEL
   HOST 0 T=
   a u SUM WANT  3 4 WANT-POINTER
   OUT OUT-U @ SB$ T$= ;

\ Byte i is the low byte of i + i/509; 509 is prime, so the pages differ and a
\ page copied to the wrong place changes the checksum.
: LARGE-CASE ( -- )
   WPROF:DATA-BASE  MOD MOD-U @ FILE-SIZE +  PAGE-BYTES +  13 + {: u:n :}
   u PAYLOAD-RESERVE
   u 0 ?do  i  i 509 / +  $FF and  i PAYLOAD c!  loop
   0 PAYLOAD u s" large.bin" FILE!
   HOST-OPEN  0 0 POINTER+  1279 719 POINTER+
   s" a file larger than the module's memory: the host grows it and the module reads the copy" T-LABEL
   HOST 0 T=
   0 PAYLOAD u SUM WANT  0 0 WANT-POINTER  1279 719 WANT-POINTER
   OUT OUT-U @ SB$ T$= ;

public

: RUN ( -- )
   T-RESET
   SETUP
   BUILD
   SMALL-CASE
   LARGE-CASE
   T-REPORT ;

;package

BROWSER-ECHO-TEST:RUN
