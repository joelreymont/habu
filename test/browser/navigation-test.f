\ navigation-test.f - Wasm end-to-end browser input, tree and highlight records.
\ The printed directory keeps the module and expected/actual CLI output.
\ Each kind of delivery navigation.f receives, typed and key, runs once and four
\ times, and both runs end with the same memory: every run frees its delivery's
\ page.

require src/lib/string.f
require src/lib/fmt.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/test.f
require test/wasm/harness.f
require test/browser/memory.f

package BROWSER-NAV-TEST
private

60000 constant DEADLINE-MS
8192 constant CAP
CAP BUFFER: OUT
CAP BUFFER: ERR
variable OUT-U
variable ERR-U
PATH-CAP BUFFER: EXE
PATH-CAP BUFFER: DIR
variable DIR-U
PATH-CAP BUFFER: MOD
variable MOD-U
PATH-CAP BUFFER: FILE

: ARG+ ( ptr u8 n -- ) >LEN PROC-ARGV+ ;

: N-ARG+ ( n -- )
   SB-RESET FMT:SB-INT SB$ ARG+ ;

: SETUP ( -- )
   s" browser-navigation" FS:MKTEMP-DIR {: a:ptr u:n :}
   a DIR u BYTE-COPY u DIR-U !
   s" browser navigation: module and CLI output in " type DIR u type cr
   DIR u s" navigation.wasm" MOD FS:JOIN-PATH MOD-U ! ;

: SPAWN ( ptr u8 len -- n )
   OUT CAP >LEN ERR CAP >LEN DEADLINE-MS >MS
   RUN-ARGV-CAPTURE-OUTCOME PROC-OUTCOME>RC RC>N
   {: outu:len erru:len rc:n :}
   outu LEN>N OUT-U ! erru LEN>N ERR-U ! rc ;

: BUILD ( -- n )
   PROC-ARGV-RESET
   s" --load" ARG+ s" tools/wasm-build.f" ARG+ s" --" ARG+
   s" test/browser/navigation.f" ARG+ s" NAV:TURN" ARG+ MOD MOD-U @ ARG+
   s" bin/hb" >LEN SPAWN ;

: HOST-OPEN ( -- )
   PROC-ARGV-RESET
   s" src/lib/browser/host-cli.mjs" ARG+ MOD MOD-U @ ARG+ ;

: HOST ( -- n )
   s" bun" >LEN EXE RESOLVE-EXECUTABLE {: u:len :}
   EXE u SPAWN ;

: STEP+ ( ptr u8 n n n n n -- )
   {: flag:ptr u:n x:n y:n buttons:n mods:n :}
   flag u ARG+ x N-ARG+ y N-ARG+ buttons N-ARG+ mods N-ARG+ ;

: TYPED+ ( ptr u8 n -- )
   s" --typed" ARG+ ARG+ ;

: LINE ( ptr u8 n -- )
   SB-APPEND 10 SB-APPEND-C ;

: START-WANT ( -- )
   SB-RESET
   s" hello" LINE
   s\" tree 2 [{\"key\":1,\"parent\":0,\"label\":\"Assembly\"},{\"key\":2,\"parent\":1,\"label\":\"Part\"},{\"key\":3,\"parent\":2,\"label\":\"Face\"}]" LINE
   s" draw 2 0000" LINE
   s" highlight 2 0100" LINE
   s" draw 2 0100" LINE ;

: OUTPUT! ( ptr u8 n -- )
   {: a:ptr n:n :}
   DIR DIR-U @ s" expected.txt" FILE FS:JOIN-PATH {: u:n :}
   FILE u a n FS:WRITE-ALL ;

: ACTUAL! ( -- )
   DIR DIR-U @ s" actual.txt" FILE FS:JOIN-PATH {: u:n :}
   FILE u OUT OUT-U @ FS:WRITE-ALL ;

: INPUT-CASE ( -- )
   HOST-OPEN
   s" --pointer" 5 6 1 3 STEP+
   s" --move" -3 7 1 3 STEP+
   s" --release" 8 9 0 3 STEP+
   s" --wheel" ARG+ 10 N-ARG+ -20 N-ARG+ 4 N-ARG+
   s" --key" ARG+ s" Escape" ARG+ 8 N-ARG+
   s" --resize" ARG+ 100 N-ARG+ 80 N-ARG+
   s" --control" ARG+ 3 N-ARG+
   s" tree" TYPED+
   HOST 0 T=
   START-WANT
   s" text 2:5,6@640,480 1,3" LINE
   s" text 5:-3,7@640,480 1,3" LINE
   s" text 6:8,9@640,480 0,3 pressed 1,3" LINE
   s" text 7:10,-20@640,480 0,4" LINE
   s" text key Escape 640,480 8" LINE
   s" text 9:0,0@100,80 0,0" LINE
   s" text 10:3,0@100,80 0,0" LINE
   s\" tree 3 [{\"key\":1,\"parent\":0,\"label\":\"Assembly\"},{\"key\":2,\"parent\":1,\"label\":\"Part\"},{\"key\":3,\"parent\":2,\"label\":\"Face\"}]" LINE
   SB$ OUTPUT!
   ACTUAL!
   s" pointer/wheel/key/resize/control fields and tree selection round-trip through Wasm" T-LABEL
   OUT OUT-U @ SB$ T$= ;

: ZERO-CASE ( -- )
   HOST-OPEN s" zero" TYPED+
   HOST 0 T=
   START-WANT
   s" highlight 0" LINE
   s" draw 0" LINE
   s" zero triangles clear safely" T-LABEL
   OUT OUT-U @ SB$ T$= ;

: REDRAW-CASE ( -- )
   HOST-OPEN s" redraw" TYPED+
   HOST 0 T=
   START-WANT
   s" draw 2 0100" LINE
   s" a camera redraw keeps the selected mask" T-LABEL
   OUT OUT-U @ SB$ T$= ;

: REJECT ( ptr u8 n ptr u8 n -- )
   {: command:ptr cu:n why:ptr wu:n :}
   HOST-OPEN command cu TYPED+
   HOST 1 T=
   s" malformed browser record is rejected" T-LABEL
   ERR ERR-U @ why wu CONTAINS? TTRUE ;

: INVALID-CASES ( -- )
   s" json" s" tree JSON" REJECT
   s" utf8" s" UTF-8" REJECT
   s" duplicate" s" duplicate tree key" REJECT
   s" parent" s" parent" REJECT
   s" selected" s" selected tree key" REJECT
   s" mask" s" highlight mask" REJECT
   s" short" s" highlight count" REJECT
   s" long" s" highlight count" REJECT ;

: FREED-CASES ( -- )
   s" four typed turns end with the memory one does"
      MOD MOD-U @ [: s" redraw" TYPED+ ;] BROWSER-MEMORY-TEST:FREED
   s" four key turns end with the memory one does"
      MOD MOD-U @ [: s" --key" ARG+ s" Escape" ARG+ 8 N-ARG+ ;]
      BROWSER-MEMORY-TEST:FREED ;

public

: RUN ( -- )
   T-RESET SETUP
   s" browser navigation module builds" T-LABEL
   BUILD {: rc:n :}
   rc 0 T=
   rc 0<> if ERR ERR-U @ type cr T-REPORT exit then
   s" browser navigation module validates" T-LABEL
   MOD MOD-U @ WASM-HARNESS:VALID? TTRUE
   INPUT-CASE ZERO-CASE REDRAW-CASE INVALID-CASES FREED-CASES
   T-REPORT ;

;package

BROWSER-NAV-TEST:RUN
