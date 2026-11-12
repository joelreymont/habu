\ navigation-test.f - Wasm end-to-end browser input, tree, highlight and occlude
\ records.
\ The printed directory keeps the module and expected/actual CLI output.
\ Each kind of delivery navigation.f receives, typed, key and occluded, runs
\ once and four times, and both runs end with the same memory: every run frees
\ its delivery's page.

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
   RUN-ARGV-CAPTURE-OUTCOME PROCESS:ENDING>RC RC>N
   {: outu:len erru:len rc:n :}
   outu LEN>N OUT-U ! erru LEN>N ERR-U ! rc ;

: BUILD ( -- n )
   s" test/browser/navigation.f" MOD MOD-U @ WASM-HARNESS:BUILD ;

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

: ROWS-WANT ( -- )
   \ navigation.f's tree line through Shelf's label.
   s\" tree 2 [{\"key\":1,\"parent\":0,\"label\":\"Assembly\"}," SB-APPEND
   s\" {\"key\":2,\"parent\":1,\"label\":\"Part\"}," SB-APPEND
   s\" {\"key\":3,\"parent\":2,\"label\":\"Face\"}," SB-APPEND
   s\" {\"key\":4,\"parent\":0,\"label\":\"Shelf\"" SB-APPEND ;

: START-WANT ( -- )
   SB-RESET
   s" hello" LINE
   ROWS-WANT s\" ,\"more\":true}]" LINE
   s" mesh 7 6 2" LINE
   s" highlight 1 01000000" LINE
   s" draw 2 7/10/0/0/ff7314/0/32768 7/11/1/-/2060c0/65535/0"
      LINE ;

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
   s" --open" ARG+ 4 N-ARG+
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
   ROWS-WANT
   s\" },{\"key\":5,\"parent\":4,\"label\":\"Board\"}," SB-APPEND
   s\" {\"key\":6,\"parent\":4,\"label\":\"Bracket\"}]" LINE
   s\" tree 3 [{\"key\":1,\"parent\":0,\"label\":\"Assembly\"},{\"key\":2,\"parent\":1,\"label\":\"Part\"},{\"key\":3,\"parent\":2,\"label\":\"Face\"}]" LINE
   SB$ OUTPUT!
   ACTUAL!
   s" input fields, tree opening and selection round-trip through Wasm" T-LABEL
   OUT OUT-U @ SB$ T$= ;

: ZERO-CASE ( -- )
   HOST-OPEN s" zero" TYPED+
   HOST 0 T=
   START-WANT
   s" draw 0" LINE
   s" highlight 0" LINE
   s" zero instances clear safely" T-LABEL
   OUT OUT-U @ SB$ T$= ;

: REMESH-CASE ( -- )
   HOST-OPEN s" remesh" TYPED+
   HOST 0 T=
   START-WANT
   s" draw 0" LINE
   s" unmesh 7" LINE
   s" mesh 7 6 2" LINE
   s" draw 2 7/10/0/0/ff7314/0/32768 7/11/1/-/2060c0/65535/0"
      LINE
   s" a mesh no row draws unmeshes and meshes again by its handle" T-LABEL
   OUT OUT-U @ SB$ T$= ;

: REDRAW-CASE ( -- )
   HOST-OPEN s" redraw" TYPED+
   HOST 0 T=
   START-WANT
   s" draw 2 7/10/0/0/ff7314/0/32768 7/11/1/-/2060c0/65535/0"
      LINE
   s" a redraw draws the held mesh with the held highlight" T-LABEL
   OUT OUT-U @ SB$ T$= ;

: OCCLUDE-CASE ( -- )
   HOST-OPEN s" occlude" TYPED+ s" --occluded" ARG+ s" 0100" ARG+
   HOST 0 T=
   START-WANT
   s" occlude 2 9" LINE
   s" draw 2 7/10/0/0/ff7314/0/32768 7/11/1/-/2060c0/65535/0"
      LINE
   s" text occluded 0100 0,9 640,480" LINE
   s" an occlude's answer reaches the module as event 19" T-LABEL
   OUT OUT-U @ SB$ T$= ;

: UNANSWERED-CASE ( -- )
   HOST-OPEN s" occlude" TYPED+
   HOST 0 T=
   START-WANT
   s" occlude 2 9" LINE
   s" draw 2 7/10/0/0/ff7314/0/32768 7/11/1/-/2060c0/65535/0"
      LINE
   s" outstanding occlude 9" LINE
   s" an occlude no step answers is outstanding at the end" T-LABEL
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
   s" more" s" more must be a boolean" REJECT
   s" unsent" s" parent 1 has more" REJECT
   s" selected" s" selected tree key" REJECT
   s" length" s" triangles is 92 bytes, not 91" REJECT
   s" index" s" index 6 is not below its 6 vertices" REJECT
   s" unheld" s" a draw of handle 9, which is not held" REJECT
   s" unmesh" s" an unmesh of handle 9, which is not held" REJECT
   s" offset" s" offset 0 needs 2 highlight words; the highlight has 1"
      REJECT
   s" handle" s" a mesh handle 4294967296 is not 1 to 4294967295" REJECT
   s" held" s" a mesh of handle 7, which is already held" REJECT
   s" shown" s" an unmesh of handle 7, which the last draw's rows name"
      REJECT
   s" short"
      s" a highlight of 0 words, fewer than the 1 the last draw's rows need"
      REJECT
   s" flags" s" flags 2 set a bit other than bit 0" REJECT
   s" colour" s" a draw row's colour byte 3 is 1, not 0" REJECT
   s" span" s" an occlude of 47 bytes, not whole 24-byte boxes" REJECT
   s" id" s" an occlude needs a positive id, got 0" REJECT ;

: FREED-CASES ( -- )
   s" four typed turns end with the memory one does"
      MOD MOD-U @ [: s" redraw" TYPED+ ;] BROWSER-MEMORY-TEST:FREED
   s" four key turns end with the memory one does"
      MOD MOD-U @ [: s" --key" ARG+ s" Escape" ARG+ 8 N-ARG+ ;]
      BROWSER-MEMORY-TEST:FREED
   s" four occluded turns end with the memory one does"
      MOD MOD-U @
      [: s" occlude" TYPED+ s" --occluded" ARG+ s" 0100" ARG+ ;]
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
   INPUT-CASE ZERO-CASE REDRAW-CASE REMESH-CASE OCCLUDE-CASE
   UNANSWERED-CASE INVALID-CASES FREED-CASES
   T-REPORT ;

;package

BROWSER-NAV-TEST:RUN
