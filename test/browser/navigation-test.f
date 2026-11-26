\ navigation-test.f - Wasm end-to-end browser input, tree, toolbar, highlight,
\ pick, occlude, labels and section records.
\ The printed directory keeps the module and expected/actual CLI output.
\ Each kind of delivery navigation.f receives, typed, key, occluded and set, runs
\ once and four times, and both runs end with the same memory: every run frees
\ its delivery's page.

require src/lib/string.f
require src/lib/fmt.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/test/assert.f
require test/wasm/harness.f
require test/browser/memory.f

package BROWSER-NAV-TEST
private

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

: ARG+ ( str -- ) PROC-ARGV+ ;

: N-ARG+ ( n -- )
   SB-RESET FMT:SB-INT SB$ ARG+ ;

: SETUP ( -- )
   s" browser-navigation" FS:MKTEMP-DIR {: d:str :}
   d STR:DATA DIR d STR:LEN BYTE-COPY d STR:LEN DIR-U !
   s" browser navigation: module and CLI output in " type d type cr
   d s" navigation.wasm" MOD FS:JOIN-PATH MOD-U ! ;

: SPAWN ( str -- n )
   OUT CAP >LEN ERR CAP >LEN PROCESS:WAIT-FOREVER
   RUN-ARGV-CAPTURE-OUTCOME PROCESS:ENDING>RC RC>N
   {: outu:len erru:len rc:n :}
   outu LEN>N OUT-U ! erru LEN>N ERR-U ! rc ;

: BUILD ( -- n )
   s" test/browser/navigation.f" MOD MOD-U @ >str WASM-HARNESS:BUILD ;

: HOST-OPEN ( -- )
   PROC-ARGV-RESET
   s" src/lib/browser/host-cli.mjs" ARG+ MOD MOD-U @ >str ARG+ ;

: HOST ( -- n )
   s" bun" EXE RESOLVE-EXECUTABLE {: u:n :}
   EXE u >str SPAWN ;

: STEP+ ( str n n n n -- )
   {: flag:str x:n y:n buttons:n mods:n :}
   flag ARG+ x N-ARG+ y N-ARG+ buttons N-ARG+ mods N-ARG+ ;

: TYPED+ ( str -- )
   s" --typed" ARG+ ARG+ ;

: LINE ( str -- )
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
   s" mesh 7 6 2 0 0 0" LINE
   s" highlight 1 01000000" LINE
   s" draw 2 7/10/0/0/ff7314/0/32768 7/11/1/-/2060c0/65535/0"
      LINE ;

: OUTPUT! ( str -- )
   {: s:str :}
   DIR DIR-U @ >str s" expected.txt" FILE FS:JOIN-PATH {: u:n :}
   FILE u >str s FS:WRITE-ALL ;

: ACTUAL! ( -- )
   DIR DIR-U @ >str s" actual.txt" FILE FS:JOIN-PATH {: u:n :}
   FILE u >str OUT OUT-U @ >str FS:WRITE-ALL ;

: INPUT-CASE ( -- )
   HOST-OPEN
   s" --pointer" 5 6 1 3 STEP+
   s" --move" -3 7 1 3 STEP+
   s" --release" 8 9 0 3 STEP+
   s" --wheel" ARG+ 10 N-ARG+ -20 N-ARG+ 30 N-ARG+ 40 N-ARG+ 4 N-ARG+
   s" --key" ARG+ s" Escape" ARG+ 8 N-ARG+
   s" --resize" ARG+ 100 N-ARG+ 80 N-ARG+
   s" --control" ARG+ 3 N-ARG+
   s" --open" ARG+ 4 N-ARG+
   s" tree" TYPED+
   HOST 0 T=
   START-WANT
   s" text 2:5,6@640,480 1,3 0,0" LINE
   s" text 5:-3,7@640,480 1,3 0,0" LINE
   s" text 6:8,9@640,480 0,3 0,0 pressed 1,3" LINE
   s" text 7:10,-20@640,480 0,4 30,40" LINE
   s" text key Escape 640,480 8" LINE
   s" text 9:1000,0@100,80 0,0 0,0" LINE
   s" text 10:3,0@100,80 0,0 0,0" LINE
   ROWS-WANT
   s\" },{\"key\":5,\"parent\":4,\"label\":\"Board\"}," SB-APPEND
   s\" {\"key\":6,\"parent\":4,\"label\":\"Bracket\"}]" LINE
   s\" tree 3 [{\"key\":1,\"parent\":0,\"label\":\"Assembly\"},{\"key\":2,\"parent\":1,\"label\":\"Part\"},{\"key\":3,\"parent\":2,\"label\":\"Face\",\"dim\":true}]" LINE
   SB$ OUTPUT!
   ACTUAL!
   s" input fields, tree opening, selection and a dim row round-trip through Wasm" T-LABEL
   OUT OUT-U @ >str SB$ T$= ;

: ZERO-CASE ( -- )
   HOST-OPEN s" zero" TYPED+
   HOST 0 T=
   START-WANT
   s" draw 0" LINE
   s" highlight 0" LINE
   s" zero instances clear safely" T-LABEL
   OUT OUT-U @ >str SB$ T$= ;

: REMESH-CASE ( -- )
   HOST-OPEN s" remesh" TYPED+
   HOST 0 T=
   START-WANT
   s" draw 0" LINE
   s" unmesh 7" LINE
   s" mesh 7 6 2 0 0 0" LINE
   s" draw 2 7/10/0/0/ff7314/0/32768 7/11/1/-/2060c0/65535/0"
      LINE
   s" a mesh no row draws unmeshes and meshes again by its handle" T-LABEL
   OUT OUT-U @ >str SB$ T$= ;

: REDRAW-CASE ( -- )
   HOST-OPEN s" redraw" TYPED+
   HOST 0 T=
   START-WANT
   s" draw 2 7/10/0/0/ff7314/0/32768 7/11/1/-/2060c0/65535/0"
      LINE
   s" a redraw draws the held mesh with the held highlight" T-LABEL
   OUT OUT-U @ >str SB$ T$= ;

: PICK-CASE ( -- )
   \ 0.5's f32 bits are 1056964608.
   HOST-OPEN s" pick" TYPED+
   s" --picked" ARG+ 7 N-ARG+ 2 N-ARG+ s" 0.5" ARG+  s" pick" TYPED+
   HOST 0 T=
   START-WANT
   s" pick 5 6 9" LINE
   s" text 18:7,2@1056964608,9 640,480 0,0" LINE
   s" pick 5 6 9" LINE
   s" outstanding pick 9" LINE
   s" a pick's answer reaches the module as a picked event, and a pick no step answers is outstanding" T-LABEL
   OUT OUT-U @ >str SB$ T$= ;

: OCCLUDE-CASE ( -- )
   HOST-OPEN s" occlude" TYPED+ s" --occluded" ARG+ s" 0100" ARG+
   HOST 0 T=
   START-WANT
   s" occlude 2 9" LINE
   s" draw 2 7/10/0/0/ff7314/0/32768 7/11/1/-/2060c0/65535/0"
      LINE
   s" text occluded 0100 0,9 640,480" LINE
   s" an occlude's answer reaches the module as event 19" T-LABEL
   OUT OUT-U @ >str SB$ T$= ;

: UNANSWERED-CASE ( -- )
   HOST-OPEN s" occlude" TYPED+
   HOST 0 T=
   START-WANT
   s" occlude 2 9" LINE
   s" draw 2 7/10/0/0/ff7314/0/32768 7/11/1/-/2060c0/65535/0"
      LINE
   s" outstanding occlude 9" LINE
   s" an occlude no step answers is outstanding at the end" T-LABEL
   OUT OUT-U @ >str SB$ T$= ;

: TOOLBAR-CASE ( -- )
   HOST-OPEN s" toolbar" TYPED+
   s" --set" ARG+ s" find" ARG+ s" bolt M6" ARG+
   HOST 0 T=
   START-WANT
   s\" toolbar [{\"key\":\"fit\",\"label\":\"Fit\",\"shortcut\":\"f\"," SB-APPEND
   s\" \"icon\":\"M4 9V4h5M20 9V4h-5M4 15v5h5M20 15v5h-5\"}," SB-APPEND
   s\" {\"key\":\"mark\",\"label\":\"Mark\",\"pressed\":true}," SB-APPEND
   s\" {\"key\":\"top\",\"label\":\"Top\",\"kind\":\"item\",\"group\":\"Views\",\"pressed\":false}," SB-APPEND
   s\" {\"key\":\"cut\",\"label\":\"Section\",\"kind\":\"range\",\"min\":-50,\"max\":50,\"value\":0}," SB-APPEND
   s\" {\"key\":\"find\",\"label\":\"Filter\",\"kind\":\"text\",\"value\":\"\"}]" LINE
   s" text set find bolt M6 640,480" LINE
   s" a toolbar of every kind reaches the host, and a set reaches the module with its key and text" T-LABEL
   OUT OUT-U @ >str SB$ T$= ;

: LABELS-CASE ( -- )
   HOST-OPEN s" labels" TYPED+
   HOST 0 T=
   START-WANT
   s\" labels 2 1,2,-0.5:\"A\" 0,0.25,0:\"Ø6\"" LINE
   s" a labels record reaches the host as its points and texts" T-LABEL
   OUT OUT-U @ >str SB$ T$= ;

: SECTION-CASE ( -- )
   HOST-OPEN s" section" TYPED+ s" section-none" TYPED+
   HOST 0 T=
   START-WANT
   s" section 0 0 1 -0.5 c08040" LINE
   s" section none" LINE
   s" a section reaches the host as its plane and cap colour, and none clears it" T-LABEL
   OUT OUT-U @ >str SB$ T$= ;

: REJECT ( str str -- )
   {: command:str why:str :}
   HOST-OPEN command TYPED+
   HOST 1 T=
   s" malformed browser record is rejected" T-LABEL
   ERR ERR-U @ >str why STR:CONTAINS? TTRUE ;

: INVALID-CASES ( -- )
   s" json" s" tree JSON" REJECT
   s" utf8" s" UTF-8" REJECT
   s" duplicate" s" duplicate tree key" REJECT
   s" parent" s" parent" REJECT
   s" more" s" more must be a boolean" REJECT
   s" dim" s" dim must be a boolean" REJECT
   s" unsent" s" parent 1 has more" REJECT
   s" selected" s" selected tree key" REJECT
   s" length" s" triangles is 104 bytes, not 103" REJECT
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
   s" flags" s" flags 4 set a bit other than bits 0 and 1" REJECT
   s" colour" s" a draw row's colour byte 3 is 1, not 0" REJECT
   s" pick-id" s" a pick needs a positive id, got 0" REJECT
   s" span" s" an occlude of 47 bytes, not whole 24-byte boxes" REJECT
   s" id" s" an occlude needs a positive id, got 0" REJECT
   s" bar-key" s" a toolbar row's key must be a non-empty string" REJECT
   s" bar-twice" s\" duplicate toolbar key \"a\"" REJECT
   s" bar-label" s\" toolbar row \"a\"'s label must be a string" REJECT
   s" bar-label-empty" s\" toolbar row \"a\"'s label must not be empty" REJECT
   s" bar-kind" s\" kind must be \"button\", \"item\", \"range\" or \"text\"" REJECT
   s" bar-member" s" states step, which a button does not take" REJECT
   s" bar-group" s" group must be a string" REJECT
   s" bar-group-empty" s\" toolbar row \"a\"'s group must not be empty" REJECT
   s" bar-item" s" is an item, which needs a group to name its menu" REJECT
   s" bar-pressed" s" pressed must be a boolean" REJECT
   s" bar-pressed-range" s" states pressed, which a range does not take" REJECT
   s" bar-integer" s" min, max and value must be safe integers" REJECT
   s" bar-order" s" min 5 is not below its max 5" REJECT
   s" bar-within" s" value 11 is not within 0 to 10" REJECT
   s" bar-value" s" value must be a string" REJECT
   s" bar-icon" s" icon of 0 bytes is not 1 to 2048" REJECT
   s" bar-long" s" icon of 2049 bytes is not 1 to 2048" REJECT
   s" bar-markup" s" icon is not SVG path data" REJECT
   s" bar-shortcut-twice" s\" shortcut \"h\" is another row's" REJECT
   s" bar-shortcut-help" s\" shortcut \"?\" is the page's own" REJECT
   s" bar-shortcut-escape" s\" shortcut \"Escape\" is the page's own" REJECT
   s" bar-shortcut-tab" s\" shortcut \"Tab\" is the page's own" REJECT
   s" bar-shortcut-key"
      s\" toolbar row \"group\"'s shortcut \"g\" is row \"g\"'s key" REJECT
   s" bar-shortcut-range" s" states shortcut, which a range does not take" REJECT
   s" labels-span" s" a labels span of 35 bytes does not end on an entry"
      REJECT
   s" labels-infinite" s" label 0 at Infinity, 2, -0.5, not all finite" REJECT
   s" labels-utf8" s" label 0's UTF-8 is invalid" REJECT
   s" section-short" s" a section of 12 bytes, not 0 or 20" REJECT
   s" section-zero" s" a section's normal is zero" REJECT
   s" section-nan" s" a section of 0, 0, 1, NaN, not all finite" REJECT
   s" section-colour" s" a section's colour byte 3 is 1, not 0" REJECT ;

: FREED-CASES ( -- )
   s" four typed turns end with the memory one does"
      MOD MOD-U @ >str [: s" redraw" TYPED+ ;] BROWSER-MEMORY-TEST:FREED
   s" four key turns end with the memory one does"
      MOD MOD-U @ >str [: s" --key" ARG+ s" Escape" ARG+ 8 N-ARG+ ;]
      BROWSER-MEMORY-TEST:FREED
   s" four occluded turns end with the memory one does"
      MOD MOD-U @ >str
      [: s" occlude" TYPED+ s" --occluded" ARG+ s" 0100" ARG+ ;]
      BROWSER-MEMORY-TEST:FREED
   s" four set turns end with the memory one does"
      MOD MOD-U @ >str [: s" --set" ARG+ s" cut" ARG+ s" 12" ARG+ ;]
      BROWSER-MEMORY-TEST:FREED ;

public

: RUN ( -- )
   T-RESET SETUP
   s" browser navigation module builds" T-LABEL
   BUILD {: rc:n :}
   rc 0 T=
   rc 0<> if ERR ERR-U @ >str type cr T-REPORT exit then
   s" browser navigation module validates" T-LABEL
   MOD MOD-U @ >str WASM-HARNESS:VALID? TTRUE
   INPUT-CASE ZERO-CASE REDRAW-CASE REMESH-CASE PICK-CASE
   OCCLUDE-CASE UNANSWERED-CASE TOOLBAR-CASE LABELS-CASE SECTION-CASE
   INVALID-CASES FREED-CASES
   T-REPORT ;

;package

BROWSER-NAV-TEST:RUN
