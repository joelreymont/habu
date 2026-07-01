\ end-to-end-test.f - prove the parse -> metrics wiring on real NDJSON text, not
\ hand-fed values. Builds capture NDJSON lines, validates each through the SCHEMA
\ package (SCHEMA:VALIDATE-LINE), extracts fields via the tools/json.f get-by-key
\ layer, feeds the CAMSYNC timestamp-metrics kernel, and checks the result against
\ the .zig oracle value. The integration driver reopens `package CAMSYNC` (so the
\ TM-/TG-/TX- kernel words are unqualified) and qualifies the cross-package
\ SCHEMA: calls. This is the wiring the per-kernel tests stub with direct feeds.
\ Run: ../habu/bin/hb --load odin/end-to-end-test.f

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/float.f
require lib/sort.f
require lib/hashmap.f
require lib/test.f
require tools/json.f
require odin/capture-schema.f
require odin/capture-schema-json.f
require odin/timestamp-metrics.f

package CAMSYNC
private

\ --- NDJSON builder (one object into TB, accumulated into ND with newlines) ---
$800 constant TB-CAP
create TB TB-CAP allot
variable TB-N  variable TB-CP
: TB+C ( n -- ) TB TB-N @ + c!  TB-N @ 1+ TB-N ! ;
: TB+ ( ptr u8 i64 -- ) {: a:ptr u:i64 :} 0 TB-CP ! begin TB-CP @ u < while a TB-CP @ + c@ TB+C TB-CP @ 1+ TB-CP ! repeat ;
: Q$ ( ptr u8 i64 -- ) {: a:ptr u:i64 :} J-DQ TB+C a u TB+ J-DQ TB+C ;
: KEY: ( ptr u8 i64 -- ) Q$ J-COLON TB+C ;
: SVAL ( ptr u8 i64 ptr u8 i64 -- ) {: k:ptr ku:i64 v:ptr vu:i64 :} k ku KEY: v vu Q$ J-COMMA TB+C ;
: RVAL ( ptr u8 i64 ptr u8 i64 -- ) {: k:ptr ku:i64 r:ptr ru:i64 :} k ku KEY: r ru TB+ J-COMMA TB+C ;
: J{ ( -- ) 0 TB-N ! J-LBRACE TB+C ;
: J} ( -- ) J-RBRACE TB TB-N @ 1- + c! ;
: J$ ( -- ptr u8 i64 ) TB TB-N @ ;

$2000 constant ND-CAP
create ND ND-CAP allot
variable ND-N  variable ND-CP
: ND-RESET ( -- ) 0 ND-N ! ;
: ND+LINE ( ptr u8 i64 -- ) {: a:ptr u:i64 :}      \ append a built line + newline
   0 ND-CP ! begin ND-CP @ u < while a ND-CP @ + c@  ND ND-N @ + c!  ND-N @ 1+ ND-N !  ND-CP @ 1+ ND-CP ! repeat
   10 ND ND-N @ + c!  ND-N @ 1+ ND-N ! ;
: ND$ ( -- ptr u8 i64 ) ND ND-N @ ;

\ a full, schema-valid frame line varying (serial, frame_index, sdk, host, dropped)
: FRAME-LINE ( ptr u8 i64 ptr u8 i64 ptr u8 i64 ptr u8 i64 ptr u8 i64 -- )
   {: ser:ptr seru:i64 fi:ptr fiu:i64 sd:ptr sdu:i64 ho:ptr hou:i64 dr:ptr dru:i64 :}
   J{
   s" type" s" frame" SVAL  s" schema_version" s" odin.capture.v1" SVAL
   s" serial" ser seru SVAL  s" logical_name" s" cam_a0" SVAL
   s" frame_index" fi fiu RVAL  s" sdk_image_timestamp_ns" sd sdu RVAL
   s" host_monotonic_ns" ho hou RVAL  s" width" s" 1920" RVAL  s" height" s" 1200" RVAL
   s" fps_target" s" 60" RVAL  s" pixel_format" s" null" SVAL
   s" exposure_us" s" -1" RVAL  s" gain" s" -1" RVAL  s" auto_exposure" s" true" RVAL
   s" image_path" s" null" RVAL  s" dropped" dr dru RVAL
   s" duplicate" s" false" RVAL  s" timestamp_regressed" s" false" RVAL
   J} J$ ND+LINE ;

\ --- parse -> extract -> feed ---
variable EE-ROOT  variable EE-ACC  variable EE-NI
: NUM>I ( ptr u8 i64 -- n ) {: a:ptr u:i64 :}      \ non-negative decimal text -> int
   0 EE-ACC !  0 EE-NI !
   begin EE-NI @ u < while
      a EE-NI @ + c@ 48 -  EE-ACC @ 10 *  +  EE-ACC !
      EE-NI @ 1+ EE-NI !
   repeat
   EE-ACC @ ;
: EE-STR ( ptr u8 i64 -- ptr u8 i64 ) {: k:ptr ku:i64 :} EE-ROOT @ k ku JSON-GET JSON-STRING$ ;
: EE-INT ( ptr u8 i64 -- n ) {: k:ptr ku:i64 :} EE-ROOT @ k ku JSON-GET JSON-NUMBER$ NUM>I ;
: EE-BOOL ( ptr u8 i64 -- n ) {: k:ptr ku:i64 :}
   EE-ROOT @ k ku JSON-GET JSON-BOOL@ if 1 else 0 then ;
variable EE-FIDX variable EE-SDK variable EE-HOST
: EE-FEED ( i64 -- ) EE-ROOT !
   s" type" EE-STR SCHEMA:RTYPE {: rt:n :}
   rt SCHEMA:SCHEMA = if s" command" EE-STR TX-SCHEMA exit then
   rt SCHEMA:SUMMARY = if s" serial" EE-STR  s" frames_dropped" EE-INT  TM-SUMMARY exit then
   rt SCHEMA:FRAME = if
      s" frame_index" EE-INT EE-FIDX !  s" sdk_image_timestamp_ns" EE-INT EE-SDK !  s" host_monotonic_ns" EE-INT EE-HOST !
      s" serial" EE-STR  s" logical_name" EE-STR  s" fps_target" EE-INT  EE-FIDX @ EE-SDK @ EE-HOST @
      s" dropped" EE-BOOL  s" duplicate" EE-BOOL  s" timestamp_regressed" EE-BOOL  TM-ADD
      EE-FIDX @ EE-SDK @ EE-HOST @ TG-ADD
   then ;

variable EE-VST  variable EE-RT2  variable EE-PST
: EE-LINE ( ptr u8 i64 -- ) {: la:ptr lu:i64 :}     \ validate + parse + feed one line
   lu 0 > if
      la lu SCHEMA:VALIDATE-LINE nip EE-VST !    \ status; rtype ignored here
      EE-VST @ SCHEMA:V-OK = if
         la lu JSON-PARSE-TRY drop EE-PST !  EE-RT2 !    \ (root st) -> vars (no mid-control locals)
         EE-PST @ JSON-PARSE-OK = if EE-RT2 @ EE-FEED then
      then
   then ;
variable EE-LS variable EE-LI
: EE-RUN ( ptr u8 i64 -- ) {: a:ptr u:i64 :}        \ split NDJSON on newlines, feed each
   0 EE-LS !  0 EE-LI !
   begin EE-LI @ u < while
      a EE-LI @ + c@ 10 = if
         a EE-LS @ +  EE-LI @ EE-LS @ -  EE-LINE
         EE-LI @ 1+ EE-LS !
      then
      EE-LI @ 1+ EE-LI !
   repeat
   EE-LS @ u < if a EE-LS @ +  u EE-LS @ -  EE-LINE then ;

: E2E-RUN ( -- )
   T-RESET  TM-RESET  TG-RESET  TX-RESET  ND-RESET
   \ schema (multi-helper) + three cam_a0 frames at ~16.6667ms period
   J{ s" type" s" schema" SVAL s" schema_version" s" odin.capture.v1" SVAL
      s" helper_version" s" 0.1.0" SVAL s" sdk_version" s" 5" SVAL s" host" s" zb" SVAL
      s" command" s" capture-null-multi" SVAL s" config_path" s" null" RVAL J} J$ ND+LINE
   s" 306885122" s" 0" s" 1000000000" s" 2000000000" s" false" FRAME-LINE
   s" 306885122" s" 1" s" 1016666667" s" 2016666667" s" false" FRAME-LINE
   s" 306885122" s" 2" s" 1033333334" s" 2033333334" s" false" FRAME-LINE
   ND$ EE-RUN
   TM-FINISH
   \ metrics computed from PARSED records match the .zig oracle (test 1667 cam_a0)
   1 TM-COUNT T=
   3 0 TM-FRAMES@ T=
   2 0 TM-PSAMP@ T=
   16666667 0 TM-SDK-MEAN@ T= ;

E2E-RUN
T-REPORT

end-package
