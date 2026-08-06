\ gate-build-hbb.f - checked in-process hb-build helpers for positive AOT gates.
\
\ Load after tools/hb-build-lib.f and test/gate-build-common.f.

using GATE

: GB-HBB-PREPARE ( -- )
   HBB-RESET-OPTIONS
   GB-SRC$ GB-OUT$ HBB-PATHS!
   GT-ROOT BF-TMP! ;

: GB-HBB-BUILD-OUT ( ptr u8 n -- ) {: label:ptr labelu :}
   HBB-BUILD
   HB-BUILD:ARTIFACT-HIT? if s" artifact-cache-hit" GS-EVENT else s" artifact-cache-miss" GS-EVENT then
   HB-BUILD:OBJECT-HIT? if s" object-cache-hit" GS-EVENT then
   HB-BUILD:MAKER-HIT? if s" maker-cache-hit" GS-EVENT then
   HB-BUILD:MAKER-BUILT? if s" maker-cache-miss" GS-EVENT s" maker-build" GS-EVENT then
   HB-BUILD:MAKER-RAN? if s" maker-run" GS-EVENT then
   BF-TMP-RESET
   GB-OUT$ FILE? 0= if label labelu GE-FAIL then ;

: GB-HBB-BUILD ( ptr u8 n -- )
   GB-WRITE-SRC
   GB-HBB-PREPARE
   GB-HBB-BUILD-OUT ;

: GB-HBB-BUILD-STRICT ( ptr u8 n -- )
   GB-WRITE-SRC
   GB-HBB-PREPARE
   HBB-STRICT-ON
   GB-HBB-BUILD-OUT ;

;using
