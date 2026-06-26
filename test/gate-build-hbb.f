\ gate-build-hbb.f - checked in-process hb-build helpers for positive AOT gates.
\
\ Load after tools/hb-build-lib.f and test/gate-build-common.f.

: GB-HBB-PREPARE ( -- )
   HBB-RESET-OPTIONS
   GB-SRC$ GB-OUT$ HBB-AOT-PATHS!
   GT-ROOT BF-TMP! ;

: GB-HBB-BUILD-OUT ( ptr u8 n -- ) {: label:ptr labelu :}
   HBB-BUILD-AOT
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
