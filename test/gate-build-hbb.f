\ gate-build-hbb.f - checked in-process hb-build helpers for positive AOT gates.
\
\ Load after tools/hb-build-lib.f and test/gate-build-common.f.

: GB-HBB-PREPARE ( -- )
   HBB-RESET-OPTIONS
   GB-SRC$ GB-OUT$ HBB-PATHS!
   GT-ROOT BF-TMP! ;

: GB-HBB-PREPARE-REPL ( -- )
   HBB-RESET-OPTIONS
   HBB-REPL-ON
   GB-SRC$ GB-OUT$ HBB-PATHS!
   GT-ROOT BF-TMP! ;

: GB-HBB-BUILD-OUT ( ptr u8 n -- ) {: label:ptr labelu :}
   HBB-BUILD
   HBB-ARTIFACT-HIT @ 0 <> if s" artifact-cache-hit" GS-EVENT else s" artifact-cache-miss" GS-EVENT then
   HBB-MAKER-HIT @ 0 <> if s" maker-cache-hit" GS-EVENT then
   HBB-MAKER-BUILD @ 0 <> if s" maker-cache-miss" GS-EVENT s" maker-build" GS-EVENT then
   HBB-MAKER-RUN @ 0 <> if s" maker-run" GS-EVENT then
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

: GB-HBB-BUILD-REPL ( ptr u8 n -- )
   GB-WRITE-SRC
   GB-HBB-PREPARE-REPL
   GB-HBB-BUILD-OUT ;
