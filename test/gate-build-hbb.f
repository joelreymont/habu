\ gate-build-hbb.f - checked in-process hb-build helpers for positive AOT gates.

require src/lib/fs.f
require tools/hb-build-lib.f
require tools/build-support.f             \ the shared BF-TMP surface
require test/gate-build-common.f

using BUILD-FIXPOINT                     \ the build tmp root
using HB-BUILD-CLI                       \ the hb-build options and entry

: GB-HBB-PREPARE ( -- )
   HBB-RESET-OPTIONS
   GB-SRC$ GB-OUT$ HBB-PATHS!
   GT-ROOT BF-TMP! ;

: GB-HBB-BUILD-OUT ( ptr u8 n -- ) {: label:ptr labelu :}
   HBB-BUILD
   BF-TMP-RESET
   GB-OUT$ FILE? 0= if label labelu GE-FAIL then ;

: GB-HBB-BUILD ( ptr u8 n -- )
   GB-WRITE-SRC
   GB-HBB-PREPARE
   GB-HBB-BUILD-OUT ;

;using                                   \ HB-BUILD-CLI
;using                                   \ BUILD-FIXPOINT
