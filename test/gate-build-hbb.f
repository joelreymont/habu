\ gate-build-hbb.f - checked in-process hb-build helpers for positive AOT gates.

require lib/fs.f
require tools/hb-build-lib.f
\ tools/build-fixpoint.f requires none of its lib preamble by design - its
\ --load list is caller-composed - and hb-build-lib.f above composes it, so
\ this require of the BF-TMP surface must stay below that line.
require tools/build-fixpoint.f
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
