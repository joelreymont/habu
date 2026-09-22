\ gate-build-hbb.f - checked in-process hb-build helpers for positive AOT gates.

\ tools/build-fixpoint.f requires none of its lib preamble by design - its
\ --load list is caller-composed - so this file names that preamble here.
require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/codesign.f
require tools/build-fixpoint.f
require tools/cli-run.f
require tools/hb-build-lib.f
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
