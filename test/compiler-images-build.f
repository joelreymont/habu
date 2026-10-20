\ Build the native suite's three compiler images from one target source load.
\ The caller supplies an absolute private directory and the bundle switch.
1 set-tier
require src/os/script-argv.f
require src/lib/executable-build.f

package COMPILER-IMAGES-BUILD
private
: LOAD ( -- )
   s" tools/native-build-core.f" required
   s" test/compiler/native-host-bundle-check.f" required ;

: BUILD ( -- )
   SCRIPT-ARGC 1 <> if
      s" compiler-images-build: one output directory is required" 64 die
   then
   ['] LOAD EXECUTABLE-BUILD:WITH
   s" 0 SCRIPT-ARGV$ NATIVE-BUILD:RUN-BUNDLE-HOST-CHECK" evaluate-closed ;

' BUILD
;package
execute
