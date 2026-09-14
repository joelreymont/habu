1 set-tier
\ Hold native compilation through every tool dependency and the build command.
require lib/executable-build.f

package HB-BUILD-ENTRY
private

: BUILD ( -- )
   s" tools/hb-build-core.f" required ;

' BUILD
;package
EXECUTABLE-BUILD:WITH
