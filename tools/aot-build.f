\ Load the stripped image driver through the retained native compiler.
1 set-tier
require lib/executable-build.f

package AOT-BUILD-ENTRY
private

: LOAD ( -- )
   s" tools/aot-build-core.f" required ;

' LOAD
;package
EXECUTABLE-BUILD:WITH
