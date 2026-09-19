\ Load the stripped image driver through the retained native compiler.
1 set-tier
require lib/executable-build.f

package AOT-BUILD-ENTRY
private

: LOAD-CORE ( -- ) s" tools/aot-build-core.f" required ;

: LOAD ( -- )
   NSTR:ACTIVE {: application :}
   NSTR:WINDOW-OPEN
   [: LOAD-CORE ;] catch
   application NSTR:SWITCH throw ;

' LOAD
;package
EXECUTABLE-BUILD:WITH
