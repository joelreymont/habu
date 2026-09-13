\ Executable image support and every dependency compile through the optimizer.
1 set-tier
require lib/executable-build.f

package APP-IMAGE-ENTRY
private

: LOAD ( -- ) s" src/habu/app-image-core.f" required ;

' LOAD
;package
EXECUTABLE-BUILD:WITH
