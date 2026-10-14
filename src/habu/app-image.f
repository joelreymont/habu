\ Executable image support and every dependency compile through the optimizer.
1 set-tier
require src/lib/executable-build.f
require src/lib/ffi-abi.f

package APP-IMAGE-ENTRY
private

: LOAD ( -- )
   [: s" src/habu/app-image-core.f" required ;]
      FFI:WITH-MAKER-DECLARATIONS ;

' LOAD
;package
EXECUTABLE-BUILD:WITH
