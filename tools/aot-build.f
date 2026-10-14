\ Load the stripped image driver through the retained native compiler. The
\ engine's maker compiles this file after the application has loaded, into the
\ literal pool src/habu/aot-window-latch.f AOT-DATA-SPAN handed back to it.
1 set-tier
require src/lib/executable-build.f
require src/lib/ffi-abi.f

package AOT-BUILD-ENTRY
private

: LOAD ( -- )
   [: s" tools/aot-build-core.f" required ;]
      FFI:WITH-MAKER-DECLARATIONS ;

' LOAD
;package
EXECUTABLE-BUILD:WITH
