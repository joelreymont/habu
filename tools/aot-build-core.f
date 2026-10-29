\ aot-build-core.f - the linker half of the stripped build; tools/aot-build-open.f
\ runs first and latches the span. Everything here lands above the span: the
\ linker, src/habu/app-image.f and the lib modules app-image-core.f requires. A
\ module the application already loaded is shared; the linker uses it at build
\ time only.
require src/lib/image-lifecycle.f
require src/habu/app-image.f
require src/habu/aot-decl.f
require src/habu/aot-closure.f
require src/habu/aot-lib.f
require tools/aot-code-map.f

package AOT-LINK
private

\ Read the map after resource cleanup and before FFI forgets its addresses: on
\ Darwin RELOAD calls Mach through the shared FFI table, which may be application
\ data, and a lazy first query from LINK would repopulate it after PREPARE and
\ refuse the pointer it stored. Reverse registration order runs this hook before
\ FFI's. The hook is persistent, so a full capture's PREPARE runs it too
\ (test/preloaded-engine.f); that capture drops the map and its image rereads it
\ (src/habu/proc-maps.f "A capture drops the map whole").
' PROC-MAPS:RELOAD IMAGE-LIFECYCLE:REGISTER-PERSISTENT


\ Lifecycle callbacks run before the capture reads application data: a registrant
\ holds process-local state (FFI symbols, a mapped buffer, a session) the image
\ does not restore, and a load-time foreign address is refused without PREPARE
\ (test/stripped-lifecycle-prepare.f). Last before LINK: phase two loads the
\ linker above the span and shares lib modules the application loaded, so a
\ cache released sooner could be retaken inside the span.
: NATIVE-BUILD ( -- )
   AOT-WINDOW-LATCHED
   IMAGE-LIFECYCLE:PREPARE
   AOT-DATA-RELEASE
   BLOB-SRC @ AOT-OWNED:COLLECT
   CHECKER-STORAGE-PREPARE
   [: LINK MAP-WRITE ;] [: FFI:IMAGE-UNPIN ;] finally ;

public

\ Invoke after this file's include has returned, with package scope closed, and
\ after tools/aot-build-open.f has run - AOT-WINDOW-LATCHED refuses otherwise.
: BUILD-NATIVE ( -- )
   ['] NATIVE-BUILD EXECUTABLE-BUILD:WITH ;

;package
