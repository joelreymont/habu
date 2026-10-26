\ This file is the LINKER half of the stripped build. tools/aot-build-open.f
\ runs first: it retires temporary support, opens the capture window, loads the
\ application and latches the span. No library visible to the application's
\ `require` is loaded when that window opens. Everything below therefore lands
\ ABOVE the span - the linker,
\ src/habu/app-image.f, and the eight lib modules app-image-core.f requires. A
\ module the application already loaded is shared in the harmless direction: the
\ linker uses the application's copy, at build time only.
require src/lib/image-lifecycle.f
require src/habu/app-image.f
require src/habu/aot-decl.f
require src/habu/aot-closure.f
require src/habu/aot-lib.f
require tools/aot-code-map.f

package AOT-LINK
private

\ Read the map after resource cleanup but before FFI forgets its addresses.
\ On Darwin RELOAD calls Mach through the shared FFI table, which may be
\ application data. A lazy first query from LINK repopulates that table after
\ PREPARE and then refuses the pointer it just stored. FFI registered its
\ persistent hook when proc-maps required it; reverse order runs this one first.
\ Being persistent, it runs in every PREPARE of a process that loaded the linker,
\ a full capture's too (the keyed linker image is one, test/preloaded-engine.f).
\ That capture drops what the hook read, so the image it writes reads its own
\ map at its first question (src/habu/proc-maps.f, A CAPTURE DROPS THE MAP WHOLE).
' PROC-MAPS:RELOAD IMAGE-LIFECYCLE:REGISTER-PERSISTENT


\ THE LIFECYCLE CALLBACKS RUN HERE, BEFORE THE CAPTURE READS THE APPLICATION'S
\ DATA. A registrant holds process-local state - package FFI's resolved symbols,
\ a mapped buffer, an open session - taken by THIS process while the application
\ loaded, and the image restores none of it. src/habu/native-runtime.f
\ CAPTURE-PREPARE runs PREPARE first for the engine build and for the snapshot
\ path (src/habu/app-image-core.f APP-IMAGE:SAVE); the stripped link did not,
\ so an application whose load-time code took a foreign address was refused with
\ `stripped AOT persistent data holds a pointer into memory the build mapped`
\ (test/stripped-lifecycle-prepare.f). Last before LINK, not
\ earlier: phase two loads the linker above the latched span and shares whatever
\ lib modules the application itself loaded, so a cache released any sooner
\ could be taken again inside the span.
: NATIVE-BUILD ( -- )
   AOT-WINDOW-LATCHED
   IMAGE-LIFECYCLE:PREPARE
   BLOB-SRC @ AOT-OWNED:COLLECT
   CHECKER-STORAGE-PREPARE
   [: LINK MAP-WRITE ;] [: FFI:IMAGE-UNPIN ;] finally ;

public

\ Invoke after this file's include has returned, with package scope closed, and
\ after tools/aot-build-open.f has run - AOT-WINDOW-LATCHED refuses otherwise.
: BUILD-NATIVE ( -- )
   ['] NATIVE-BUILD EXECUTABLE-BUILD:WITH ;

;package
