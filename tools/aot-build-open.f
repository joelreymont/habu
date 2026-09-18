\ Phase one of the stripped build: open the capture window and load the
\ application, with NOTHING the application can `require` loaded yet.
\
\ That is the whole invariant. A library already loaded is the copy the
\ application's own `require` resolves to, so its persistent cells sit below the
\ capture window and src/habu/aot-closure.f DATA-ADDRESS! refuses the image with
\ "address refers to data outside the restored span". tools/aot-build.f pulls in
\ src/habu/app-image.f, and that alone requires eight lib modules, so it must not
\ run until the application has been read and the span latched. The maker child
\ therefore loads THIS file first (tools/hb-build-lib.f HBB-RUN-MAKER-CMD), then
\ tools/aot-build.f, then calls AOT-LINK:BUILD-NATIVE.
\
\ Only lib-free files may be required here. Both of these are baked into the
\ engine or free of lib by construction; neither can pull a lib module in.
1 set-tier
require lib/executable-build.f
require src/os/script-argv.f
require src/habu/aot-window-latch.f

package AOT-LINK
private

\ There is no lib/string.f here, so no STR=: the JSON flag is one character and
\ this is the whole of it. The flag cannot wait for phase two - an application
\ that fails to compile fails inside the `script-required` below, and
\ tools/hb-build.f has already promised its caller which diagnostic shape that
\ produces. DIAG-JSON! is the checker's own typed setter, so no trust row.
: JSON-FLAG? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   u 1 = if a c@ [char] 1 = else false then ;

: OPEN-OPTIONS ( -- )
   SCRIPT-ARGC 2 <> SCRIPT-ARGC 4 <> and if
      s" aot-build: source, JSON flag and optional entry/seed are required" 74 die
   then
   1 SCRIPT-ARGV$ JSON-FLAG? DIAG-JSON! ;

\ The entry name and the preseed are NOT set here: neither changes how the
\ application loads, and both of their setters (aot-closure.f ENTRY-NAME!,
\ aot-lib.f SEED+) belong to the linker, which must not be loaded yet.
: OPEN-AND-LOAD ( -- )
   OPEN-OPTIONS
   AOT-DATA-START
   0 SCRIPT-ARGV$ script-required
   AOT-DATA-SPAN ;

\ Handed out as an xt and run with the package CLOSED, the same shape
\ tools/aot-build.f uses. Both halves matter: AOT-LINK publishes exactly LINK and
\ BUILD-NATIVE, which test/gate-aot-positive-lib.f pins, and `script-required`
\ must not run inside an open package or the application's own definitions would
\ land in this one.
' OPEN-AND-LOAD
;package
EXECUTABLE-BUILD:WITH
