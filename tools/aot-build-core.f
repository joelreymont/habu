\ The application and linker run in one native executable-build scope, and this
\ file is the LINKER half of it. tools/aot-build-open.f is the other half and runs
\ first: it opens the capture window, loads the application and latches the span,
\ because nothing the application can `require` may be loaded when the window
\ opens. Everything below therefore lands ABOVE the span - the linker,
\ src/habu/app-image.f, and the eight lib modules app-image-core.f requires. A
\ module the application already loaded is shared in the harmless direction: the
\ linker uses the application's copy, at build time only.
require src/habu/app-image.f
require src/os/script-argv.f
require src/habu/aot-decl.f
require src/habu/aot-closure.f
require src/habu/aot-lib.f

package AOT-LINK
private

: NATIVE-NIBBLE ( n -- n ) {: c:n :}
   c $30 >= c $39 <= and if c $30 - exit then
   c $61 >= c $66 <= and if c $61 - 10 + exit then
   c $41 >= c $46 <= and if c $41 - 10 + exit then
   s" aot: bad preseed hex digit" 74 die ;


: NATIVE-SEED-CELL ( ptr u8 -- n ) {: a:ptr :}
   0
   16 0 ?do
      4 lshift a i + c@ NATIVE-NIBBLE or
   loop ;


: NATIVE-SEED ( ptr u8 n -- ) {: a:ptr u:n :}
   u 16 mod 0<> if s" aot: preseed hex not cell-aligned" 74 die then
   SEED-RESET
   u 16 / 0 ?do
      a i 16 * + NATIVE-SEED-CELL SEED+
   loop ;


\ The options phase one did not take, now that the words that consume them exist.
\ The argument count was checked there, before anything was loaded on its behalf.
: LINK-OPTIONS ( -- )
   SCRIPT-ARGC 4 = if
      2 SCRIPT-ARGV$ ENTRY-NAME!
      3 SCRIPT-ARGV$ NATIVE-SEED
   then ;


: NATIVE-BUILD ( -- )
   AOT-WINDOW-LATCHED
   LINK-OPTIONS
   LINK ;

public

\ Invoke after this file's include has returned, with package scope closed, and
\ after tools/aot-build-open.f has run - AOT-WINDOW-LATCHED refuses otherwise.
: BUILD-NATIVE ( -- )
   ['] NATIVE-BUILD EXECUTABLE-BUILD:WITH ;

;package
