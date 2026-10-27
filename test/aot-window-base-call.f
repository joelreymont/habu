\ A created word made just before a window opens stays a named call in it.
\
\ THE WORD IS EMPTY AND IS THE LAST THING BEFORE THE WINDOW, so its address is
\ the window's DATA base: `create` publishes at an aligned DP and the opener
\ (src/habu/aot-arm.f LATCH-OPEN) aligns again and latches that same DP as D0.
\ The window's first cell, WV, sits at the same address. Tier 0 folds a created
\ word's address into its user only when the word itself was defined inside the
\ window (src/habu/habu2.f EM-COMPILE-CALL), so XUSE keeps a call to QLBL and
\ the capture carries it by name. A fold judged by the address alone would take
\ QLBL for WV, and the capture would file the literal as window DATA offset 0:
\ seeded, XUSE would push WV's address instead of QLBL's. The fixture dies if
\ QLBL's address ever stops being the base, so the case cannot fall silent.
\
\ THE PRELUDE MARK IS THE WINDOW'S OWN START, so the band is empty and QLBL
\ counts as the booting engine's word, as a prefix word mentioned by a REPL
\ window does.

require src/arch/arm64/asm.f
require src/arch/arm64/icode.f
require src/habu/layout.f
require src/habu/aot-decl.f
require src/habu/aot-arm.f
require src/habu/aot-capture.f

package AOT-WINDOW-BASE-CALL
public
create QLBL
;package

AOT-ARM:WINDOW-OPEN
package AOT-WINDOW-BASE-CALL-BODY
public
variable WV
: XUSE ( -- ) AOT-WINDOW-BASE-CALL:QLBL 4 type ;
;package
AOT-ARM:WINDOW-CLOSE
AOT-ARM:R0 @ AOT-ARM:D0 @ AOT-CAPTURE:PRELUDE-MARK
AOT-ARM:WINDOW$ AOT-CAPTURE:CAPTURE

package AOT-WINDOW-BASE-CALL
using AOT-BUF

: ?BASE ( -- )
   QLBL BYTE-VIEW data-base BYTE-VIEW - DATA-VA VA>N + AOT-ARM:D0 @ = if exit then
   s" aot-window-base-call: QLBL is not the window's DATA base" 74 die ;

: SITE$ ( n -- ptr u8 n ) SITE-ROW * AOT-SITE-BUF@ + 4 + W32@ POOL$ ;

: ?CALLED ( -- )
   AOT-SITE-N @ 0 ?do
      i SITE$ s" AOT-WINDOW-BASE-CALL:QLBL" STR= if unloop exit then
   loop
   s" aot-window-base-call: XUSE does not call QLBL by name" 74 die ;

?BASE ?CALLED
;using
;package
s" AOT-WINDOW-BASE-CALL-CAPTURED" type cr
