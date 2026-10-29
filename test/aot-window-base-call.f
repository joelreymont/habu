\ A created word made just before a window opens stays a named call in it.

\ The prelude mark is the window's own start, so the band is empty and QLBL
\ counts as the booting engine's word, as a prefix word a REPL window mentions.

require src/arch/arm64/asm.f
require src/arch/arm64/icode.f
require src/habu/layout.f
require src/habu/aot-decl.f
require src/habu/aot-arm.f
require src/habu/aot-capture.f

package AOT-WINDOW-BASE-CALL
public
\ The word is empty and the last thing before the window, so its address is the
\ window's DATA base: `create` publishes at an aligned DP, and the opener
\ (src/habu/aot-arm.f LATCH-OPEN) aligns again and latches that DP as D0, where
\ the window's first cell WV also sits.
create QLBL
;package

AOT-ARM:WINDOW-OPEN
package AOT-WINDOW-BASE-CALL-BODY
public
variable WV
\ Tier 0 folds a created word's address into its user only when the word was
\ defined inside the window (src/habu/habu2.f EM-COMPILE-CALL), so XUSE keeps a
\ call to QLBL, carried by name. A fold judged by address alone would file the
\ literal as window DATA offset 0, and seeded XUSE would push WV's address.
: XUSE ( -- ) AOT-WINDOW-BASE-CALL:QLBL 4 type ;
;package
AOT-ARM:WINDOW-CLOSE
AOT-ARM:R0 @ AOT-ARM:D0 @ AOT-CAPTURE:PRELUDE-MARK
AOT-ARM:WINDOW$ AOT-CAPTURE:CAPTURE

package AOT-WINDOW-BASE-CALL
using AOT-BUF

\ The fixture dies if QLBL's address stops being the base.
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
