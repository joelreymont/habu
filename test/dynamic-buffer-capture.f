\ Capture the same DATA window twice against a retained allocation runtime.
\ Window buffers are cleaned each time; writer buffers before/after it stay live.
package DBUF-CAPTURE-TEST
public
ndict@ here variable PRE-R variable PRE-D PRE-D ! PRE-R !
;package

require lib/test.f
require src/arch/arm64/asm.f
require src/arch/arm64/icode.f
require src/habu/layout.f
require src/habu/aot-decl.f
require src/habu/aot-arm.f
require src/habu/aot-capture.f
require src/compiler/native/string.f

package DBUF-CAPTURE-TEST
DYNAMIC-BUFFER BEFORE n
1 BEFORE-RESERVE 81 0 BEFORE !
;package

AOT-ARM:WINDOW-OPEN
NSTR:WINDOW-OPEN

package DBUF-CAPTURE-VALUE
public
DYNAMIC-BUFFER SCRATCH n
: USE ( -- ) 1 SCRATCH-RESERVE 17 0 SCRATCH ! ;
USE
;package

AOT-ARM:WINDOW-CLOSE

package DBUF-CAPTURE-TEST
DYNAMIC-BUFFER AFTER n
1 AFTER-RESERVE 82 0 AFTER !

: READ-WINDOW ( -- ) 0 DBUF-CAPTURE-VALUE:SCRATCH drop ;

: CAPTURE ( -- )
   PRE-R @ PRE-D @ AOT-CAPTURE:PRELUDE-MARK
   AOT-ARM:WINDOW$ AOT-CAPTURE:CAPTURE ;

: WRITERS-LIVE ( -- )
   0 BEFORE @ 81 T=
   0 AFTER @ 82 T=
   AOT-BUF:AOT-NAMES-LEN @ 0 > TTRUE
   AOT-BUF:AOT-NAMES-BUF@ c@ 0 > TTRUE ;

: RUN ( -- )
   T-RESET
   CAPTURE
   ['] READ-WINDOW 7122 TTHROWS
   WRITERS-LIVE
   DBUF-CAPTURE-VALUE:USE
   0 DBUF-CAPTURE-VALUE:SCRATCH @ 17 T=
   CAPTURE
   ['] READ-WINDOW 7122 TTHROWS
   WRITERS-LIVE
   BEFORE-RELEASE AFTER-RELEASE
   T-REPORT ;

RUN
;package
