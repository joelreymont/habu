\ Run in the source-built cold engine. Prefix targets belong to that real
\ source prefix, while the compiler/capture helpers are excluded tooling.
package NAMED-CELLS-CAPTURE
ndict@ here variable PRE-R variable PRE-D PRE-D ! PRE-R !
;package
require src/arch/arm64/asm.f
require src/arch/arm64/icode.f
require src/habu/layout.f
require src/habu/aot-decl.f
require src/habu/aot-arm.f
require src/habu/aot-capture.f
require src/habu/aot-ident.f
require src/habu/fdio.f
require src/habu/aot-file.f

AOT-ARM:WINDOW-OPEN
include test/aot-named-cells-window.f
AOT-ARM:WINDOW-CLOSE
include test/aot-named-cells-init.f

package NAMED-CELLS-CAPTURE
using AOT-BUF
using AOT-WINDOW
create KEY 32 allot

: U32@ ( ptr u8 -- n ) {: p:ptr :}
   p c@ p 1+ c@ 8 lshift or p 2 + c@ 16 lshift or p 3 + c@ 24 lshift or ;
: U32! ( n ptr u8 -- ) {: value:n p:ptr :}
   4 0 ?do value i 8 * rshift p i + c! loop ;

\ A third argument forges only the name of an otherwise admitted CODE row.
\ The ordinary reader and writer still validate every serialized section.
: FORGE-NAME ( -- )
   SCRIPT-ARGC 2 = if exit then
   2 SCRIPT-ARGV$ {: name:ptr size:n :}
   size 0= size 255 > or if 64 throw then
   AOT-NAMES-LEN @ {: off:n :}
   off size + 1+ AOT-NAMES-RESERVE
   size AOT-NAMES-BUF@ off + c!
   name AOT-NAMES-BUF@ off + 1+ size BYTE-COPY
   size 1+ AOT-NAMES-LEN +!
   XTOFF-N @ 0 ?do
      XTOFF-BUF@ i XTOFF-ROW * + 4 + {: row:ptr :}
      row U32@ XTOFF-KIND-MASK and XTOFF-NAME-TAG = if
         off 1+ XTOFF-NAME-TAG or row U32! unloop exit
      then
   loop
   79 throw ;

: RUN ( -- )
   SCRIPT-ARGC 2 < SCRIPT-ARGC 3 > or if 64 throw then
   NAMED-CELLS-WINDOW:CHECK
   PRE-R @ PRE-D @ AOT-CAPTURE:PRELUDE-MARK
   AOT-ARM:WINDOW$ AOT-CAPTURE:CAPTURE
   AOT-IDENT:RESET
   s" test/aot-named-cells-window.f" AOT-IDENT:PATH+
   s" test/aot-named-cells-init.f" AOT-IDENT:PATH+
   s" NAMED-CELLS-WINDOW:CHECK" AOT-CAPTURE:BOOTRUN+
   FORGE-NAME
   1 SCRIPT-ARGV$ KEY SHA256-FILE 0<> if 79 throw then
   KEY 0 SCRIPT-ARGV$ AOT-FILE:WRITE ;
RUN
;using
;using
;package
