\ Run in the source-built cold engine. Prefix targets belong to that real
\ source prefix, while the compiler/capture helpers are excluded tooling.
package NAMED-CELLS-CAPTURE
ndict@ here variable PRE-R variable PRE-D PRE-D ! PRE-R !
PTR-VARIABLE OLD-POOL
;package
require src/arch/arm64/asm.f
require src/arch/arm64/icode.f
require src/habu/layout.f
require src/habu/aot-decl.f
require src/habu/aot-arm.f
require src/habu/aot-capture.f
require src/habu/aot-shadow.f
require src/habu/aot-ident.f
require src/habu/fdio.f
require src/habu/aot-file.f
require src/compiler/native/abi.f
require src/compiler/native/shadow.f
require src/compiler/native/string.f
require src/arch/x86-64/passes.f

package NAMED-CELLS-CAPTURE
using AOT-BUF
using AOT-WINDOW
private
create KEY 32 allot
create FSHA-CTX SHA256-FILE-CTX-BYTES allot   \ this fixture's file-digest context

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

: CAPTURE-X64 ( -- )
   AOT-ARM:WINDOW$
   ['] AOT-CAPTURE:SHADOW-PRE-REACH ['] AOT-CAPTURE:SHADOW-LIVE?
   AOT-CAPTURE:CAPTURE-NATIVE
   AOT-CAPTURE:SHADOW-CAPTURE
   NSHADOW:CLOSE ;

public
: RUN ( [ -- ] -- ) {: check :}
   SCRIPT-ARGC 2 < SCRIPT-ARGC 3 > or if 64 throw then
   check execute
   PRE-R @ PRE-D @ AOT-CAPTURE:PRELUDE-MARK
   HB-TARGET-LINUX-X86-64? if
      CAPTURE-X64
   else
      AOT-ARM:WINDOW$ AOT-CAPTURE:CAPTURE
   then
   AOT-IDENT:RESET
   s" test/aot-named-cells-window.f" AOT-IDENT:PATH+
   s" test/aot-named-cells-init.f" AOT-IDENT:PATH+
   s" NAMED-CELLS-WINDOW:CHECK" AOT-CAPTURE:BOOTRUN+
   FORGE-NAME
   FSHA-CTX 1 SCRIPT-ARGV$ KEY SHA256-FILE-IN 0<> if 79 throw then
   KEY 0 SCRIPT-ARGV$ AOT-FILE:WRITE ;
;using
;using
;package

package NAMED-CELLS-CAPTURE
: REQUESTED ( -- n ) HB-TARGET-LINUX-X86-64? if 1 else tier@ then ;
REQUESTED
;package
set-tier

package NAMED-CELLS-CAPTURE
: OPEN ( -- )
   HB-TARGET-LINUX-X86-64? if
      NSTR:ACTIVE OLD-POOL !
      NABI:BINDING NSHADOW:OPEN-NATIVE
      AOT-ARM:WINDOW-OPEN
      NSTR:WINDOW-OPEN
   else AOT-ARM:WINDOW-OPEN then ;
' OPEN
public
: CLOSE ( -- )
   HB-TARGET-LINUX-X86-64? if
      NSTR:WINDOW-CLOSE
      OLD-POOL @ NSTR:SWITCH
   then ;
;package
execute
include test/aot-named-cells-window.f
' NAMED-CELLS-CAPTURE:CLOSE execute
AOT-ARM:WINDOW-CLOSE
include test/aot-named-cells-init.f
' NAMED-CELLS-WINDOW:CHECK NAMED-CELLS-CAPTURE:RUN
