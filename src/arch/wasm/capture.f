\ capture.f - the capture's reader of Wasm shadow emissions: it walks NSHADOW's
\ map of a Wasm shadow (src/arch/wasm/backend.f) and fills AOT-SHADOW's tables
\ (src/habu/aot-decl.f), as src/habu/aot-shadow.f does for x86-64.
\
\ WHY ITS OWN READER. A Wasm emission's address site is the padded ten-byte
\ SLEB after `i64.const` (src/arch/wasm/encode.f), which the x86-64 reader's
\ MOVABS carrier can neither read nor rewrite; WLEB reads and rewrites it in
\ place. Everything else is the x86-64 reader's, called rather than restated, so
\ the two keep one set of rules: the map's publication order (SH-ORDER), the
\ shipped rows (SH-NUMBER, SH-SHIPPED), the stripped and retired records
\ (SH-STRIP), a call row and every target it names (SH-CALL, SH-TARGET), and
\ the declared code cells (SH-XTCELL). It reopens the capture's package for
\ them, as aot-shadow.f does, and loads beside the Wasm backend, which no
\ engine compiles in.
\
\ A call's field is the padded zero index the linker writes, so only its row is
\ carried. A self call (RECURSE) names its own function's body offset, no host
\ address; its row names the record being filed (WC-CALL), as a call to
\ another shipped definition names that one's.

require lib/span.f
require src/core/bytes.f
require src/compiler/native/shadow.f
require src/habu/aot-decl.f
require src/habu/aot-capture.f
require src/habu/aot-shadow.f
require src/arch/wasm/leb.f
require src/arch/wasm/wstruct.f

package AOT-CAPTURE
using NSHADOW
using AOT-SHADOW
using WLEB

\ ---- one emission ---------------------------------------------------------------
\ An address site of the emission just copied, the padded SLEB holding the host
\ value the definition named. A function of the emission's offset, a quotation's
\ or a does> clause's address (src/arch/wasm/encode.f), stays and is a FUN
\ site, as aot-shadow.f SH-ADDR keeps one; another word's entry becomes its
\ row's target and the field 0; a window DATA address becomes its window
\ coordinate, the value an ARM64 DATA site holds (aot-shadow.f SH-DATA?). The
\ encoder rows no other kind.
: WC-ADDR ( n n -- ) {: e:n k:n :}
   e k ADDR-SITE@ {: off:n :}
   CODE-BUF@ SH-AT @ +  SH-LEN @  SPAN:MAKE {: s :}
   s SPAN:$ off S64-PAD@ {: v:n :}
   SH-AT @ off + {: at:n :}
   e k ADDR-SITE-KIND@ WSTRUCT:ADDR-CODE = if
      e v SH-FUN? if  at AOT-SHADOW:FUN 0 SH-SITE+  exit  then
      off v SH-TARGET {: target:n :}
      0 s off S64-PATCH
      at AOT-SHADOW:CODE target SH-SITE+
   else
      v SH-DATA? 0= if off v SH-DATA-OUT then
      v ACAP-W-D0 @ - AOT-BUF:AOT-DATA-D0 @ +  s off S64-PATCH
      at AOT-SHADOW:DATA 0 SH-SITE+
   then ;

\ A call of the emission just copied. One naming a function of the emission is
\ a self call, since the selector's only callee in its own module is function
\ zero, the definition's (src/arch/wasm/select.f S-SELF).
: WC-CALL ( n n -- ) {: e:n k:n :}
   e  e k CALL-TARGET@  SH-FUN? 0= if e k SH-CALL exit then
   SH-AT @ e k CALL-SITE@ +  AOT-SHADOW:CALL
   SH-REC @ SH-SHIPPED AOT-BUF:SITE-REC-TAG or  SH-SITE+ ;

: WC-COPY ( n -- ) {: e:n :}
   e SIZE {: size:n :}
   CODE-LEN @ {: at:n :}
   at size + CODE-LEN !
   e BYTES  CODE-BUF@ at +  size BYTE-COPY
   e SH-E !  at SH-AT !  size SH-LEN !
   e CALL-SITES 0 ?do e i WC-CALL loop
   e ADDR-SITES 0 ?do e i WC-ADDR loop ;

\ ---- the walk -------------------------------------------------------------------
\ aot-shadow.f SH-WALK over the Wasm copy: each emission copied once, when the
\ first shipped row over it arrives.
: WC-WALK ( -- )
   -1 SH-E !
   RECORDS 0 ?do
      i RECORD@ {: idx:n :}
      idx ACAP-W-R0 @ >=  idx ACAP-W-R1 @ <  and if
         idx SH-REC !
         idx SH-SHIPPED {: row:n :}
         row 0 >= if
            i EMISSION@ {: e:n :}
            e SH-E @ <> if e WC-COPY then
            row  i ENTRY@  SH-REC+
         else
            i SH-STRIP
         then
      then
   loop ;

public

\ Fill the shadow tables from the open Wasm shadow's map after capture completion.
: WASM-SHADOW-CAPTURE ( -- )
   SH-ORDER
   SH-NUMBER
   WC-WALK
   AOT-WINDOW:XTOFF-N @ 0 ?do i SH-XTCELL loop ;

: WASM-TARGET-CAPTURE ( n n n n n n -- )
   {: bstart:n bend:n rstart:n rend:n d0:n d1:n :}
   bstart bend rstart rend d0 d1 CAPTURE-PREPARE
   SH-KEEP-PRIVATE
   bstart bend d0 CAPTURE-COMPLETE
   WASM-SHADOW-CAPTURE ;

;using
;using
;using
;package
