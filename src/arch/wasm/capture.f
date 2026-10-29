\ capture.f - the capture's reader of Wasm shadow emissions: walks NSHADOW's map
\ of a Wasm shadow and fills AOT-SHADOW's tables (src/habu/aot-decl.f), as
\ src/habu/aot-shadow.f does for x86-64. Its own reader because a Wasm address
\ site is the padded SLEB after `i64.const`, which the x86-64 MOVABS carrier
\ cannot read or rewrite; every other rule is aot-shadow.f's, called in its
\ reopened package. It loads beside the Wasm backend, which no engine compiles in.

require src/lib/span.f
require src/core/bytes.f
require src/compiler/native/shadow.f
require src/habu/aot-decl.f
require src/habu/aot-capture.f
require src/habu/aot-shadow.f
require src/arch/wasm/leb.f
require src/arch/wasm/wstruct.f
require src/compiler/native/emission.f

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
   e k ADDR-SITE-KIND@ NEMIT:ADDR-CODE = if
      e v SH-FUN? if  at AOT-SHADOW:FUN 0 SH-SITE+  exit  then
      off v SH-TARGET {: target:n :}
      0 s off S64-PATCH
      at AOT-SHADOW:CODE target SH-SITE+
   else
      v SH-DATA? 0= if off v SH-DATA-OUT then
      v ACAP-W-D0 @ - AOT-BUF:AOT-DATA-D0 @ +  s off S64-PATCH
      at AOT-SHADOW:DATA 0 SH-SITE+
   then ;

\ A call's field is the padded zero index the linker writes, so only its row is
\ carried. A call naming a function of the emission is a self call (the
\ selector's only callee in its own module is function zero, src/arch/wasm/
\ select.f S-SELF): its row names the record being filed, as a call to another
\ shipped definition names that one's.
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
\ first shipped row over it arrives. A shipped record whose emission a slotless
\ clause patch has outdated gets no row, so the linker refuses it by name when
\ reached (link-wasm.f NO-ROUTINE); its call sites still resolve to the shipped
\ row (aot-shadow.f SH-MAPPED), as test/wasm/build.f's span-buffer refusal
\ requires.
: WC-WALK ( -- )
   -1 SH-E !
   RECORDS 0 ?do
      i RECORD@ {: idx:n :}
      idx ACAP-W-R0 @ >=  idx ACAP-W-R1 @ <  and if
         idx SH-REC !
         idx SH-SHIPPED {: row:n :}
         row 0 >= if
            i EMISSION@ {: e:n :}
            e ROUTINE? if
               e SH-E @ <> if e WC-COPY then
               row  i ENTRY@  SH-REC+
            then
         else
            i SH-STRIP
         then
      then
   loop ;

public

\ SH-INDEX and SH-ASSIGN give each shipped record its map row, which SH-TARGET
\ reads; SH-ROOTS marks only rows a shipped record enters, since this reader
\ follows no x86 edges.
: WASM-SHADOW-CAPTURE ( -- )
   SH-ORDER SH-INDEX SH-ROOTS SH-ASSIGN
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
