\ Load after native-window-owner-child's checker handover. Finish the same cold
\ prefix as the fresh graph fixture, so the serialized registry base belongs to
\ the target engine rather than to the retained compiler's larger host registry.
s" lib/prelude.f" provided
include src/core/enums.f
include src/core/type-family-sha.f
include src/core/combinators.f
include src/habu/code-span.f
include src/habu/xref.f
include src/core/generated-declaration-dictionary.f
include src/core/generated-declaration-protection.f
include src/core/layout-buffer-seal.f
s" src/core/checker-owner-abi.f" provided
require src/core/checker-owner-guard.f
include src/core/lower-cert-seal.f
require lib/errors.f
require lib/adt/option.f
require lib/cad-num-types.f
require lib/cad-num-arithmetic.f
require lib/string.f
require lib/memory.f
require lib/vector.f
s" src/habu/layout.f" provided

\ The native reader receives only the completed artifact after this process exits.
package PREFIX-LITERAL-PRODUCER
ndict@ here variable PRE-R variable PRE-D PRE-D ! PRE-R !
;package

require src/habu/aot-arm.f
require src/arch/arm64/asm.f
require src/arch/arm64/icode.f
require src/habu/aot-decl.f
require src/habu/aot-capture.f
require src/habu/aot-ident.f
require src/habu/fdio.f
require src/habu/aot-file.f

package PREFIX-LITERAL-PRODUCER
public
: MODE? ( ptr u8 n -- bool ) s" HABU_LITERAL_MODE" GETENV CORE-STR= ;

\ Evaluation composes the case's source; its emitted definitions still use the
\ live checker and optimizing compiler.
TRUSTED: DEFINE-SOURCE ( ptr u8 n -- ) evaluate ;


: PRELUDE ( -- )
   s" private" MODE? if
      s" package DYNAMIC-STORAGE public export REG-BYTES ;package" DEFINE-SOURCE then
   s" prelude" MODE? if
      s" package LITERAL-PRELUDE public : OUTSIDE-ONLY ( n -- n ) 7 + ; ;package" DEFINE-SOURCE then ;


: DEFINE-WINDOW ( -- )
   s" global" MODE? s" nonentry" MODE? or if
      s" package LITERAL-WINDOW public : HOLDER ( -- [ n -- n ] ) ['] ASCII-UPPER ; ;package" DEFINE-SOURCE exit then
   s" public" MODE? if
      s" package LITERAL-WINDOW public : HOLDER ( -- [ -- n ] ) ['] PREFIX-MARK:REQ ; ;package" DEFINE-SOURCE exit then
   s" private" MODE? if
      s" package LITERAL-WINDOW public : HOLDER ( -- [ n -- n ] ) ['] DYNAMIC-STORAGE:REG-BYTES ; ;package" DEFINE-SOURCE exit then
   s" public-collision" MODE? if
      s" : REQ ( -- n ) 9001 ; package LITERAL-WINDOW public : HOLDER ( -- [ -- n ] ) ['] PREFIX-MARK:REQ ; ;package" DEFINE-SOURCE exit then
   s" shadow" MODE? if
      s" package LITERAL-WINDOW public : HOLDER ( -- [ n -- n ] ) ['] ASCII-UPPER ; ;package undefine ASCII-UPPER : ASCII-UPPER ( n -- n ) drop 9001 ;" DEFINE-SOURCE exit then
   s" prelude" MODE? if
      s" package LITERAL-WINDOW public : HOLDER ( -- [ n -- n ] ) ['] LITERAL-PRELUDE:OUTSIDE-ONLY ; ;package" DEFINE-SOURCE exit then
   s" prefix-literal: unknown mode" 64 die ;
;package

using PREFIX-LITERAL-PRODUCER
using AOT-ARM
1 set-tier
PRELUDE
WINDOW-OPEN
DEFINE-WINDOW
WINDOW-CLOSE
;using
;using

package PREFIX-LITERAL-PRODUCER
using AOT-BUF
using AOT-ARM
using AOT-CAPTURE
using SNAP-RELOC
create KEY 32 allot
create CHAIN 16 allot
variable CHAIN-SITE

: EQ ( n n -- ) <> if 79 throw then ;
: U32@ ( ptr u8 -- n ) {: p:ptr :}
   p c@ p 1+ c@ 8 lshift or p 2 + c@ 16 lshift or p 3 + c@ 24 lshift or ;

TRUSTED: PATCH ( n n -- ) patch32 ;

: ADDRESS-BIT? ( n -- bool ) {: at:n :}
   at dbase@ - {: off:n :}
   data-base ADDRMAP-OFF + off 5 rshift + BYTE-VIEW c@
   off 2 rshift 7 and rshift 1 and 0<> ;

\ Corrupt only the verified literal in this process's checked native holder.
\ No corrupted code executes. The existing capture scanner must refuse an
\ address two bytes inside a real prefix entry, before any artifact is written.
: NONENTRY ( -- )
   s" nonentry" MODE? 0= if exit then
   s" LITERAL-WINDOW:HOLDER" XREF-FIND {: rec:ptr :}
   rec XREF-FOUND? 0= if 79 throw then
   0 CHAIN-SITE !
   rec XREF-LEN 4 / 0 ?do
      rec XREF-START i 4 * + {: at:n :}
      at ADDRESS-BIT? if
         CHAIN-SITE @ 0 EQ
         at 16 + rec XREF-START rec XREF-LEN + > if 79 throw then
         at CHAIN-SITE !
      then
   loop
   CHAIN-SITE @ 0= if 79 throw then
   CHAIN-SITE @ XREF-N>U8 CHAIN 16 BYTE-COPY
   CHAIN CHAINV {: old:n :}
   s" ASCII-UPPER" XREF-FIND XREF-START old EQ
   ndict@ 0 ?do i XREF-REC XREF-START old 2 + = if 79 throw then loop
   CHAIN old 2 + SET-CHAIN
   4 0 ?do CHAIN i 4 * + U32@ CHAIN-SITE @ i 4 * + PATCH loop ;
using AOT-XTSITE
: ROWS ( -- )
   N @ 1 EQ
   BUF@ 4 + U32@ {: off:n :}
   s" named=" type AOT-NAMES-BUF@ off 1+ +
   AOT-NAMES-BUF@ off + c@ type cr ;
;using

using AOT-IDENT
: RUN ( -- )
   B0 @ B1 @ code-origin 1 EQ
   NONENTRY
   PRE-R @ PRE-D @ PRELUDE-MARK
   WINDOW$ CAPTURE
   ROWS
   AOT-IDENT:RESET
   s" test/aot-prefix-literal-producer.f" PATH+
   s" HABU_LITERAL_ENGINE" GETENV KEY SHA256-FILE 0 EQ
   KEY s" HABU_LITERAL_ARTIFACT" GETENV AOT-FILE:WRITE
   s" prefix-literal: captured" type cr ;
;using
RUN
;package
