\ Loaded after native-window-owner-child's checker handover. Match the cold
\ engine's remaining prefix before opening a portable registry window.
s" lib/prelude.f" provided
include src/core/enums.f
include src/core/type-family-sha.f
include src/core/combinators.f
require src/habu/code-span.f
require src/habu/xref.f
include src/core/generated-declaration-dictionary.f
include src/core/generated-declaration-protection.f
include src/core/layout-buffer-seal.f
include src/core/lower-cert-seal.f
require lib/errors.f
require lib/adt/option.f
require lib/num-types.f
require lib/num-arithmetic.f
require lib/string.f
require lib/memory.f
require lib/vector.f

package PAYLOAD-NATIVE-PRODUCER
ndict@ here variable PRE-R variable PRE-D PRE-D ! PRE-R !
;package

s" src/habu/layout.f" provided
s" src/core/checker-owner-abi.f" provided
require src/habu/aot-arm.f
require src/arch/arm64/asm.f
require src/arch/arm64/icode.f
require src/habu/aot-decl.f
require src/habu/aot-capture.f
require src/habu/aot-ident.f
require src/habu/fdio.f
require src/habu/aot-file.f

1 set-tier
AOT-ARM:WINDOW-OPEN

package PAYLOAD-NATIVE
public

STRUCTURE pair 0
   FIELD left n
   FIELD right n
;STRUCTURE

: BUMP ( n -- n ) 1+ ;

\ An unchecked native row is still useful ABI metadata after import.
0 set-check
: ABI-ONLY ( n -- n ) ;
TRUSTED: ASSERTED ( n -- n ) ;
' LOWER-CERT-HOOK:HOOK set-check


: PAIR-SUM ( pair -- n ) PAYLOAD--NATIVE-PAIR:UNMAKE + ;

;package

AOT-ARM:WINDOW-CLOSE

package PAYLOAD-NATIVE-PRODUCER
create KEY 32 allot
create FSHA-CTX SHA256-FILE-CTX-BYTES allot   \ this fixture's file-digest context

: EQ ( n n -- ) <> if 79 throw then ;


: RUN ( -- )
   AOT-ARM:B0 @ AOT-ARM:B1 @ code-origin 1 EQ
   PRE-R @ PRE-D @ AOT-CAPTURE:PRELUDE-MARK
   AOT-ARM:WINDOW$ AOT-CAPTURE:CAPTURE
   AOT-IDENT:RESET
   s" test/aot-payload-native-producer.f" AOT-IDENT:PATH+
   FSHA-CTX s" HABU_PAYLOAD_TEST_ENGINE" GETENV KEY SHA256-FILE-IN 0 EQ
   KEY s" HABU_PAYLOAD_TEST_ARTIFACT" GETENV AOT-FILE:WRITE
   s" native graph artifact written" type cr ;

RUN
;package
