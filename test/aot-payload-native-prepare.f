\ Loaded by the retained window driver before it opens its literal pool.
\ Match the cold engine's prefix, then open the portable source window.
s" src/lib/prelude.f" provided
include src/core/enums.f
include src/core/combinators.f
require src/habu/code-span.f
require src/habu/xref.f
include src/core/generated-declaration-dictionary.f
include src/core/generated-declaration-protection.f
include src/core/layout-buffer-seal.f
include src/core/lower-cert-seal.f
require src/lib/errors.f
require src/lib/span.f
require src/lib/adt/option.f
require src/lib/num-types.f
require src/lib/num-arithmetic.f
require src/lib/string.f
require src/lib/memory.f
require src/lib/vector.f

package PAYLOAD-NATIVE-PRODUCER
ndict@ here BYTE-VIEW NULL-PTR BYTE-VIEW - variable PRE-R variable PRE-D PRE-D ! PRE-R !
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
