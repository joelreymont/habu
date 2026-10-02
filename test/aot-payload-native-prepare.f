\ Loaded by the retained window driver before it opens its literal pool.
\ Match the cold engine's prefix, then open the portable source window.
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
require lib/span.f
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
