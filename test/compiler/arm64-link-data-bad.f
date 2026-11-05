\ arm64-link-data-bad.f - a cell declared as a DATA pointer that holds an
\ address outside the build's space is refused; test/compiler/arm64-link-data.f
\ runs it and checks the refusal.

require src/lib/errors.f
require src/habu/layout.f
require src/compiler/native/shadow.f
require src/arch/arm64/icode.f
require src/arch/arm64/link-kernel.f
require src/habu/linker.f
require src/habu/link-band.f
require src/habu/link-data.f

package LINKER
private
package ARM64-LINK-DATA-BAD

: BODY ( -- )
   s" variable LD-BAD  1 LD-BAD !  LD-BAD ptr-cell-mark" evaluate-closed
   s" ARM64-LINK-DATA-BAD-ARMED" type cr
   ARM64:KERNEL
   LINK-STRIPPED
   s" arm64-link-data-bad: the DATA pointer was not refused" 1 die ;

public

: RUN ( -- ) s" app" [: BODY ;] LINKER:WITH ;

;package

ARM64-LINK-DATA-BAD:RUN

;package
