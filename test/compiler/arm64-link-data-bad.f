\ arm64-link-data-bad.f - the link's refusals, one per process, chosen by the
\ first script argument: `data`, a cell declared as a DATA pointer that holds an
\ address outside the build's space; `header`, a build's xt! into a header cell
\ the link writes. test/compiler/arm64-link-data.f runs each and checks it.

require src/lib/errors.f
require src/lib/string.f
require src/os/script-argv.f
require src/habu/layout.f
require src/habu/xref.f
require src/compiler/native/shadow.f
require src/arch/arm64/icode.f
require src/arch/arm64/link-kernel.f
require src/habu/linker.f
require src/habu/link-band.f
require src/habu/link-data.f

package LINKER
private
package ARM64-LINK-DATA-BAD

CAST: ENTRY-XT ( n -- [ -- ] )


: DATA-BODY ( -- )
   s" variable LD-BAD  1 LD-BAD !  LD-BAD ptr-cell-mark" evaluate-closed
   s" ARM64-LINK-DATA-BAD-ARMED" type cr
   LINK-STRIPPED
   s" arm64-link-data-bad: the DATA pointer was not refused" 1 die ;


: HEADER-BODY ( -- )
   s" : LD-HB ( -- ) ;" evaluate-closed
   s" LD-HB" XREF-FIND-INDEX XREF-REC XREF-START ENTRY-XT
   data-base UNCGH-CELL + xt!
   s" ARM64-LINK-DATA-BAD-ARMED" type cr
   LINK-STRIPPED
   s" arm64-link-data-bad: the header store was not refused" 1 die ;


public

: RUN ( -- )
   0 SCRIPT-ARGV$ {: at:ptr u:n :}
   at u s" data" STR= if s" app" [: DATA-BODY ;] LINKER:WITH exit then
   at u s" header" STR= if s" app" [: HEADER-BODY ;] LINKER:WITH exit then
   s" arm64-link-data-bad: the case is data or header" 1 die ;

;package

ARM64-LINK-DATA-BAD:RUN

;package
