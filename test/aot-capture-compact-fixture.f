\ Shared synthetic graph setup and refusal probe.
require lib/test.f
require lib/process-argv.f
require src/arch/arm64/asm.f
require src/arch/arm64/icode.f
require src/habu/layout.f
require src/habu/aot-decl.f
require src/habu/aot-arm.f
require src/habu/aot-capture.f

package AOT-CAPTURE
using AOT-BUF
public
: CGT-ROW ( n n n -- ) {: k:n start:n size:n :}
   k ACAP-REC-DST {: row:ptr :}
   48 0 ?do 0 row i + c! loop
   start row AOT-N-C!
   size CODE-SPAN:EXACT row 8 + AOT-N-C! ;

: CGT-SETUP ( -- )
   ACAP-RESET
   6 AOT-REC-N ! 6 ACAP-REC-ALL ! 24 AOT-BLOB-LEN !
   6 ACAP-GSITE-RESERVE
   6 0 ?do
      0 i ACAP-GSITE !
      $D65F03C0 AOT-BLOB-BUF@ i 4 * + AOT-P32!
   loop
   0 397 4 CGT-ROW
   398 0 ACAP-REC-DST 8 + AOT-N-C!
   -1 0 ACAP-REC-DST 40 + AOT-N-C!      \ namespace fields are not code
   1 0 4 CGT-ROW
   2 4 4 CGT-ROW
   3 8 8 CGT-ROW                     \ unreachable body to be removed
   4 16 4 CGT-ROW
   5 16 4 CGT-ROW                    \ same-entry alias
   ACAP-GRAPH-INDEX 0 ACAP-GWORK-N ! ;

: CGT-REFUSE ( -- )
   CGT-SETUP
   0 SCRIPT-ARGV$ s" adrp" CORE-STR= if $90000009 else $58000089 then
   AOT-BLOB-BUF@ AOT-P32!
   1 ACAP-GRAPH-MARK-REC ACAP-GRAPH-SWEEP ;

;using
;package
