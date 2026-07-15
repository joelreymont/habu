\ bulk-diff-scan.f - AOT external-directory scanner entry.

require tools/bulk-diff-scan-core.f

package BULK-DIFF-CLI
private

PTR-VARIABLE RESULT-A
variable RESULT-U
variable WRITE-OFF

: ARG$ ( n -- ptr u8 n )
   1+ ARGV dup ZLEN ;

: SCAN ( -- )
   0 ARG$ 1 ARG$ 2 ARG$ BULK-DIFF:RUN RESULT-U ! RESULT-A ! ;

: FAIL ( n -- )
   BULK-DIFF:REPORT 1 die ;

: WRITE-RESULT ( -- )
   0 WRITE-OFF !
   begin WRITE-OFF @ RESULT-U @ < while
      1 RESULT-A @ WRITE-OFF @ + RESULT-U @ WRITE-OFF @ - write {: wrote:n :}
      wrote 0 <= if
         S\" {\"phase\":\"stdout\",\"row\":-1,\"side\":\"none\",\"path_hex\":\"\",\"code\":-2105}" 1 die
      then
      WRITE-OFF @ wrote + WRITE-OFF !
   repeat ;

public

: RUN ( -- )
   ARGC 4 <> if
      S\" {\"phase\":\"argv\",\"row\":-1,\"side\":\"none\",\"path_hex\":\"\",\"code\":64}" 64 die
   then
   [: SCAN ;] catch {: code:n :}
   code 0<> if code FAIL then
   WRITE-RESULT ;

;package

: MAIN ( -- )
   BULK-DIFF-CLI:RUN ;
