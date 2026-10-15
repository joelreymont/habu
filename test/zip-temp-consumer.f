\ Supply an output path and an unusable TMPDIR to test clean OPEN refusal.
require src/lib/zip.f
require src/lib/zip-test-fixture.f
require src/lib/fs.f
require src/lib/process-argv.f
require src/lib/test.f

package ZIP-TEST

: TRY-TEMP ( ptr u8 n -- ptr u8 n )
   2dup ZIP:OPEN ZIP:CLOSE ;

public
: TEMP-RUN ( -- )
   T-RESET
   0 SCRIPT-ARGV$ {: path:ptr len:n :}
   path len ORIGINAL$ WRITE-ALL
   path len [: TRY-TEMP ;] catch {: code:n :} 2drop
   code ZIP:E-OPEN T=
   T-REPORT path len type cr ;
;package

: MAIN ( -- ) ZIP-TEST:TEMP-RUN ;
