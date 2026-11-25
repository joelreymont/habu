\ Supply an output path and an unusable TMPDIR: OPEN refuses with E-FS-DIR.
require src/lib/zip.f
require src/lib/zip-test-fixture.f
require src/lib/fs.f
require src/lib/process-argv.f
require src/lib/test.f

package ZIP-TEST

: TRY-TEMP ( str -- str )
   dup ZIP:OPEN ZIP:CLOSE ;

public
: TEMP-RUN ( -- )
   T-RESET
   0 SCRIPT-ARGV$ {: path:str :}
   path ORIGINAL$ FS:WRITE-ALL
   path [: TRY-TEMP ;] catch {: code:n :} drop
   code E-FS-DIR T=
   T-REPORT path type cr ;

;package

: MAIN ( -- ) ZIP-TEST:TEMP-RUN ;
