\ kept-zip.f - the file kept-main.f requires.

require src/lib/string.f
require src/lib/zip.f
require src/lib/zip-test-fixture.f
require src/lib/fs.f
require src/lib/fs-mutate.f

package ZIP-TEST
public
EXPORT ORIGINAL$
;package

package FILE-OBJECTS-KEPT-ZIP
private
create ROOT PATH-CAP allot
variable ROOT-LEN
create PATH PATH-CAP allot

: OPEN-COPY ( -- ZIP:archive )
   s" habu-kept-zip" FS:MKTEMP-DIR {: dir:ptr len:n :}
   dir ROOT len BYTE-COPY  len ROOT-LEN !
   ROOT len s" kept.zip" PATH FS:JOIN-PATH {: path-len:n :}
   PATH path-len ZIP-TEST:ORIGINAL$ FS:WRITE-ALL
   PATH path-len ZIP:OPEN
   ROOT ROOT-LEN @ FS:REMOVE-TREE ;

TYPED-VARIABLE OLD ZIP:archive
OPEN-COPY OLD !

public
: CHECK ( -- )
   OPEN-COPY {: own:ZIP:archive :}
   [: OLD @ ZIP:COUNT drop ;] catch {: code:n :}
   code ZIP:E-HANDLE = if ." zip refused" cr else ." zip " code . cr then
   own ZIP:CLOSE ;
;package
