\ kept-zip.f - the file kept-main.f requires.

require src/lib/string.f
require src/lib/zip.f
require src/lib/zip-test-fixture.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/zip-types.f

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
   s" habu-kept-zip" FS:MKTEMP-DIR {: dir:str :}
   dir STR:DATA ROOT dir STR:LEN BYTE-COPY
   dir STR:LEN ROOT-LEN !
   ROOT ROOT-LEN @ >str s" kept.zip" PATH FS:JOIN-PATH
   PATH swap >str {: path:str :}
   path ZIP-TEST:ORIGINAL$ FS:WRITE-ALL
   path ZIP:OPEN
   ROOT ROOT-LEN @ >str FS:REMOVE-TREE ;

TYPED-VARIABLE OLD ZIP:archive
OPEN-COPY OLD !

public
: CHECK ( -- )
   OPEN-COPY {: own:ZIP:archive :}
   [: OLD @ ZIP:COUNT drop ;] catch {: code:n :}
   code ZIP:E-HANDLE = if ." zip rejected" cr else ." zip " code . cr then
   own ZIP:CLOSE ;
;package
