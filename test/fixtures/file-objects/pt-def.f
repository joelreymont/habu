\ pt-def.f - BLK, which reads tokens through the first ;BLK, and the parses-through: row that says so.

require src/lib/string.f

package FILE-OBJECTS-PT
: END? ( str -- bool ) {: s:str :} s STR:LEN 0=  s s" ;BLK" STR:=  or ;
public
: BLK ( -- ) begin parse-name END? until ;
;package

parses-through: FILE-OBJECTS-PT:BLK 0 ( ;BLK )
