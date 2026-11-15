\ pt-use.f - a top-level BLK whose operands the check reads through ;BLK, then the word MAIN calls.

require test/fixtures/file-objects/pt-def.f

package FILE-OBJECTS-PT-USE
public
FILE-OBJECTS-PT:BLK x y ;BLK
: PT-SAY ( -- ) s" PT" type cr ;
;package
