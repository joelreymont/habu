\ word-load-main.f - its top level runs a word that loads a file, so the build
\ refuses the load.

package FILE-OBJECTS-WORD-LOAD
public
: LOAD-INC ( -- ) s" test/fixtures/file-objects/word-load-inc.f" included ;
;package

: MAIN ( -- ) ;

FILE-OBJECTS-WORD-LOAD:LOAD-INC
