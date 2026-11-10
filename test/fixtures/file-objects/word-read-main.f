\ word-read-main.f - its top level runs a word that reads a file through the
\ loader's reader, so the build refuses the read.

package FILE-OBJECTS-WORD-READ
public
: READ-INC ( -- )
   s" test/fixtures/file-objects/word-load-inc.f" SOURCE-ROOT:RESOLVE drop
   SOURCE-ROOT:RESOLVED-ROOT$ SOURCE-ROOT:READ-OS 2drop ;
;package

: MAIN ( -- ) ;

FILE-OBJECTS-WORD-READ:READ-INC
