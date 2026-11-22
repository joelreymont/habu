split-str
\ Where an engine word's source is. No record carries a source location yet,
\ so each word answers not found.

require src/core/include.f

package ENGINE-ORIGIN

public

\ Path, start, end and name of SYM's definition.
: TARGET ( n -- str n n str bool )
   drop NULL$ 0 0 NULL$ false ;

\ The symbol defined at a byte of a file.
: SYMBOL-AT ( str n -- n bool )
   drop 2drop 0 false ;

\ Visit the symbols one engine-relative file defines.
: EACH-IN ( str [ n -- ] -- )
   drop 2drop ;

;package
