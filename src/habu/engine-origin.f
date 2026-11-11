\ Where an engine word's source is. No record carries a source location yet,
\ so each word answers not found.

require src/core/include.f

package ENGINE-ORIGIN
public

\ Path, start, end and name of SYM's definition.
: TARGET ( n -- ptr u8 n n n ptr u8 n bool )
   drop NULL$ 0 0 NULL$ INCLUDE-FALSE ;

\ The symbol defined at a byte of a file.
: SYMBOL-AT ( ptr u8 n n -- n bool )
   drop 2drop 0 INCLUDE-FALSE ;

\ Visit the symbols one engine-relative file defines.
: EACH-IN ( ptr u8 n [ n -- ] -- )
   drop 2drop ;

;package
