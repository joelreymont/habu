\ reopen-binding-late.f - the second file of package REOPEN-ORDER: the reopen
\ that gives the package its own `@`. The definition's own body is compiled
\ before the tail exists, so the fetch inside it is the engine's.

require test/reopen-binding-early.f

package REOPEN-ORDER

public

: @ ( ^n n -- n ) {: base:^ off:n :} base off cells + @ ;

: LATE ( -- n ) SLOT 0 @ ;

;package
