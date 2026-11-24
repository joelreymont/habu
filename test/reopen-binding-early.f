\ reopen-binding-early.f - the first file of package REOPEN-ORDER. It names `@`
\ bare before the package owns that tail, so EARLY is the engine's cell fetch.
\ test/reopen-binding-late.f reopens the package and defines the tail.

package REOPEN-ORDER

private

variable SLOT

public

: PUT ( n -- ) SLOT ! ;
: EARLY ( -- n ) SLOT @ ;

;package
