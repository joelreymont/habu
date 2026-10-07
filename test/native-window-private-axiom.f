\ A hook-less definition the checker records no row for, of the name an
\ owner-private primitive types. Inside package FFI the name FFI-PTR>CELL binds
\ that primitive's owner-private symbol (src/core/checker.f CK-REC-BIND), the
\ symbol a definition of it is recorded under, and the declaration names an
\ unknown type, so nothing is recorded: the symbol has the axiom and no row.
\ The body is refused (E-NELAB-UNDER: it compiles against the axiom's one
\ input) and rolled back, and the rollback has no row to retract, so the
\ refusal reaches the window as its verdict.
package FFI
1 set-tier
0 set-check
: FFI-PTR>CELL ( n -- no-such-type ) drop drop ;
;package
