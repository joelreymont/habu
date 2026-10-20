\ A hook-less definition of an owner-private primitive's name records no row:
\ inside FFI, FFI-PTR>CELL binds that primitive's owner-private symbol
\ (src/core/checker.f CK-REC-BIND), and its unknown type prevents recording a
\ declaration. The body compiles against the axiom's one input and is refused
\ (E-NELAB-UNDER -8304). Its rollback has no row to retract; a catch lets the
\ window survive, with the row recorded just before the refusal still kept.
\ DEFINE certifies with the hook on, then compiles the refused definition when
\ it runs inside FFI at tier 1 with the hook cell empty. Prints the caught code,
\ then whether NW-BEFORE still has its row.
package NW-PRIVATE-AXIOM
public
: DEFINE ( -- ) s" : FFI-PTR>CELL ( n -- no-such-type ) drop drop ;" evaluate-closed ;
;package

package FFI
1 set-tier
0 set-check
: NW-BEFORE ( n -- n ) 1 + ;
' NW-PRIVATE-AXIOM:DEFINE catch .
s" NW-BEFORE" EFFECT-QUERY .
;package
