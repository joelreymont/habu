\ test/native-window-private-axiom.f's refusal under a catch: the window
\ survives it and the row recorded just before it stays. DEFINE certifies with
\ the hook on and compiles the refused definition when it runs, inside FFI at
\ tier 1 with the hook cell empty. Prints the caught code, then whether
\ NW-BEFORE still has its row.
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
