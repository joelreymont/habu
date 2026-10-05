\ test/native-window-private-axiom-catch.f's refusal after a row of its name:
\ CHECK! records FFI-PTR>CELL's row with no definition and NW-PRIOR-LATER
\ records one after it. The caught refusal keeps both: its rollback cuts only
\ the rows recorded since its definition began, and it recorded none. Prints
\ CHECK!'s verdict, the caught code, then whether each row stays.
package NW-PRIVATE-PRIOR
public
: DEFINE ( -- ) s" : FFI-PTR>CELL ( n -- no-such-type ) drop drop ;" evaluate-closed ;
;package

package FFI
1 set-tier
0 set-check
s" FFI-PTR>CELL ( ptr a -- n ) FFI-PTR>CELL" CHECK! .
: NW-PRIOR-LATER ( n -- n ) 1 + ;
' NW-PRIVATE-PRIOR:DEFINE catch .
s" FFI-PTR>CELL" CHECKER-RECORD-SYM? CHECKER-FIND-USIG-SYM .
s" NW-PRIOR-LATER" EFFECT-QUERY .
;package
