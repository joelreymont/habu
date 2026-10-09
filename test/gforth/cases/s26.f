\ The seal is one-way: SEAL-FRIEND after it changes nothing (habu1.f:3839
\ BSEALFRIEND stores the latch's own value again), even with a package's
\ FRIEND-LATCH-CELL naming the checker's hook cell in scope.
package P
$38 constant FRIEND-LATCH-CELL
SEAL-FRIEND
: F ( -- n ) 1 ;
F .
;package
