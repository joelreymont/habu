\ fnv.f - package FNV: the FNV-1a hash in 64 and 32 bits. The 32-bit pair
\ folds in a whole cell, so its top bits can run into the sign.

package FNV
public

$CBF29CE484222325 constant BASIS
$100000001B3 constant PRIME
$811C9DC5 constant BASIS32
$1000193 constant PRIME32

: STEP ( n n -- n ) xor PRIME * ;              \ one byte or cell into the hash
: STEP32 ( n n -- n ) xor PRIME32 * ;

: BYTES ( n str -- n ) {: h:n s:str :}
   h  s STR:LEN 0 ?do s STR:DATA i + c@ STEP loop ;
: BYTES32 ( n str -- n ) {: h:n s:str :}
   h  s STR:LEN 0 ?do s STR:DATA i + c@ STEP32 loop ;

;package
