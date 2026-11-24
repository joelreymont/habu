\ array.f - cell arrays summed, counted and prefix-summed in place (package
\ ARRAY); the caller passes a length that is not negative.
\ STORAGE CLASS. CALLER-OWNED.

package ARRAY

public

: A-SUM ( ^n len -- n ) {: arr:^ len :}
   0
   len LEN>N 0 ?do
      arr i cells + @ +
   loop ;

: A-COUNT-EVEN ( ^n len -- count ) {: arr:^ len :}
   0
   len LEN>N 0 ?do
      arr i cells + @ 2 mod 0= if 1+ then
   loop >COUNT ;

: A-PREFIX-SUM! ( ^n len -- ) {: arr:^ len :}
   len LEN>N 1 <= if exit then
   len LEN>N 1 ?do
      arr i 1 - cells + @
      arr i cells + @ +
      arr i cells + !
   loop ;

;package
