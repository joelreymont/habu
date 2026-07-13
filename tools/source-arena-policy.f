\ source-arena-policy.f - checked source-arena capacity policy.

package SOURCE-ARENA

100 constant PCT-BASE

public

: NEED ( n -- n )
   dup SOURCE-HEADROOM-PCT * PCT-BASE 1 - + PCT-BASE / + ;

: NEXT-POW2 ( n -- n ) {: need:n :}
   1 begin dup need < while 1 lshift repeat ;

;package
