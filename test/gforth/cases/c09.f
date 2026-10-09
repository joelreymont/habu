\ recursion through if/else
: FACT ( n -- n ) dup 1 > if dup 1 - recurse * else drop 1 then ;
: C09 ( -- ) 10 FACT . 1 FACT . ;
C09
