\ habu-array-lib.f - checked array helpers for LLM benchmark arm.

: A@ ( ptr a n -- a )
   cells swap + @ ;

: A! ( a ptr a n -- )
   cells swap + ! ;

: A+! {: delta arr:ptr ix :} ( n ptr a n -- )
   arr ix A@ delta + arr ix A! ;

: A-SWAP {: arr:ptr ix jx :} ( ptr a n n -- )
   arr ix A@ arr jx A@ arr ix A! arr jx A! ;

: LAST-INDEX ( n -- n )
   1 - ;

: MIRROR-INDEX {: len ix :} ( n n -- n )
   len LAST-INDEX ix - ;

: EVEN? ( n -- bool )
   2 mod 0= ;
