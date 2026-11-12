\ part-main.f - a part NFETCH-CHECK with a BAD-TAG that returns; MAIN's fetch
\ of an out-of-domain tag still dies with the bad-tag error.

package APP

package NFETCH-CHECK
public
: BAD-TAG ( n n -- ) 2drop ;
;package

ENUM lay fresh valued ;ENUM
TYPED-VARIABLE SLOT lay
CAST: N>LAY ( n -- lay )

: L-GET ( ptr lay -- lay ) @ ;

public

: RUN ( -- ) 7 N>LAY SLOT !  SLOT L-GET drop  s" passed" type cr ;

;package

: MAIN ( -- ) APP:RUN ;
