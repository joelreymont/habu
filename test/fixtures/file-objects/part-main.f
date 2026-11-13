\ part-main.f - MAIN's fetch of an out-of-domain tag dies with the bad-tag error.

package APP

ENUM lay fresh valued ;ENUM
TYPED-VARIABLE SLOT lay
CAST: N>LAY ( n -- lay )

: L-GET ( ptr lay -- lay ) @ ;

public

: RUN ( -- ) 7 N>LAY SLOT !  SLOT L-GET drop  s" passed" type cr ;

;package

: MAIN ( -- ) APP:RUN ;
