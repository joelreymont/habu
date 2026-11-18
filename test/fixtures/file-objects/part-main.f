split-str
\ part-main.f - MAIN's fetch of an out-of-domain tag dies with the bad-tag error.

package APP

ENUM lay fresh valued ;ENUM
TYPED-VARIABLE SLOT lay
CAST: RAW ( ^lay -- ^n )

: L-GET ( ^lay -- lay ) @ ;

public

: RUN ( -- ) 7 SLOT RAW !  SLOT L-GET drop  s" passed" type cr ;

;package

: MAIN ( -- ) APP:RUN ;
