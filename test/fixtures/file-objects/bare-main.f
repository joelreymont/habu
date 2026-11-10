\ bare-main.f - a program with no require, built by tools/hb-build.f between two
\ builds of log-main.f and between two of path-main.f, so that app-end.f's object
\ is this program's. L-GET's fetch of an enum compiles at tier 0 to a call of
\ NFETCH-CHECK by name, which a built program has only from its boot rows; MAIN
\ prints valued.

ENUM lay fresh valued ;ENUM
TYPED-VARIABLE SLOT lay

: L-GET ( ptr lay -- lay ) @ ;

: LAY$ ( lay -- ptr u8 n )
   MATCH lay
      fresh OF s" fresh" ENDOF
      valued OF s" valued" ENDOF
   ;MATCH ;

: MAIN ( -- ) LAY:VALUED SLOT !  SLOT L-GET LAY$ type cr ;
