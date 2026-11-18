split-str
\ bare-main.f - a program with no require, built by tools/hb-build.f between two
\ builds of log-main.f and between two of path-main.f, so that app-end.f's object
\ is this program's.

ENUM lay fresh valued ;ENUM
TYPED-VARIABLE SLOT lay

: L-GET ( ^lay -- lay ) @ ;

: LAY$ ( lay -- str )
   MATCH lay
      fresh OF s" fresh" ENDOF
      valued OF s" valued" ENDOF
   ;MATCH ;

: MAIN ( -- ) LAY:VALUED SLOT !  SLOT L-GET LAY$ type cr ;
