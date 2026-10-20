\ Production AOT subject for frame slots carried through a conditional loop.
package FRAME-SLOT-IMAGE
private
variable COUNT
variable SNAP

: HIT ( ptr u8 n -- )
   2drop 1 COUNT +! ;

: EARLY ( n -- n )
   begin dup 0 > while 1- repeat ;

: REPLACE ( -- ) 29 SNAP ! ;

: SNAPSHOT ( bool -- n )
   SNAP @ {: saved:n :}
   if REPLACE then
   saved ;

public
: BB ( ptr u8 n -- n )
   {: p:ptr u:n :}
   0 COUNT !
   0 begin dup u < while
      p over + c@ 47 = if p over HIT then
      1+
   repeat drop COUNT @ ;

: CASES ( -- )
   3 EARLY 0 <> if 76 throw then
   17 SNAP ! false SNAPSHOT 17 <> if 77 throw then
   17 SNAP ! true SNAPSHOT 17 <> if 78 throw then
   SNAP @ 29 <> if 79 throw then
   s" " BB 0 <> if 71 throw then
   s" /" BB 1 <> if 72 throw then
   s" abc" BB 0 <> if 73 throw then
   s" a/b/c" BB 2 <> if 74 throw then
   s" /a//b/" BB 4 <> if 75 throw then
   s" frame-slots: ok" type cr ;
;package

: MAIN ( -- ) FRAME-SLOT-IMAGE:CASES ;
