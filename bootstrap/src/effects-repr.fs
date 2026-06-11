\ effects-repr.fs — effect nodes. An effect is a 4-cell arena node holding the
\ data-in, data-out, return-in, return-out stacks. Handle = arena index.

: MK-EFFECT ( din dout rin rout -- e )
   4 ARENA-ALLOT {: din dout rin rout idx :}
   din  idx     ARENA!
   dout idx 1+  ARENA!
   rin  idx 2 + ARENA!
   rout idx 3 + ARENA!
   idx ;

: EFF>DIN  ( e -- s )   ARENA@ ;
: EFF>DOUT ( e -- s )   1+ ARENA@ ;
: EFF>RIN  ( e -- s )   2 + ARENA@ ;
: EFF>ROUT ( e -- s )   3 + ARENA@ ;
