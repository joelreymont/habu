\ Focused executable probe for typed W>1 locals.
STRUCTURE wl-pair 0
   FIELD left n
   FIELD right n
;STRUCTURE

: WL-ID ( wl-pair -- wl-pair )
   {: pair:wl-pair :}
   pair ;

: WL-SUM ( wl-pair -- n )
   {: pair:wl-pair :}
   pair WL-PAIR:UNMAKE + ;

7 11 WL-PAIR:MAKE WL-ID WL-PAIR:UNMAKE
11 <> if s" wide local right mismatch" 70 die then
7 <> if s" wide local left mismatch" 70 die then

13 17 WL-PAIR:MAKE WL-SUM
30 <> if s" wide local sum mismatch" 70 die then
