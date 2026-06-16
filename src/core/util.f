\ util.fs — shared in-subset helpers for the selfhost layer. Load FIRST (before
\ walk.fs / checker.fs / vs.fs, which all use STR=).
variable SEQ

: STR= {: a:ptr u b:ptr v :}   \ ( ptr u8 n ptr u8 n -- bool ) byte-wise string equality
   u v = IF
     -1 SEQ !
     0 BEGIN dup u < WHILE
       dup a + c@  over b + c@  <> IF 0 SEQ ! THEN
       1 + REPEAT drop
	   ELSE 0 SEQ ! THEN
	   SEQ @ 0 <> ;

\ NUL-terminated path helper for open: copy (a,u) to d, append NUL.
256 constant PATH-CAP
: PATHZ {: a:ptr u d:ptr :} ( ptr u8 n ptr u8 -- )
   u 1 + PATH-CAP > IF s" path too long" 76 die THEN
   0 BEGIN dup u < WHILE  dup a + c@  over d + c!  1 + REPEAT drop  0 d u + c! ;
create PZB PATH-CAP allot

: PATH0 {: a:ptr u :} ( ptr u8 n -- ptr u8 )
   a u PZB PATHZ  PZB ;     \ shared scratch
\ read a little-endian u32 from byte addr p
variable RDP

: RD32 {: p:ptr :} ( ptr u8 -- n )
   p c@  p 1 + c@ 8 lshift or  p 2 + c@ 16 lshift or  p 3 + c@ 24 lshift or ;

\ trust stub: subsets that load the builder source WITHOUT the checker still
\ execute the `s" name" s" sig" trust` declarations — drop the four args.
\ checker.f redefines trust with the real USIG recorder (latest wins).
: TRUST {: na nu sa su :} ;
