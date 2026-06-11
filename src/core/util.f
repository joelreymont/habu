\ util.fs — shared in-subset helpers for the selfhost layer. Load FIRST (before
\ walk.fs / checker.fs / vs.fs, which all use STR=).
variable SEQ
: STR= {: a u b v :} u v = IF -1 SEQ ! 0 BEGIN dup u < WHILE dup a + c@ over b + c@ <> IF 0 SEQ ! THEN 1 + REPEAT drop ELSE 0 SEQ ! THEN SEQ @ ;
\ NUL-terminated path helper for open: copy (a,u) to d, append NUL.
: PATHZ {: a u d :}
   0 BEGIN dup u < WHILE  dup a + c@  over d + c!  1 + REPEAT drop  0 d u + c! ;
create PZB 32 allot
: PATH0 {: a u :}  a u PZB PATHZ  PZB ;     \ ( a u -- pathz ) shared scratch
\ read a little-endian u32 from byte addr p
variable RDP
: RD32 {: p :}  p c@  p 1 + c@ 8 lshift or  p 2 + c@ 16 lshift or  p 3 + c@ 24 lshift or ;
