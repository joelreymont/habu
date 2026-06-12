\ treeshake.f — THE tree shaker (habu-only; bootstrap stays minimal and never
\ shakes). With SHAKE? on (hb-build's maker), a prim is
\ emitted and seeded ONLY if its name appears as a whitespace token in the
\ user program (SHK-A/SHK-U). Sound over-approximation; default off = keep
\ all. Gates: FPRIM/FPRIM-L (habu1.f) + keyword entries (habu2.f EM-COMPILE);
\ armed by build.f. Load after env.f, before habu1.f.

variable SHAKE?   variable SHK-A   variable SHK-U
variable SKP  variable STS

: SHK-LC ( c -- c )  dup 64 > over 91 < and IF 32 + THEN ;

: SHK-TOK= {: p a u :}
   u 0 ?do  p i + c@ SHK-LC  a i + c@  = 0 = IF unloop 0 EXIT THEN  loop  -1 ;

: KEEP? {: a u :}
   SHAKE? @ 0 = IF -1 EXIT THEN
   0 SKP !
   BEGIN SKP @ SHK-U @ < WHILE
      SHK-A @ SKP @ + c@ 33 < IF
         SKP @ 1 + SKP !
      ELSE
         SKP @ STS !
         BEGIN SKP @ SHK-U @ < IF SHK-A @ SKP @ + c@ 32 > ELSE 0 THEN WHILE
            SKP @ 1 + SKP ! REPEAT
         SKP @ STS @ - u = IF
            SHK-A @ STS @ +  a u SHK-TOK= IF -1 EXIT THEN THEN
      THEN
   REPEAT 0 ;
