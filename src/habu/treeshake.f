\ treeshake.f — THE tree shaker. With SHAKE? on (hb-build's maker), a prim is
\ emitted and seeded ONLY if its name appears as a whitespace token in the
\ user program (SHK-A/SHK-U). Sound over-approximation; default off = keep
\ all. Gates: FPRIM/FPRIM-L (habu1.f) + keyword entries (habu2.f EM-COMPILE);
\ armed by build.f. Load after the shared argv/env prefix, before habu1.f.

variable SHAKE?   variable SHK-A   variable SHK-U
variable SKP  variable STS
s" SHAKE?" s" -- ptr n" TRUST
s" SHK-A" s" -- ptr ptr u8" TRUST
s" SHK-U" s" -- ptr n" TRUST
s" SKP" s" -- ptr n" TRUST
s" STS" s" -- ptr n" TRUST
: SHK-A@ ( -- ptr u8 )
   SHK-A @ ;
s" SHK-A@" s" -- ptr u8" TRUST

: SHK-LC ( c -- c )  dup 64 > over 91 < and IF 32 + THEN ;

: SHK-TRUE ( -- bool )
   0 0= ;

: SHK-FALSE ( -- bool )
   0 0= 0= ;

: SHK-FLAG@ ( ptr bool -- bool )
   @ ;

: SHK-TOK= ( ptr u8 ptr u8 n -- bool ) {: p a u :}
   u 0 ?do  p i + c@ SHK-LC  a i + c@  = 0= IF unloop 0 0= 0= EXIT THEN  loop  0 0= ;

: KEEP? ( ptr u8 n -- bool ) {: a u :}
   SHAKE? @ 0 = IF 0 0= EXIT THEN
   0 SKP !
   BEGIN SKP @ SHK-U @ < WHILE
      SHK-A@ SKP @ + c@ 33 < IF
         SKP @ 1 + SKP !
      ELSE
         SKP @ STS !
         BEGIN SKP @ SHK-U @ < IF SHK-A@ SKP @ + c@ 32 > ELSE 0 0= 0= THEN WHILE
            SKP @ 1 + SKP ! REPEAT
         SKP @ STS @ - u = IF
            SHK-A@ STS @ +  a u SHK-TOK= IF 0 0= EXIT THEN THEN
      THEN
   REPEAT 0 0= 0= ;

\ --- call-graph reachability (for AOT). A word is kept only when it is
\ REACHABLE from the roots through the program's definition call graph — not
\ merely named somewhere (textual KEEP? above over-keeps dead defs + names in
\ comments/strings). SOUND only when the program is compiled ONCE at build time
\ (AOT): the embed/REPL builds recompile their baked source at startup and must
\ keep every NAMED word, so they stay on KEEP?. Roots: top-level executed
\ tokens (SHK-TOPLEVEL) or a named entry like MAIN (SHK-FROM). REACH holds the
\ kept names, case-folded + space-joined; IN-REACH? tests membership.
create REACHBUF 65536 allot
variable REACHN  variable TKP   variable CHG
variable INDEF   variable XNAME variable KEEPCUR
variable RSP     variable RTS   variable TA    variable TU
s" REACHN" s" -- ptr n" TRUST
s" TKP" s" -- ptr n" TRUST
s" CHG" s" -- ptr bool" TRUST
s" INDEF" s" -- ptr bool" TRUST
s" XNAME" s" -- ptr bool" TRUST
s" KEEPCUR" s" -- ptr bool" TRUST
s" RSP" s" -- ptr n" TRUST
s" RTS" s" -- ptr n" TRUST
s" TA" s" -- ptr ptr u8" TRUST
s" TU" s" -- ptr n" TRUST
: TA@ ( -- ptr u8 )
   TA @ ;
s" TA@" s" -- ptr u8" TRUST

: NMF= ( ptr u8 ptr u8 n -- bool ) {: s a u :}
   u 0 ?do  s i + c@  a i + c@ SHK-LC  = 0= IF unloop 0 0= 0= EXIT THEN  loop  0 0= ;

: IN-REACH? ( ptr u8 n -- bool ) {: a u :}
   0 RSP !
   BEGIN RSP @ REACHN @ < WHILE
      REACHBUF RSP @ + c@ 33 < IF RSP @ 1+ RSP ! ELSE
         RSP @ RTS !
         BEGIN RSP @ REACHN @ < IF REACHBUF RSP @ + c@ 32 > ELSE 0 0= 0= THEN WHILE RSP @ 1+ RSP ! REPEAT
         RSP @ RTS @ - u = IF REACHBUF RTS @ + a u NMF= IF 0 0= EXIT THEN THEN
      THEN
   REPEAT 0 0= 0= ;

: ADD-REACH ( ptr u8 n -- ) {: a u :}
   a u IN-REACH? IF EXIT THEN
   u 0 ?do  a i + c@ SHK-LC  REACHBUF REACHN @ + c!  REACHN @ 1+ REACHN !  loop
   32 REACHBUF REACHN @ + c!  REACHN @ 1+ REACHN !  SHK-TRUE CHG ! ;

: SKIP-PAST ( n -- ) {: ch :}
   BEGIN TKP @ SHK-U @ < WHILE
      SHK-A@ TKP @ + c@  TKP @ 1+ TKP !  ch = IF EXIT THEN REPEAT ;

: OPN2? ( ptr u8 n n -- bool ) {: a u c0 :}
   u 2 = a c@ c0 = and a 1+ c@ 34 = and ;

: NEXT-TOK ( -- a u )                 \ next word token; 0 0 at end; skips \ ( s" ."
   BEGIN
      BEGIN TKP @ SHK-U @ < IF SHK-A@ TKP @ + c@ 33 < ELSE 0 0= 0= THEN WHILE TKP @ 1+ TKP ! REPEAT
      TKP @ SHK-U @ < 0= IF SHK-A@ 0 EXIT THEN
      SHK-A@ TKP @ +  TKP @
      BEGIN TKP @ SHK-U @ < IF SHK-A@ TKP @ + c@ 32 > ELSE 0 0= 0= THEN WHILE TKP @ 1+ TKP ! REPEAT
      TKP @ swap -
      2dup 1 = swap c@ 92 = and IF 2drop 10 SKIP-PAST ELSE
      2dup 1 = swap c@ 40 = and IF 2drop 41 SKIP-PAST ELSE
      2dup 115 OPN2? IF 2drop 34 SKIP-PAST ELSE
      2dup 46 OPN2? IF 2drop 34 SKIP-PAST ELSE
         EXIT
      THEN THEN THEN THEN
   AGAIN ;

\ one walk of the program. mode 0: add top-level (root) tokens. mode 1: for each
\ definition whose name is already in REACH, add its body tokens (one expansion).
: SCAN ( n -- ) {: mode :}
   0 TKP ! SHK-FALSE INDEF ! SHK-FALSE XNAME ! SHK-FALSE KEEPCUR !
   BEGIN
      NEXT-TOK TU ! TA !
      TU @ 0= IF EXIT THEN
      TU @ 1 = TA@ c@ 58 = and IF SHK-TRUE XNAME ! SHK-TRUE INDEF !
      ELSE XNAME SHK-FLAG@ IF TA@ TU @ IN-REACH? KEEPCUR ! SHK-FALSE XNAME !
      ELSE TU @ 1 = TA@ c@ 59 = and IF SHK-FALSE INDEF ! SHK-FALSE KEEPCUR !
      ELSE INDEF SHK-FLAG@ IF mode 1 = KEEPCUR SHK-FLAG@ and IF TA@ TU @ ADD-REACH THEN
      ELSE mode 0= IF TA@ TU @ ADD-REACH THEN
      THEN THEN THEN THEN
   AGAIN ;

: SHK-CLOSE ( -- )
   BEGIN SHK-FALSE CHG ! 1 SCAN CHG @ 0= UNTIL ;

: SHK-TOPLEVEL ( -- )
   0 REACHN !  0 SCAN  SHK-CLOSE ;

: SHK-FROM ( ptr u8 n -- ) {: a u :}
   0 REACHN !  a u ADD-REACH  SHK-CLOSE ;
