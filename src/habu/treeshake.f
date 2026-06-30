\ treeshake.f — THE tree shaker. With SHAKE? on (hb-build's maker), a prim is
\ emitted and seeded ONLY if its name appears as a whitespace token in the
\ user program (SHK-A/SHK-U). Sound over-approximation; default off = keep
\ all. Gates: FPRIM/FPRIM-L (habu1.f) + keyword entries (habu2.f EM-COMPILE);
\ armed by build.f. Load after the shared argv/env prefix, before habu1.f.

variable SHAKE?   PTR-VARIABLE SHK-A   variable SHK-U
variable SKP  variable STS
PTR-VARIABLE SHK-P  PTR-VARIABLE SHK-Q  variable SHK-N  variable SHK-C
PTR-VARIABLE KEEP-A  variable KEEP-U
s" SHAKE?" s" -- ptr n" TRUST
s" SHK-U" s" -- ptr n" TRUST
s" SKP" s" -- ptr n" TRUST
s" STS" s" -- ptr n" TRUST
s" SHK-N" s" -- ptr n" TRUST
s" SHK-C" s" -- ptr n" TRUST
s" KEEP-U" s" -- ptr n" TRUST
: SHK-A@ ( -- ptr u8 )
   SHK-A @ ;
: SHK-P@ ( -- ptr u8 )
   SHK-P @ ;
: SHK-Q@ ( -- ptr u8 )
   SHK-Q @ ;
: KEEP-A@ ( -- ptr u8 )
   KEEP-A @ ;

TRUSTED: SHK-BYTE+ ( ptr u8 n -- ptr u8 ) + ;

: SHK-C@ ( ptr u8 n -- n )
   SHK-BYTE+ c@ ;

: SHK-LC ( c -- c )  dup $40 > over $5B < and IF $20 + THEN ;

: SHK-TRUE ( -- bool )
   0 0= ;

: SHK-FALSE ( -- bool )
   0 0= 0= ;

: SHK-FLAG@ ( ptr bool -- bool )
   @ ;

: SHK-TOK= ( ptr u8 ptr u8 n -- bool )
   SHK-N ! SHK-Q ! SHK-P !
   0 BEGIN dup SHK-N @ < WHILE
      dup SHK-P@ swap SHK-C@ SHK-LC
      over SHK-Q@ swap SHK-C@ = 0= IF drop SHK-FALSE EXIT THEN
      1 +
   REPEAT drop SHK-TRUE ;

: KEEP? ( ptr u8 n -- bool )
   KEEP-U ! KEEP-A !
   SHAKE? @ 0 = IF 0 0= EXIT THEN
   0 SKP !
   BEGIN SKP @ SHK-U @ < WHILE
      SHK-A@ SKP @ SHK-C@ $21 < IF
         SKP @ 1 + SKP !
      ELSE
         SKP @ STS !
         BEGIN SKP @ SHK-U @ < IF SHK-A@ SKP @ SHK-C@ $20 > ELSE 0 0= 0= THEN WHILE
            SKP @ 1 + SKP ! REPEAT
         SKP @ STS @ - KEEP-U @ = IF
            SHK-A@ STS @ SHK-BYTE+  KEEP-A@ KEEP-U @ SHK-TOK= IF 0 0= EXIT THEN THEN
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
create REACHBUF $10000 allot
variable REACHN  variable TKP   variable CHG
variable INDEF   variable XNAME variable KEEPCUR
variable RSP     variable RTS   PTR-VARIABLE TA    variable TU
variable SCAN-MODE
s" REACHN" s" -- ptr n" TRUST
s" TKP" s" -- ptr n" TRUST
s" CHG" s" -- ptr bool" TRUST
s" INDEF" s" -- ptr bool" TRUST
s" XNAME" s" -- ptr bool" TRUST
s" KEEPCUR" s" -- ptr bool" TRUST
s" RSP" s" -- ptr n" TRUST
s" RTS" s" -- ptr n" TRUST
s" TU" s" -- ptr n" TRUST
s" SCAN-MODE" s" -- ptr n" TRUST
: TA@ ( -- ptr u8 )
   TA @ ;

: NMF= ( ptr u8 ptr u8 n -- bool )
   SHK-N ! SHK-Q ! SHK-P !
   0 BEGIN dup SHK-N @ < WHILE
      dup SHK-P@ swap SHK-C@
      over SHK-Q@ swap SHK-C@ SHK-LC = 0= IF drop SHK-FALSE EXIT THEN
      1 +
   REPEAT drop SHK-TRUE ;

: IN-REACH? ( ptr u8 n -- bool )
   KEEP-U ! KEEP-A !
   0 RSP !
   BEGIN RSP @ REACHN @ < WHILE
      REACHBUF RSP @ + c@ $21 < IF RSP @ 1+ RSP ! ELSE
         RSP @ RTS !
         BEGIN RSP @ REACHN @ < IF REACHBUF RSP @ + c@ $20 > ELSE 0 0= 0= THEN WHILE RSP @ 1+ RSP ! REPEAT
         RSP @ RTS @ - KEEP-U @ = IF REACHBUF RTS @ + KEEP-A@ KEEP-U @ NMF= IF 0 0= EXIT THEN THEN
      THEN
   REPEAT 0 0= 0= ;

: ADD-REACH ( ptr u8 n -- )
   KEEP-U ! KEEP-A !
   KEEP-A@ KEEP-U @ IN-REACH? IF EXIT THEN
   0 BEGIN dup KEEP-U @ < WHILE
      KEEP-A@ over SHK-C@ SHK-LC  REACHBUF REACHN @ + c!
      REACHN @ 1+ REACHN !  1 +
   REPEAT drop
   $20 REACHBUF REACHN @ + c!  REACHN @ 1+ REACHN !  SHK-TRUE CHG ! ;

: SKIP-PAST ( n -- )
   SHK-C !
   BEGIN TKP @ SHK-U @ < WHILE
      SHK-A@ TKP @ SHK-C@  TKP @ 1+ TKP !  SHK-C @ = IF EXIT THEN REPEAT ;

: OPN2? ( ptr u8 n n -- bool )
   SHK-C ! KEEP-U ! KEEP-A !
   KEEP-U @ 2 = KEEP-A@ c@ SHK-C @ = and KEEP-A@ 1 SHK-C@ $22 = and ;

: NEXT-TOK ( -- a u )                 \ next word token; 0 0 at end; skips \ ( s" ."
   BEGIN
      BEGIN TKP @ SHK-U @ < IF SHK-A@ TKP @ SHK-C@ $21 < ELSE 0 0= 0= THEN WHILE TKP @ 1+ TKP ! REPEAT
      TKP @ SHK-U @ < 0= IF SHK-A@ 0 EXIT THEN
      SHK-A@ TKP @ SHK-BYTE+  TKP @
      BEGIN TKP @ SHK-U @ < IF SHK-A@ TKP @ SHK-C@ $20 > ELSE 0 0= 0= THEN WHILE TKP @ 1+ TKP ! REPEAT
      TKP @ swap -
      2dup 1 = swap c@ $5C = and IF 2drop $A SKIP-PAST ELSE
      2dup 1 = swap c@ $28 = and IF 2drop $29 SKIP-PAST ELSE
      2dup $73 OPN2? IF 2drop $22 SKIP-PAST ELSE
      2dup $2E OPN2? IF 2drop $22 SKIP-PAST ELSE
         EXIT
      THEN THEN THEN THEN
   AGAIN ;

\ one walk of the program. mode 0: add top-level (root) tokens. mode 1: for each
\ definition whose name is already in REACH, add its body tokens (one expansion).
: SCAN ( n -- )
   SCAN-MODE !
   0 TKP ! SHK-FALSE INDEF ! SHK-FALSE XNAME ! SHK-FALSE KEEPCUR !
   BEGIN
      NEXT-TOK TU ! TA !
      TU @ 0= IF EXIT THEN
      TU @ 1 = TA@ c@ $3A = and IF SHK-TRUE XNAME ! SHK-TRUE INDEF !
      ELSE XNAME SHK-FLAG@ IF TA@ TU @ IN-REACH? KEEPCUR ! SHK-FALSE XNAME !
      ELSE TU @ 1 = TA@ c@ $3B = and IF SHK-FALSE INDEF ! SHK-FALSE KEEPCUR !
      ELSE INDEF SHK-FLAG@ IF SCAN-MODE @ 1 = KEEPCUR SHK-FLAG@ and IF TA@ TU @ ADD-REACH THEN
      ELSE SCAN-MODE @ 0= IF TA@ TU @ ADD-REACH THEN
      THEN THEN THEN THEN
   AGAIN ;

: SHK-CLOSE ( -- )
   BEGIN SHK-FALSE CHG ! 1 SCAN CHG @ 0= UNTIL ;

: SHK-TOPLEVEL ( -- )
   0 REACHN !  0 SCAN  SHK-CLOSE ;

: SHK-FROM ( ptr u8 n -- )
   0 REACHN !  ADD-REACH  SHK-CLOSE ;
