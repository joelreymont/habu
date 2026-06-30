\ clobber-lint.f -- register-clobber analysis for BL-able emitter routines.
\ Self-hosted clobber lint. Run:
\   bin/hb --load tools/lint/text.f tools/lint/token.f tools/lint/lib.f tools/lint/clobber-lint.f

require lib/errors.f
require lib/string.f
require tools/lint/text.f
require tools/lint/token.f
require tools/lint/lib.f

create FB 131072 allot

: NL  ( -- )  10 emit ;

\ ---- register sets --------------------------------------------------------
variable WMSK  variable RMSK
variable RX  variable RACC

: CL-BIT  ( n -- n )  1 swap lshift ;
: CL-HAS?  ( n n -- bool )  CL-BIT and 0 <> ;
: CL-ADD  ( n n -- n )  CL-BIT or ;
: CL-WOR  ( n -- )  WMSK @ or WMSK ! ;
: CL-ROR  ( n -- )  RMSK @ or RMSK ! ;

0 28 CL-ADD 31 CL-ADD constant CONTRACT-MASK
0 0 CL-ADD 2 CL-ADD 3 CL-ADD 4 CL-ADD 5 CL-ADD constant KWCMP-MASK
0 8 CL-ADD 16 CL-ADD constant SYS-SCRATCH-MASK

: CL-DIGIT?  ( n -- bool )  dup 47 > swap 58 < and ;
: CL-NUM-REG  ( ptr u8 n -- n ) {: a:ptr u :}
   u 0= if -1 exit then
   0 RACC !  0 RX !
   begin RX @ u < while
      a RX @ + c@ dup CL-DIGIT? 0= if drop -1 exit then
      48 -  RACC @ 10 * + RACC !
      RX @ 1+ RX !
   repeat
   RACC @ 32 < if RACC @ else -1 then ;
: REG-OF  ( ptr u8 n -- n ) {: a:ptr u :}
   a u s" XDS"   LINT-STR= if 19 exit then
   a u s" SP"    LINT-STR= if 31 exit then
   a u s" A"     LINT-STR= if 9 exit then
   a u s" B"     LINT-STR= if 10 exit then
   a u s" C"     LINT-STR= if 11 exit then
   a u s" XREG-RBASE" LINT-STR= if 20 exit then
   a u s" DBASE" LINT-STR= if 26 exit then
   a u s" NDICT" LINT-STR= if 27 exit then
   a u s" CP"    LINT-STR= if 28 exit then
   a u s" DATA"  LINT-STR= if 20 exit then
   a u CL-NUM-REG ;

\ ---- string/token helpers -------------------------------------------------
: START-L?  ( ptr u8 n -- bool ) {: a:ptr u :}
   u 0 > if a c@ LINT-FOLD 108 = else LINT-FALSE then ;
: ENDS-COMMA?  ( ptr u8 n -- bool ) {: a:ptr u :}
   u 0 > if a u 1- + c@ 44 = else LINT-FALSE then ;
: START-DOLLAR?  ( ptr u8 n -- bool ) {: a:ptr u :}
   u 0 > if a c@ 36 = else LINT-FALSE then ;
: LOWER-CHAR?  ( n -- bool )  dup 96 > swap 123 < and ;
: UPPERISH?  ( ptr u8 n -- bool ) {: a:ptr u :}
   0 RX !
   begin RX @ u < while
      a RX @ + c@ LOWER-CHAR? if LINT-FALSE exit then
      RX @ 1+ RX !
   repeat  LINT-TRUE ;
: STOP-MN?  ( ptr u8 n -- bool ) {: a:ptr u :}
   a u s" RET," LINT-STR= if LINT-TRUE exit then
   a u s" B," LINT-STR= ;

\ ---- modeled maps ---------------------------------------------------------
: RETURNS-MASK  ( ptr u8 n -- n ) {: a:ptr u :}
   a u s" Lcfpop" LINT-STR=CI if 0 9 CL-ADD exit then
   a u s" Lkwcmp" LINT-STR=CI if 0 0 CL-ADD exit then
   a u s" Lloc-find" LINT-STR=CI if 0 0 CL-ADD exit then
   a u s" Ltok" LINT-STR=CI if 0 0 CL-ADD exit then
   a u s" Lsrcrd" LINT-STR=CI if 0 9 CL-ADD exit then
   a u s" Lfind" LINT-STR=CI if 0 11 CL-ADD 12 CL-ADD 13 CL-ADD exit then
   a u s" Lnum" LINT-STR=CI if 0 2 CL-ADD 11 CL-ADD 12 CL-ADD exit then
   a u s" Lvralloc" LINT-STR=CI if 0 14 CL-ADD exit then
   a u s" Lfralloc" LINT-STR=CI if 0 14 CL-ADD exit then
   a u s" Lvpushf" LINT-STR=CI if 0 exit then
   a u s" Lfforcek" LINT-STR=CI if 0 14 CL-ADD exit then
   a u s" Lfbinprep" LINT-STR=CI if 0 13 CL-ADD 14 CL-ADD 15 CL-ADD exit then
   a u s" Lvbit" LINT-STR=CI if 0 8 CL-ADD exit then
   a u s" Lvforcek" LINT-STR=CI if 0 14 CL-ADD exit then
   a u s" Lvtop2c" LINT-STR=CI if 0 11 CL-ADD 12 CL-ADD 13 CL-ADD exit then
   a u s" Lvbinprep" LINT-STR=CI if 0 11 CL-ADD 12 CL-ADD 13 CL-ADD 14 CL-ADD 15 CL-ADD exit then
   a u s" Lvdrop" LINT-STR=CI if 0 13 CL-ADD exit then
   a u s" Lvswapx" LINT-STR=CI if 0 13 CL-ADD exit then
   a u s" Lvnipx" LINT-STR=CI if 0 13 CL-ADD exit then
   a u s" Lvcopy" LINT-STR=CI if 0 13 CL-ADD exit then
   0 ;
: PRESERVE-MASK  ( ptr u8 n -- n ) {: a:ptr u :}
   a u s" Lvpushc" LINT-STR=CI if 0 11 CL-ADD exit then
   a u s" Lvpushr" LINT-STR=CI if 0 14 CL-ADD exit then
   a u s" Lvforcek" LINT-STR=CI if 0 5 CL-ADD exit then
   a u s" Lfforcek" LINT-STR=CI if 0 5 CL-ADD exit then
   a u s" Lvpushf" LINT-STR=CI if 0 11 CL-ADD exit then
   a u s" Lvbit" LINT-STR=CI if 0 7 CL-ADD exit then
   a u s" Lbcap" LINT-STR=CI if 0 0 CL-ADD 1 CL-ADD 2 CL-ADD 16 CL-ADD exit then
   a u s" Lbcs" LINT-STR=CI if 0 0 CL-ADD 1 CL-ADD 2 CL-ADD 16 CL-ADD exit then
   0 ;

: PSEUDO?  ( ptr u8 n -- bool ) {: a:ptr u :}
   a u s" g-push" LINT-STR=CI if LINT-TRUE exit then
   a u s" g-pop" LINT-STR=CI if LINT-TRUE exit then
   a u s" g-print9" LINT-STR=CI if LINT-TRUE exit then
   a u s" c-lit" LINT-STR=CI if LINT-TRUE exit then
   a u s" c-call" LINT-STR=CI if LINT-TRUE exit then
   a u s" c-popflag" LINT-STR=CI if LINT-TRUE exit then
   a u s" c-pushcp" LINT-STR=CI if LINT-TRUE exit then
   a u s" c-emitw" LINT-STR=CI if LINT-TRUE exit then
   a u s" c-bback" LINT-STR=CI if LINT-TRUE exit then
   a u s" cf-entry" LINT-STR=CI if LINT-TRUE exit then
   a u s" cfb-entry" LINT-STR=CI if LINT-TRUE exit then
   a u s" fold-entry" LINT-STR=CI if LINT-TRUE exit then
   a u s" vop-entry" LINT-STR=CI if LINT-TRUE exit then
   a u s" vcmp-entry" LINT-STR=CI if LINT-TRUE exit then
   a u s" vshuf-entry" LINT-STR=CI if LINT-TRUE exit then
   a u s" vun-entry" LINT-STR=CI ;
: INSTR?  ( ptr u8 n -- bool ) {: a:ptr u :}
   a u PSEUDO? if LINT-TRUE exit then
   a u ENDS-COMMA?  a u START-DOLLAR? 0= and  a u UPPERISH? and ;

: MN-W3?  ( ptr u8 n -- bool ) {: a:ptr u :}
   a u s" ADD," LINT-STR= if LINT-TRUE exit then  a u s" SUB," LINT-STR= if LINT-TRUE exit then
   a u s" MUL," LINT-STR= if LINT-TRUE exit then  a u s" AND," LINT-STR= if LINT-TRUE exit then
   a u s" ORR," LINT-STR= if LINT-TRUE exit then  a u s" EOR," LINT-STR= if LINT-TRUE exit then
   a u s" LSLV," LINT-STR= if LINT-TRUE exit then  a u s" LSRV," LINT-STR= if LINT-TRUE exit then
   a u s" SDIV," LINT-STR= ;
: MN-W2I?  ( ptr u8 n -- bool ) {: a:ptr u :}
   a u s" ADDI," LINT-STR= if LINT-TRUE exit then  a u s" SUBI," LINT-STR= if LINT-TRUE exit then
   a u s" LSLI," LINT-STR= if LINT-TRUE exit then  a u s" LSRI," LINT-STR= if LINT-TRUE exit then
   a u s" ASRI," LINT-STR= if LINT-TRUE exit then  a u s" ANDI," LINT-STR= ;
: MN-W1?  ( ptr u8 n -- bool ) {: a:ptr u :}
   a u s" MOVZ," LINT-STR= if LINT-TRUE exit then  a u s" MOVN," LINT-STR= if LINT-TRUE exit then
   a u s" ADR," LINT-STR= if LINT-TRUE exit then  a u s" LIT64," LINT-STR= if LINT-TRUE exit then
   a u s" CSET," LINT-STR= ;
: MN-WRMW?  ( ptr u8 n -- bool ) {: a:ptr u :}
   a u s" MOVK," LINT-STR= if LINT-TRUE exit then  a u s" MOVZHW," LINT-STR= if LINT-TRUE exit then
   a u s" MOVKHW," LINT-STR= if LINT-TRUE exit then  a u s" MOVNHW," LINT-STR= ;
: MN-LD?  ( ptr u8 n -- bool ) {: a:ptr u :}
   a u s" LDR," LINT-STR= if LINT-TRUE exit then  a u s" LDRB," LINT-STR= if LINT-TRUE exit then
   a u s" LDRW," LINT-STR= ;
: MN-ST?  ( ptr u8 n -- bool ) {: a:ptr u :}
   a u s" STR," LINT-STR= if LINT-TRUE exit then  a u s" STRB," LINT-STR= if LINT-TRUE exit then
   a u s" STRW," LINT-STR= ;

\ ---- register extraction/effects -----------------------------------------
16 constant RRMAX
create RRS RRMAX cells allot   variable RR#
variable RK

: RR-CHECK-ROOM ( -- )
   RR# @ RRMAX >= if s" clobber-lint: too many regs in instruction" 1 die then ;
: RR+  ( n -- )
   RR-CHECK-ROOM
   RRS RR# @ cells + !  RR# @ 1+ RR# ! ;
: RR@  ( n -- n )
   dup RR# @ < if RRS swap cells + @ else drop -1 then ;
: COLLECT-REGS  ( n n -- ) {: lo hi :}
   0 RR# !  lo RK !
   begin RK @ hi < while
      RK @ TOK REG-OF dup 0 >= if RR+ else drop then
      RK @ 1+ RK !
   repeat ;
: EW  ( n -- )  RR@ dup 0 >= if WMSK @ swap CL-ADD WMSK ! else drop then ;
: ER  ( n -- )  RR@ dup 0 >= if RMSK @ swap CL-ADD RMSK ! else drop then ;

: PSEUDO-EFFECTS  {: a u :}  ( -- )
   a u s" g-push" LINT-STR=CI if 0 ER 19 ER 19 EW exit then
   a u s" g-pop" LINT-STR=CI if 0 EW 19 ER 19 EW exit then
   a u s" g-print9" LINT-STR=CI if 0 0 CL-ADD 1 CL-ADD 2 CL-ADD 16 CL-ADD CL-WOR  0 9 CL-ADD CL-ROR exit then
   a u s" c-lit" LINT-STR=CI if 0 5 CL-ADD 6 CL-ADD 7 CL-ADD 8 CL-ADD 9 CL-ADD 30 CL-ADD CL-WOR  0 11 CL-ADD CL-ROR exit then
   a u s" c-call" LINT-STR=CI if 0 5 CL-ADD 7 CL-ADD 8 CL-ADD 9 CL-ADD 10 CL-ADD 13 CL-ADD 14 CL-ADD 15 CL-ADD 30 CL-ADD CL-WOR  0 11 CL-ADD 12 CL-ADD CL-ROR exit then
   a u s" c-popflag" LINT-STR=CI if 0 9 CL-ADD 19 CL-ADD CL-WOR exit then
   a u s" c-pushcp" LINT-STR=CI if 0 9 CL-ADD 30 CL-ADD CL-WOR exit then
   a u s" c-emitw" LINT-STR=CI if 0 9 CL-ADD 30 CL-ADD CL-WOR exit then
   a u s" c-bback" LINT-STR=CI if 0 5 CL-ADD 9 CL-ADD 10 CL-ADD 30 CL-ADD CL-WOR  0 9 CL-ADD CL-ROR exit then
   a u s" cf-entry" LINT-STR=CI if KWCMP-MASK CL-WOR exit then
   a u s" cfb-entry" LINT-STR=CI if KWCMP-MASK CL-WOR exit then
   a u s" fold-entry" LINT-STR=CI if KWCMP-MASK CL-WOR exit then
   a u s" vop-entry" LINT-STR=CI if KWCMP-MASK CL-WOR exit then
   a u s" vcmp-entry" LINT-STR=CI if KWCMP-MASK CL-WOR exit then
   a u s" vshuf-entry" LINT-STR=CI if KWCMP-MASK CL-WOR exit then
   a u s" vun-entry" LINT-STR=CI if KWCMP-MASK CL-WOR exit then ;

: SYS-EXIT?  ( n n -- bool ) {: lo hi :}
   hi lo <= if LINT-FALSE exit then
   hi 1- TOK s" NR-EXIT" LINT-STR= ;
: SYS?  ( ptr u8 n -- bool )
   s" SYS," LINT-STR= ;
: BLR?  ( ptr u8 n -- bool )
   s" BLR," LINT-STR= ;

: EFFECTS  {: a u lo hi :}  ( -- )
   0 WMSK !  0 RMSK !  lo hi COLLECT-REGS
   a u MN-W3? if 0 EW 1 ER 2 ER exit then
   a u MN-W2I? if 0 EW 1 ER exit then
   a u MN-W1? if 0 EW exit then
   a u MN-WRMW? if RR# @ 0 > if 0 EW 0 ER then exit then
   a u MN-LD? if 0 EW 1 ER exit then
   a u MN-ST? if 0 ER 1 ER exit then
   a u s" CMP," LINT-STR= if 0 ER 1 ER exit then
   a u s" CMPI," LINT-STR= if 0 ER exit then
   a u s" CBZ," LINT-STR= if 0 ER exit then
   a u s" CBNZ," LINT-STR= if 0 ER exit then
   a u s" SVC," LINT-STR= if 0 0 CL-ADD CL-WOR  0 0 CL-ADD 1 CL-ADD 2 CL-ADD 16 CL-ADD CL-ROR exit then
   a u s" SYS," LINT-STR= if
      lo hi SYS-EXIT? if 0 0 CL-ADD 8 CL-ADD 16 CL-ADD CL-WOR  0 0 CL-ADD CL-ROR exit then
      0 0 CL-ADD 8 CL-ADD 16 CL-ADD CL-WOR  0 0 CL-ADD 1 CL-ADD 2 CL-ADD CL-ROR exit
   then
   a u s" RET," LINT-STR= if 0 30 CL-ADD CL-ROR exit then
   a u s" BLR," LINT-STR= if 1 18 lshift 1 -  30 CL-ADD CL-WOR  0 ER exit then
   a u PSEUDO? if a u PSEUDO-EFFECTS then ;

\ ---- clobber table + BL graph --------------------------------------------
$100 constant CMAX
$400 constant EMAX
8192 constant CNBUF-CAP
create CNBUF CNBUF-CAP allot   variable CEND
create CNOFF CMAX cells allot   create CNLEN CMAX cells allot
create CWS CMAX cells allot     variable CN#
create EFROM EMAX cells allot   create ETO EMAX cells allot   variable EN#
variable CX  variable EX

: C-NAME  ( n -- ptr u8 n )
   dup CNOFF swap cells + @  swap CNLEN swap cells + @ ;
: CWS@  ( n -- n )  CWS swap cells + @ ;
: CWS!  ( n n -- )  CWS swap cells + ! ;
: C-WOR  ( n n -- ) {: idx m :}  idx CWS@ m or idx CWS! ;
: C-FIND  ( ptr u8 n -- n ) {: a:ptr u :}
   0 CX !
   begin CX @ CN# @ < while
      CX @ C-NAME a u LINT-STR=CI if CX @ exit then
      CX @ 1+ CX !
   repeat  -1 ;
: C-ADD  {: a u :}  ( -- idx )
   CN# @ CMAX >= if s" clobber-lint: too many labels" 1 die then
   CEND @ u + CNBUF-CAP > if s" clobber-lint: label store full" 1 die then
   a u CNBUF CEND @ + FOLD-TO
   CNBUF CEND @ + CNOFF CN# @ cells + !  u CNLEN CN# @ cells + !
   0 CWS CN# @ cells + !
   CEND @ u + CEND !  CN# @ dup 1+ CN# ! ;
: C-ENSURE  {: a u :}  ( -- idx )
   a u C-FIND dup 0 >= if exit then
   drop a u C-ADD ;
: EDGE?  ( n n -- bool ) {: from to :}
   0 EX !
   begin EX @ EN# @ < while
      EX @ cells EFROM + @ from =  EX @ cells ETO + @ to = and if LINT-TRUE exit then
      EX @ 1+ EX !
   repeat  LINT-FALSE ;
: EDGE+  {: from to :}  ( -- )
   from to EDGE? if exit then
   EN# @ EMAX >= if s" clobber-lint: too many BL edges" 1 die then
   from EFROM EN# @ cells + !  to ETO EN# @ cells + !
   EN# @ 1+ EN# ! ;

\ ---- definitions, labels, and routine regions ----------------------------
$80 constant OMAX
create OPENINGS OMAX cells allot   variable ON#
variable DI  variable OX  variable OPLO  variable CALA  variable CALU
variable RNEXT  variable LASTSTOP  variable RDONE  variable CUR

: DEF-END  {: lo :}  ( -- hi )
   lo 2 + DI !
   begin DI @ TN# @ < while
      DI @ TOK s" ;" LINT-STR= if DI @ exit then
      DI @ 1+ DI !
   repeat  DI @ ;
: OPEN@  ( n -- n )  OPENINGS swap cells + @ ;
: OPEN+  ( n -- )
   ON# @ OMAX >= if s" clobber-lint: too many labels in definition" 1 die then
   OPENINGS ON# @ cells + !  ON# @ 1+ ON# ! ;
: LABEL-OPEN?  {: k hi :}  ( -- f )
   k 2 + hi >= if LINT-FALSE exit then
   k TOK START-L?  k 1+ TOK s" @" LINT-STR= and  k 2 + TOK s" LBL," LINT-STR= and ;
: COLLECT-OPENINGS  {: lo hi :}  ( -- )
   0 ON# !  lo OX !
   begin OX @ hi < while
      OX @ hi LABEL-OPEN? if OX @ OPEN+ then
      OX @ 1+ OX !
   repeat ;
: CALLEE?  {: lo hi :}  ( -- f )
   hi lo - 2 < if LINT-FALSE exit then
   hi 1- TOK s" @" LINT-STR= 0= if LINT-FALSE exit then
   hi 2 - TOK START-L? 0= if LINT-FALSE exit then
   hi 2 - TOK  CALU ! CALA !  LINT-TRUE ;

: ROUTINE-INIT ( n -- ) {: oi :}
   oi OPEN@ 3 + DI !
   oi 1+ RNEXT !
   0 LASTSTOP !
   0 RDONE !
   DI @ OPLO ! ;

: ROUTINE-NEXT-OPEN? ( -- bool )
   RNEXT @ ON# @ <  DI @ RNEXT @ OPEN@ = and ;

: ROUTINE-ADVANCE-OPEN ( -- )
   LASTSTOP @ if
      -1 RDONE !
   else
      RNEXT @ 1+ RNEXT !
      DI @ 3 + DI !
      DI @ OPLO !
   then ;

: ROUTINE-CALL? ( -- bool )
   OPLO @ DI @ CALLEE?
   DI @ TOK s" BL," LINT-STR= and ;

: ROUTINE-ADD-EDGE ( n -- ) {: cidx :}
   CALA @ CALU @ C-ENSURE cidx swap EDGE+ ;

: ROUTINE-ADD-EFFECTS ( n -- ) {: cidx :}
   DI @ TOK OPLO @ DI @ EFFECTS
   cidx WMSK @ C-WOR ;

: ROUTINE-INSTR ( n -- ) {: cidx :}
   ROUTINE-CALL? if
      cidx ROUTINE-ADD-EDGE
   else
      cidx ROUTINE-ADD-EFFECTS
   then
   DI @ TOK STOP-MN? LASTSTOP !
   DI @ 1+ OPLO ! ;

: ROUTINE-STEP ( n -- ) {: cidx :}
   ROUTINE-NEXT-OPEN? if
      ROUTINE-ADVANCE-OPEN
   else
      DI @ TOK INSTR? if cidx ROUTINE-INSTR then
      DI @ 1+ DI !
   then ;

: ROUTINE-SCAN ( n n n -- ) {: cidx oi hi :}
   oi ROUTINE-INIT
   begin DI @ hi < RDONE @ 0= and while
      cidx ROUTINE-STEP
   repeat ;
: PASS1-DEF  {: lo hi :}  ( -- )
   lo hi COLLECT-OPENINGS
   0 OX !
   begin OX @ ON# @ < while
      OX @ OPEN@ TOK C-ENSURE CUR !
      CUR @ OX @ hi ROUTINE-SCAN
      OX @ 1+ OX !
   repeat ;

variable WI  variable WE
: PASS1-FILE  {: pa pu :}  ( -- )
   pa pu FB 131072 READ-FILE  TOKENIZE
   0 WI !
   begin WI @ TN# @ 1- < while
      WI @ TOK s" :" LINT-STR= if
         WI @ DEF-END WE !
         WI @ 2 + WE @ PASS1-DEF
         WE @ WI !
      then
      WI @ 1+ WI !
   repeat ;

\ ---- clobber closure ------------------------------------------------------
variable CHANGED  variable EFF
: CLOSE-CLOBBERS  ( -- )
   -1 CHANGED !
   begin CHANGED @ while
      0 CHANGED !  0 CX !
      begin CX @ CN# @ < while
         CX @ CWS@ EFF !
         0 EX !
         begin EX @ EN# @ < while
            EX @ cells EFROM + @ CX @ = if
               EFF @  EX @ cells ETO + @ CWS@  or EFF !
            then
            EX @ 1+ EX !
         repeat
         EFF @  CX @ C-NAME PRESERVE-MASK invert and EFF !
         EFF @ CX @ CWS@ <> if EFF @ CX @ CWS!  -1 CHANGED ! then
         CX @ 1+ CX !
      repeat
   repeat ;

\ ---- pass 2: call-site liveness ------------------------------------------
create POIS 32 cells allot   variable DIRTY
variable APPLY-MASK
create WNAME 128 allot       variable WLEN
create NUMBUF 2 allot
variable BAD  variable PR  variable CW  variable RETS  variable CALIDX
variable TRACK-LR

: POIS@ ( n -- n )  POIS swap cells + @ ;
: POIS! ( n n -- )  POIS swap cells + ! ;
: POIS-CLEAR  ( -- )
   0 PR !
   begin PR @ 32 < while -1 PR @ POIS!  PR @ 1+ PR ! repeat ;
: WORD-NAME!  ( n -- )
   TOK dup WLEN !  WNAME FOLD-TO ;
: CRASH-FILE?  ( ptr u8 n -- bool ) {: a:ptr u :}
   a u s" src/habu/crash.f" LINT-STR= ;
: ALLOW?  ( ptr u8 n n n -- bool ) {: fa:ptr fu reg cidx :}
   fa fu CRASH-FILE? 0= if LINT-FALSE exit then
   WNAME WLEN @ s" emit-crash-handler" LINT-STR=CI 0= if LINT-FALSE exit then
   reg 1 = reg 2 = or 0= if LINT-FALSE exit then
   cidx 0 < if LINT-FALSE exit then
   cidx C-NAME s" lhex" LINT-STR=CI ;
: DEC-TYPE  ( n -- ) {: n :}
   n 10 < if 48 n + NUMBUF c!  NUMBUF 1 type
   else 48 n 10 / + NUMBUF c!  48 n 10 mod + NUMBUF 1+ c!  NUMBUF 2 type then ;
: REG-TYPE  ( n -- )  s" x" type DEC-TYPE ;
: FINDING  {: fa fu reg cidx :}  ( -- )
   s" CLOBBER " type fa fu type s"  " type
   WNAME WLEN @ type s" : " type reg REG-TYPE
   s"  written, clobbered by " type cidx C-NAME type
   s" , then read" type NL ;
: NOTE-READS  {: fa fu rmask :}  ( -- )
   0 PR !
   begin PR @ 32 < while
      rmask CONTRACT-MASK invert and PR @ CL-HAS? if
         PR @ 30 = TRACK-LR @ 0= and if
         else
         PR @ POIS@ CALIDX !
         CALIDX @ 0 >= if
            DIRTY @ PR @ CL-HAS? if
               fa fu PR @ CALIDX @ ALLOW? 0= if
                  fa fu PR @ CALIDX @ FINDING  BAD @ 1+ BAD !
               then
               -1 PR @ POIS!
            then
         then
         then
      then
      PR @ 1+ PR !
   repeat ;
: APPLY-WRITES  ( n -- )
   CONTRACT-MASK invert and APPLY-MASK !
   APPLY-MASK @ DIRTY @ or DIRTY !
   0 PR !
   begin PR @ 32 < while
      APPLY-MASK @ PR @ CL-HAS? if -1 PR @ POIS! then
      PR @ 1+ PR !
   repeat ;
: APPLY-RETURNS  ( n -- )
   APPLY-MASK !
   APPLY-MASK @ DIRTY @ or DIRTY !
   0 PR !
   begin PR @ 32 < while
      APPLY-MASK @ PR @ CL-HAS? if -1 PR @ POIS! then
      PR @ 1+ PR !
   repeat ;
: POISON-DIRTY  {: cmask cidx :}  ( -- )
   0 PR !
   begin PR @ 32 < while
      DIRTY @ PR @ CL-HAS?  cmask PR @ CL-HAS? and if
         cidx PR @ POIS!
      then
      PR @ 1+ PR !
   repeat ;
: POISON-SYS-SCRATCH  ( -- )
   SYS-SCRATCH-MASK s" sys" C-ENSURE POISON-DIRTY ;
: POISON-LINK-REGISTER  ( n -- )
   TRACK-LR @ 0= if drop exit then
   0 30 CL-ADD swap POISON-DIRTY ;
: RET-IN-RANGE?  {: lo hi :}  ( -- bool )
   lo DI !
   begin DI @ hi < while
      DI @ TOK s" RET," LINT-STR= if LINT-TRUE exit then
      DI @ 1+ DI !
   repeat
   LINT-FALSE ;

: PASS2-DEF  {: fa fu lo hi :}  ( -- )
   lo hi RET-IN-RANGE? TRACK-LR !
   TRACK-LR @ if 0 30 CL-ADD else 0 then DIRTY !  POIS-CLEAR  lo OPLO !  lo DI !
   begin DI @ hi < while
      DI @ TOK INSTR? if
         OPLO @ DI @ CALLEE?  DI @ TOK s" BL," LINT-STR= and if
            CALA @ CALU @ RETURNS-MASK RETS !
            CALA @ CALU @ C-FIND CALIDX !
            CALIDX @ 0 >= if CALIDX @ CWS@ else 0 then
            CONTRACT-MASK invert and  RETS @ invert and CW !
            CW @ CALIDX @ POISON-DIRTY
            CALIDX @ POISON-LINK-REGISTER
            RETS @ APPLY-RETURNS
         else
            DI @ TOK  OPLO @ DI @ EFFECTS
            fa fu RMSK @ NOTE-READS
            WMSK @ APPLY-WRITES
            DI @ TOK SYS? OPLO @ DI @ SYS-EXIT? 0= and if POISON-SYS-SCRATCH then
            DI @ TOK BLR? if s" blr" C-ENSURE POISON-LINK-REGISTER then
         then
         DI @ 1+ OPLO !
      then
      DI @ 1+ DI !
   repeat ;
: PASS2-FILE  {: pa pu :}  ( -- )
   pa pu FB 131072 READ-FILE  TOKENIZE
   0 WI !
   begin WI @ TN# @ 1- < while
      WI @ TOK s" :" LINT-STR= if
         WI @ DEF-END WE !
         WI @ 1+ WORD-NAME!
         pa pu  WI @ 2 + WE @ PASS2-DEF
         WE @ WI !
      then
      WI @ 1+ WI !
   repeat ;

\ ---- driver ---------------------------------------------------------------
: ALL-PASS1  ( -- )
   0 CN# !  0 CEND !  0 EN# !
   s" src/habu/habu1.f" PASS1-FILE  s" src/habu/habu2.f" PASS1-FILE
   s" src/habu/jit.f" PASS1-FILE  s" src/habu/regalloc.f" PASS1-FILE
   s" src/habu/prof.f" PASS1-FILE  s" src/habu/rt.f" PASS1-FILE
   s" src/habu/crash.f" PASS1-FILE ;
: ALL-PASS2  ( -- )
   s" src/habu/habu1.f" PASS2-FILE  s" src/habu/habu2.f" PASS2-FILE
   s" src/habu/jit.f" PASS2-FILE  s" src/habu/regalloc.f" PASS2-FILE
   s" src/habu/prof.f" PASS2-FILE  s" src/habu/rt.f" PASS2-FILE
   s" src/habu/crash.f" PASS2-FILE ;
: CLOBBER-LINT  ( -- )
   0 PARENS? !  ALL-PASS1  CLOSE-CLOBBERS  0 BAD !  ALL-PASS2
   BAD @ 0 > if
      s" clobber-lint: " type BAD @ . s"  finding(s)" type NL
      s" clobber-lint: findings" 1 die
   else
      s" clobber-lint: clean" type NL
   then ;
CLOBBER-LINT
