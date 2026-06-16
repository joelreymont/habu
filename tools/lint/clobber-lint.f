\ clobber-lint.f -- register-clobber analysis for BL-able emitter routines.
\ Self-hosted clobber lint. Run:
\   cat tools/lint/lib.f tools/lint/clobber-lint.f | bin/hb

create FB 131072 allot

: NL  ( -- )  10 emit ;

\ ---- register sets --------------------------------------------------------
variable WMSK  variable RMSK
variable RX  variable RACC

: CL-BIT  ( r -- m )  1 swap lshift ;
: CL-HAS?  ( m r -- f )  CL-BIT and 0 <> ;
: CL-ADD  ( m r -- m' )  CL-BIT or ;
: CL-WOR  ( m -- )  WMSK @ or WMSK ! ;
: CL-ROR  ( m -- )  RMSK @ or RMSK ! ;

0 28 CL-ADD 30 CL-ADD 31 CL-ADD constant CONTRACT-MASK
0 0 CL-ADD 2 CL-ADD 3 CL-ADD 4 CL-ADD 5 CL-ADD constant KWCMP-MASK

: CL-DIGIT?  ( c -- f )  dup 47 > swap 58 < and ;
: CL-NUM-REG  {: a u :}  ( -- r|-1 )
   u 0= if -1 exit then
   0 RACC !  0 RX !
   begin RX @ u < while
      a RX @ + c@ dup CL-DIGIT? 0= if drop -1 exit then
      48 -  RACC @ 10 * + RACC !
      RX @ 1+ RX !
   repeat
   RACC @ 32 < if RACC @ else -1 then ;
: REG-OF  {: a u :}  ( -- r|-1 )
   a u s" XDS"   STR= if 19 exit then
   a u s" SP"    STR= if 31 exit then
   a u s" A"     STR= if 9 exit then
   a u s" B"     STR= if 10 exit then
   a u s" C"     STR= if 11 exit then
   a u s" RBASE" STR= if 20 exit then
   a u s" DBASE" STR= if 26 exit then
   a u s" NDICT" STR= if 27 exit then
   a u s" CP"    STR= if 28 exit then
   a u s" DATA"  STR= if 20 exit then
   a u CL-NUM-REG ;

\ ---- string/token helpers -------------------------------------------------
: START-L?  {: a u :}  ( -- f )  u 0 > if a c@ FOLD 108 = else 0 then ;
: ENDS-COMMA?  {: a u :}  ( -- f )  u 0 > if a u 1- + c@ 44 = else 0 then ;
: START-DOLLAR?  {: a u :}  ( -- f )  u 0 > if a c@ 36 = else 0 then ;
: LOWER-CHAR?  ( c -- f )  dup 96 > swap 123 < and ;
: UPPERISH?  {: a u :}  ( -- f )
   0 RX !
   begin RX @ u < while
      a RX @ + c@ LOWER-CHAR? if 0 exit then
      RX @ 1+ RX !
   repeat  -1 ;
: STOP-MN?  {: a u :}  ( -- f )
   a u s" RET," STR= if -1 exit then
   a u s" B," STR= ;

\ ---- modeled maps ---------------------------------------------------------
: RETURNS-MASK  {: a u :}  ( -- m )
   a u s" Lcfpop" STR=CI if 0 9 CL-ADD exit then
   a u s" Lkwcmp" STR=CI if 0 0 CL-ADD exit then
   a u s" Lloc-find" STR=CI if 0 0 CL-ADD exit then
   a u s" Ltok" STR=CI if 0 0 CL-ADD exit then
   a u s" Lfind" STR=CI if 0 11 CL-ADD 12 CL-ADD 13 CL-ADD exit then
   a u s" Lnum" STR=CI if 0 2 CL-ADD 11 CL-ADD 12 CL-ADD exit then
   a u s" Lvralloc" STR=CI if 0 14 CL-ADD exit then
   a u s" Lfralloc" STR=CI if 0 14 CL-ADD exit then
   a u s" Lvpushf" STR=CI if 0 exit then
   a u s" Lfforcek" STR=CI if 0 14 CL-ADD exit then
   a u s" Lfbinprep" STR=CI if 0 13 CL-ADD 14 CL-ADD 15 CL-ADD exit then
   a u s" Lvbit" STR=CI if 0 8 CL-ADD exit then
   a u s" Lvforcek" STR=CI if 0 14 CL-ADD exit then
   a u s" Lvtop2c" STR=CI if 0 11 CL-ADD 12 CL-ADD 13 CL-ADD exit then
   a u s" Lvbinprep" STR=CI if 0 11 CL-ADD 12 CL-ADD 13 CL-ADD 14 CL-ADD 15 CL-ADD exit then
   a u s" Lvdrop" STR=CI if 0 13 CL-ADD exit then
   a u s" Lvswapx" STR=CI if 0 13 CL-ADD exit then
   a u s" Lvnipx" STR=CI if 0 13 CL-ADD exit then
   a u s" Lvcopy" STR=CI if 0 13 CL-ADD exit then
   0 ;
: PRESERVE-MASK  {: a u :}  ( -- m )
   a u s" Lvpushc" STR=CI if 0 11 CL-ADD exit then
   a u s" Lvpushr" STR=CI if 0 14 CL-ADD exit then
   a u s" Lvforcek" STR=CI if 0 5 CL-ADD exit then
   a u s" Lfforcek" STR=CI if 0 5 CL-ADD exit then
   a u s" Lvpushf" STR=CI if 0 11 CL-ADD exit then
   a u s" Lvbit" STR=CI if 0 7 CL-ADD exit then
   a u s" Lbcap" STR=CI if 0 0 CL-ADD 1 CL-ADD 2 CL-ADD 16 CL-ADD exit then
   a u s" Lbcs" STR=CI if 0 0 CL-ADD 1 CL-ADD 2 CL-ADD 16 CL-ADD exit then
   0 ;

: PSEUDO?  {: a u :}  ( -- f )
   a u s" g-push" STR=CI if -1 exit then
   a u s" g-pop" STR=CI if -1 exit then
   a u s" g-print9" STR=CI if -1 exit then
   a u s" c-lit" STR=CI if -1 exit then
   a u s" c-call" STR=CI if -1 exit then
   a u s" c-popflag" STR=CI if -1 exit then
   a u s" c-pushcp" STR=CI if -1 exit then
   a u s" c-emitw" STR=CI if -1 exit then
   a u s" c-bback" STR=CI if -1 exit then
   a u s" cf-entry" STR=CI if -1 exit then
   a u s" cfb-entry" STR=CI if -1 exit then
   a u s" fold-entry" STR=CI if -1 exit then
   a u s" vop-entry" STR=CI if -1 exit then
   a u s" vcmp-entry" STR=CI if -1 exit then
   a u s" vshuf-entry" STR=CI if -1 exit then
   a u s" vun-entry" STR=CI ;
: INSTR?  {: a u :}  ( -- f )
   a u PSEUDO? if -1 exit then
   a u ENDS-COMMA?  a u START-DOLLAR? 0= and  a u UPPERISH? and ;

: MN-W3?  {: a u :}  ( -- f )
   a u s" ADD," STR= if -1 exit then  a u s" SUB," STR= if -1 exit then
   a u s" MUL," STR= if -1 exit then  a u s" AND," STR= if -1 exit then
   a u s" ORR," STR= if -1 exit then  a u s" EOR," STR= if -1 exit then
   a u s" LSLV," STR= if -1 exit then  a u s" LSRV," STR= if -1 exit then
   a u s" SDIV," STR= ;
: MN-W2I?  {: a u :}  ( -- f )
   a u s" ADDI," STR= if -1 exit then  a u s" SUBI," STR= if -1 exit then
   a u s" LSLI," STR= if -1 exit then  a u s" LSRI," STR= if -1 exit then
   a u s" ASRI," STR= if -1 exit then  a u s" ANDI," STR= ;
: MN-W1?  {: a u :}  ( -- f )
   a u s" MOVZ," STR= if -1 exit then  a u s" MOVN," STR= if -1 exit then
   a u s" ADR," STR= if -1 exit then  a u s" LIT64," STR= if -1 exit then
   a u s" CSET," STR= ;
: MN-WRMW?  {: a u :}  ( -- f )
   a u s" MOVK," STR= if -1 exit then  a u s" MOVZHW," STR= if -1 exit then
   a u s" MOVKHW," STR= if -1 exit then  a u s" MOVNHW," STR= ;
: MN-LD?  {: a u :}  ( -- f )
   a u s" LDR," STR= if -1 exit then  a u s" LDRB," STR= if -1 exit then
   a u s" LDRW," STR= ;
: MN-ST?  {: a u :}  ( -- f )
   a u s" STR," STR= if -1 exit then  a u s" STRB," STR= if -1 exit then
   a u s" STRW," STR= ;

\ ---- register extraction/effects -----------------------------------------
16 constant RRMAX
create RRS RRMAX cells allot   variable RR#
variable RK

: RR+  ( r -- )
   RR# @ RRMAX >= if s" clobber-lint: too many regs in instruction" type NL 1 die then
   RRS RR# @ cells + !  RR# @ 1+ RR# ! ;
: RR@  ( k -- r|-1 )
   dup RR# @ < if cells RRS + @ else drop -1 then ;
: COLLECT-REGS  {: lo hi :}  ( -- )
   0 RR# !  lo RK !
   begin RK @ hi < while
      RK @ TOK REG-OF dup 0 >= if RR+ else drop then
      RK @ 1+ RK !
   repeat ;
: EW  ( k -- )  RR@ dup 0 >= if WMSK @ swap CL-ADD WMSK ! else drop then ;
: ER  ( k -- )  RR@ dup 0 >= if RMSK @ swap CL-ADD RMSK ! else drop then ;

: PSEUDO-EFFECTS  {: a u :}  ( -- )
   a u s" g-push" STR=CI if 0 ER 19 ER 19 EW exit then
   a u s" g-pop" STR=CI if 0 EW 19 ER 19 EW exit then
   a u s" g-print9" STR=CI if 0 0 CL-ADD 1 CL-ADD 2 CL-ADD 16 CL-ADD CL-WOR  0 9 CL-ADD CL-ROR exit then
   a u s" c-lit" STR=CI if 0 5 CL-ADD 6 CL-ADD 7 CL-ADD 8 CL-ADD 9 CL-ADD 30 CL-ADD CL-WOR  0 11 CL-ADD CL-ROR exit then
   a u s" c-call" STR=CI if 0 5 CL-ADD 7 CL-ADD 8 CL-ADD 9 CL-ADD 10 CL-ADD 13 CL-ADD 14 CL-ADD 15 CL-ADD 30 CL-ADD CL-WOR  0 11 CL-ADD 12 CL-ADD CL-ROR exit then
   a u s" c-popflag" STR=CI if 0 9 CL-ADD 19 CL-ADD CL-WOR exit then
   a u s" c-pushcp" STR=CI if 0 9 CL-ADD 30 CL-ADD CL-WOR exit then
   a u s" c-emitw" STR=CI if 0 9 CL-ADD 30 CL-ADD CL-WOR exit then
   a u s" c-bback" STR=CI if 0 5 CL-ADD 9 CL-ADD 10 CL-ADD 30 CL-ADD CL-WOR  0 9 CL-ADD CL-ROR exit then
   a u s" cf-entry" STR=CI if KWCMP-MASK CL-WOR exit then
   a u s" cfb-entry" STR=CI if KWCMP-MASK CL-WOR exit then
   a u s" fold-entry" STR=CI if KWCMP-MASK CL-WOR exit then
   a u s" vop-entry" STR=CI if KWCMP-MASK CL-WOR exit then
   a u s" vcmp-entry" STR=CI if KWCMP-MASK CL-WOR exit then
   a u s" vshuf-entry" STR=CI if KWCMP-MASK CL-WOR exit then
   a u s" vun-entry" STR=CI if KWCMP-MASK CL-WOR exit then ;

: EFFECTS  {: a u lo hi :}  ( -- )
   0 WMSK !  0 RMSK !  lo hi COLLECT-REGS
   a u MN-W3? if 0 EW 1 ER 2 ER exit then
   a u MN-W2I? if 0 EW 1 ER exit then
   a u MN-W1? if 0 EW exit then
   a u MN-WRMW? if RR# @ 0 > if 0 EW 0 ER then exit then
   a u MN-LD? if 0 EW 1 ER exit then
   a u MN-ST? if 0 ER 1 ER exit then
   a u s" CMP," STR= if 0 ER 1 ER exit then
   a u s" CMPI," STR= if 0 ER exit then
   a u s" CBZ," STR= if 0 ER exit then
   a u s" CBNZ," STR= if 0 ER exit then
   a u s" SVC," STR= if 0 0 CL-ADD CL-WOR  0 0 CL-ADD 1 CL-ADD 2 CL-ADD 16 CL-ADD CL-ROR exit then
   a u s" SYS," STR= if 0 0 CL-ADD 16 CL-ADD CL-WOR  0 0 CL-ADD 1 CL-ADD 2 CL-ADD CL-ROR exit then
   a u s" RET," STR= if 0 30 CL-ADD CL-ROR exit then
   a u s" BLR," STR= if 1 18 lshift 1 -  30 CL-ADD CL-WOR  0 ER exit then
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

: C-NAME  ( idx -- a u )  dup cells CNOFF + @  swap cells CNLEN + @ ;
: CWS@  ( idx -- m )  cells CWS + @ ;
: CWS!  ( m idx -- )  cells CWS + ! ;
: C-WOR  {: idx m :}  ( -- )  idx CWS@ m or idx CWS! ;
: C-FIND  {: a u :}  ( -- idx|-1 )
   0 CX !
   begin CX @ CN# @ < while
      CX @ C-NAME a u STR=CI if CX @ exit then
      CX @ 1+ CX !
   repeat  -1 ;
: C-ADD  {: a u :}  ( -- idx )
   CN# @ CMAX >= if s" clobber-lint: too many labels" type NL 1 die then
   CEND @ u + CNBUF-CAP > if s" clobber-lint: label store full" type NL 1 die then
   a u CNBUF CEND @ + FOLD-TO
   CNBUF CEND @ + CNOFF CN# @ cells + !  u CNLEN CN# @ cells + !
   0 CWS CN# @ cells + !
   CEND @ u + CEND !  CN# @ dup 1+ CN# ! ;
: C-ENSURE  {: a u :}  ( -- idx )
   a u C-FIND dup 0 >= if exit then
   drop a u C-ADD ;
: EDGE?  {: from to :}  ( -- f )
   0 EX !
   begin EX @ EN# @ < while
      EX @ cells EFROM + @ from =  EX @ cells ETO + @ to = and if -1 exit then
      EX @ 1+ EX !
   repeat  0 ;
: EDGE+  {: from to :}  ( -- )
   from to EDGE? if exit then
   EN# @ EMAX >= if s" clobber-lint: too many BL edges" type NL 1 die then
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
      DI @ TOK s" ;" STR= if DI @ exit then
      DI @ 1+ DI !
   repeat  DI @ ;
: OPEN@  ( k -- tok-idx )  cells OPENINGS + @ ;
: OPEN+  ( tok-idx -- )
   ON# @ OMAX >= if s" clobber-lint: too many labels in definition" type NL 1 die then
   OPENINGS ON# @ cells + !  ON# @ 1+ ON# ! ;
: LABEL-OPEN?  {: k hi :}  ( -- f )
   k 2 + hi >= if 0 exit then
   k TOK START-L?  k 1+ TOK s" @" STR= and  k 2 + TOK s" LBL," STR= and ;
: COLLECT-OPENINGS  {: lo hi :}  ( -- )
   0 ON# !  lo OX !
   begin OX @ hi < while
      OX @ hi LABEL-OPEN? if OX @ OPEN+ then
      OX @ 1+ OX !
   repeat ;
: CALLEE?  {: lo hi :}  ( -- f )
   hi lo - 2 < if 0 exit then
   hi 1- TOK s" @" STR= 0= if 0 exit then
   hi 2 - TOK START-L? 0= if 0 exit then
   hi 2 - TOK  CALU ! CALA !  -1 ;

: ROUTINE-SCAN  {: cidx oi hi :}  ( -- )
   oi OPEN@ 3 + DI !  oi 1+ RNEXT !  0 LASTSTOP !  0 RDONE !  DI @ OPLO !
   begin DI @ hi < RDONE @ 0= and while
      RNEXT @ ON# @ <  DI @ RNEXT @ OPEN@ = and if
         LASTSTOP @ if -1 RDONE !
         else RNEXT @ 1+ RNEXT !  DI @ 3 + DI !  DI @ OPLO ! then
      else
         DI @ TOK INSTR? if
            OPLO @ DI @ CALLEE?  DI @ TOK s" BL," STR= and if
               CALA @ CALU @ C-ENSURE  cidx swap EDGE+
            else
               DI @ TOK  OPLO @ DI @ EFFECTS  cidx WMSK @ C-WOR
            then
            DI @ TOK STOP-MN? LASTSTOP !
            DI @ 1+ OPLO !
         then
         DI @ 1+ DI !
      then
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
      WI @ TOK s" :" STR= if
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
create WNAME 128 allot       variable WLEN
create NUMBUF 2 allot
variable BAD  variable PR  variable CW  variable RETS  variable CALIDX

: POIS-CLEAR  ( -- )
   0 PR !
   begin PR @ 32 < while -1 POIS PR @ cells + !  PR @ 1+ PR ! repeat ;
: WORD-NAME!  ( name-tok -- )
   TOK dup WLEN !  WNAME FOLD-TO ;
: CRASH-FILE?  {: a u :}  ( -- f )
   a u s" src/habu/crash.f" STR= ;
: ALLOW?  {: fa fu reg cidx :}  ( -- f )
   fa fu CRASH-FILE? 0= if 0 exit then
   WNAME WLEN @ s" emit-crash-handler" STR=CI 0= if 0 exit then
   reg 1 = reg 2 = or 0= if 0 exit then
   cidx 0 < if 0 exit then
   cidx C-NAME s" lhex" STR=CI ;
: DEC-TYPE  {: n :}  ( -- )
   n 10 < if 48 n + NUMBUF c!  NUMBUF 1 type
   else 48 n 10 / + NUMBUF c!  48 n 10 mod + NUMBUF 1+ c!  NUMBUF 2 type then ;
: REG-TYPE  ( r -- )  s" x" type DEC-TYPE ;
: FINDING  {: fa fu reg cidx :}  ( -- )
   s" CLOBBER " type fa fu type s"  " type
   WNAME WLEN @ type s" : " type reg REG-TYPE
   s"  written, clobbered by " type cidx C-NAME type
   s" , then read" type NL ;
: NOTE-READS  {: fa fu rmask :}  ( -- )
   0 PR !
   begin PR @ 32 < while
      rmask CONTRACT-MASK invert and PR @ CL-HAS? if
         POIS PR @ cells + @ CALIDX !
         CALIDX @ 0 >= if
            DIRTY @ PR @ CL-HAS? if
               fa fu PR @ CALIDX @ ALLOW? 0= if
                  fa fu PR @ CALIDX @ FINDING  BAD @ 1+ BAD !
               then
               -1 POIS PR @ cells + !
            then
         then
      then
      PR @ 1+ PR !
   repeat ;
: APPLY-WRITES  ( wmask -- )
   CONTRACT-MASK invert and dup DIRTY @ or DIRTY !
   0 PR !
   begin PR @ 32 < while
      dup PR @ CL-HAS? if -1 POIS PR @ cells + ! then
      PR @ 1+ PR !
   repeat  drop ;
: APPLY-RETURNS  ( rmask -- )
   dup DIRTY @ or DIRTY !
   0 PR !
   begin PR @ 32 < while
      dup PR @ CL-HAS? if -1 POIS PR @ cells + ! then
      PR @ 1+ PR !
   repeat  drop ;
: POISON-DIRTY  {: cmask cidx :}  ( -- )
   0 PR !
   begin PR @ 32 < while
      DIRTY @ PR @ CL-HAS?  cmask PR @ CL-HAS? and if
         cidx POIS PR @ cells + !
      then
      PR @ 1+ PR !
   repeat ;

: PASS2-DEF  {: fa fu lo hi :}  ( -- )
   0 DIRTY !  POIS-CLEAR  lo OPLO !  lo DI !
   begin DI @ hi < while
      DI @ TOK INSTR? if
         OPLO @ DI @ CALLEE?  DI @ TOK s" BL," STR= and if
            CALA @ CALU @ RETURNS-MASK RETS !
            CALA @ CALU @ C-FIND CALIDX !
            CALIDX @ 0 >= if CALIDX @ CWS@ else 0 then
            CONTRACT-MASK invert and  RETS @ invert and CW !
            CW @ CALIDX @ POISON-DIRTY
            RETS @ APPLY-RETURNS
         else
            DI @ TOK  OPLO @ DI @ EFFECTS
            fa fu RMSK @ NOTE-READS
            WMSK @ APPLY-WRITES
         then
         DI @ 1+ OPLO !
      then
      DI @ 1+ DI !
   repeat ;
: PASS2-FILE  {: pa pu :}  ( -- )
   pa pu FB 131072 READ-FILE  TOKENIZE
   0 WI !
   begin WI @ TN# @ 1- < while
      WI @ TOK s" :" STR= if
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
      s" clobber-lint: " type BAD @ . s"  finding(s)" type NL  1 die
   else
      s" clobber-lint: clean" type NL
   then ;
CLOBBER-LINT
