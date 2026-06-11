0 constant T-CON  1 constant T-VAR  2 constant T-PTR  3 constant S-ROW  4 constant S-PUSH  -1 constant UNBOUND
create TVT 512 allot   create RVT 512 allot
: TVINIT 0 BEGIN dup cells TVT + UNBOUND swap ! dup cells RVT + UNBOUND swap ! 1 + dup 63 > UNTIL drop ;
: TAG 7 and ;
: PAY 3 rshift ;
: MK-CON 3 lshift ;
: MK-VAR 3 lshift T-VAR or ;
: MK-ROW 3 lshift S-ROW or ;
: TV@ cells TVT + @ ;
: TV! cells TVT + ! ;
: RV@ cells RVT + @ ;
: RV! cells RVT + ! ;
create SPA 1024 allot   variable SPN
: MK-PUSH SPN @ 2 * cells SPA + {: a :} a 8 + ! a ! SPN @ 3 lshift S-PUSH or SPN @ 1 + SPN ! ;
: P>TYPE PAY 2 * cells SPA + @ ;
: P>REST PAY 2 * cells SPA + 8 + @ ;
: ISVAR TAG T-VAR = ;
: ISROW TAG S-ROW = ;
: T-RES BEGIN dup ISVAR IF dup PAY TV@ dup UNBOUND = IF drop 0 ELSE nip -1 THEN ELSE 0 THEN WHILE REPEAT ;
: R-RES BEGIN dup ISROW IF dup PAY RV@ dup UNBOUND = IF drop 0 ELSE nip -1 THEN ELSE 0 THEN WHILE REPEAT ;
create UWL 512 allot   variable USP   variable UOK
: U-PUSH USP @ cells UWL + ! USP @ 1 + USP ! ;
: U-POP USP @ 1 - USP ! USP @ cells UWL + @ ;
: PAIR swap U-PUSH U-PUSH ;
: UNPAIR U-POP U-POP swap ;
: U-ROW R-RES swap R-RES swap 2dup = IF 2drop ELSE over ISROW IF swap PAY RV! ELSE dup ISROW IF PAY RV! ELSE 2dup P>TYPE swap P>TYPE swap PAIR P>REST swap P>REST swap PAIR THEN THEN THEN ;
: U-TYPE T-RES swap T-RES swap 2dup = IF 2drop ELSE over ISVAR IF swap PAY TV! ELSE dup ISVAR IF PAY TV! ELSE over PAY over PAY = IF 2drop ELSE 2drop 0 UOK ! THEN THEN THEN THEN ;
: UNIFY 0 USP ! -1 UOK ! PAIR BEGIN USP @ UOK @ and WHILE UNPAIR over TAG dup S-ROW = swap S-PUSH = or IF U-ROW ELSE U-TYPE THEN REPEAT UOK @ ;
variable FV
: FRESH FV @ dup 1 + FV ! ;
variable OK   variable DCUR   variable UNCK
: NEW -1 OK ! 0 UNCK ! 0 SPN ! 0 USP ! TVINIT 0 FV ! FRESH MK-ROW DCUR ! ;
: STEP {: din dout :} DCUR @ din UNIFY OK @ and OK ! dout DCUR ! ;
variable SEQ
: STR= {: a u b v :} u v = IF -1 SEQ ! 0 BEGIN dup u < WHILE dup a + c@ over b + c@ <> IF 0 SEQ ! THEN 1 + REPEAT drop ELSE 0 SEQ ! THEN SEQ @ ;

\ --- generic signature parser: build a step effect from a textual " in -- out "
\ stack effect. A single lowercase letter is a polymorphic type variable (shared
\ across in/out within one signature); `n` = int (con 1), `f` = flag (con 2);
\ anything else folds to int. Row variable is shared so the effect is row-polymorphic.
create NMAP 26 cells allot
: NMAP-RESET 0 BEGIN dup cells NMAP + UNBOUND swap ! 1 + dup 25 > UNTIL drop ;
: DIGIT? {: c :} c 47 > c 58 < and ;
: LOWER? {: c :} c 96 > c 123 < and ;
variable NRES
: ALLDIG? {: a u :} u 0= IF 0 NRES ! ELSE -1 NRES ! 0 BEGIN dup u < WHILE dup a + c@ DIGIT? 0= IF 0 NRES ! THEN 1 + REPEAT drop THEN NRES @ ;
\ NB: avoid a 2nd {: :} group here — `{: c :} … {: i :}` mis-reads the slot in the
\ standalone, collapsing every var to one. Compute the slot address on the stack.
: VAR-OF {: c :}  c 97 - cells NMAP +  dup @ UNBOUND = IF FRESH over ! THEN  @ MK-VAR ;
\ NB: declare locals at word top, never inside IF/loop (corrupts the locals frame).
: TOK-TYPE {: a u :}  a c@ {: c :}
   u 1 = c 110 = and IF 1 MK-CON ELSE          \ 'n' -> int (con 1)
   u 1 = c 102 = and IF 2 MK-CON ELSE          \ 'f' -> flag (con 2)
   u 1 = c LOWER? and IF c VAR-OF ELSE          \ single letter -> type var
   1 MK-CON THEN THEN THEN ;
variable PHASE  variable INROW  variable OUTROW
: SIG-TOK {: a u :}
   a u s" --" STR= IF 1 PHASE ! ELSE
     a u TOK-TYPE PHASE @ 0= IF INROW @ MK-PUSH INROW ! ELSE OUTROW @ MK-PUSH OUTROW ! THEN
   THEN ;
variable SB variable SL variable SI variable SS
: PARSE-SIG {: a u :}
   a SB ! u SL ! NMAP-RESET 0 PHASE !
   FRESH {: s :} s MK-ROW INROW ! s MK-ROW OUTROW ! 0 SI !
   BEGIN SI @ SL @ < WHILE
     BEGIN SI @ SL @ < SB @ SI @ + c@ 32 = and WHILE SI @ 1 + SI ! REPEAT
     SI @ SL @ < IF
       SB @ SI @ + SS !
       BEGIN SI @ SL @ < SB @ SI @ + c@ 32 <> and WHILE SI @ 1 + SI ! REPEAT
       SS @ SB @ SI @ + SS @ - SIG-TOK
     THEN
   REPEAT
   INROW @ OUTROW @ STEP ;

\ --- prim table: name/sig pairs [nlen][name][slen][sig]...[0], scanned by FIND-SIG.
\ A data table (not a 26-branch word) because the standalone INLINES colon-word
\ bodies, so a dispatch word with many PARSE-SIG calls overflows. DO-TOK stays small.
create PTAB 3 c, 100 c, 117 c, 112 c, 8 c, 97 c, 32 c, 45 c, 45 c, 32 c, 97 c, 32 c, 97 c, 4 c, 100 c, 114 c, 111 c, 112 c, 4 c, 97 c, 32 c, 45 c, 45 c, 4 c, 115 c, 119 c, 97 c, 112 c, 10 c, 97 c, 32 c, 98 c, 32 c, 45 c, 45 c, 32 c, 98 c, 32 c, 97 c, 4 c, 111 c, 118 c, 101 c, 114 c, 12 c, 97 c, 32 c, 98 c, 32 c, 45 c, 45 c, 32 c, 97 c, 32 c, 98 c, 32 c, 97 c, 3 c, 110 c, 105 c, 112 c, 8 c, 97 c, 32 c, 98 c, 32 c, 45 c, 45 c, 32 c, 98 c, 4 c, 116 c, 117 c, 99 c, 107 c, 12 c, 97 c, 32 c, 98 c, 32 c, 45 c, 45 c, 32 c, 98 c, 32 c, 97 c, 32 c, 98 c, 3 c, 114 c, 111 c, 116 c, 14 c, 97 c, 32 c, 98 c, 32 c, 99 c, 32 c, 45 c, 45 c, 32 c, 98 c, 32 c, 99 c, 32 c, 97 c, 4 c, 45 c, 114 c, 111 c, 116 c, 14 c, 97 c, 32 c, 98 c, 32 c, 99 c, 32 c, 45 c, 45 c, 32 c, 99 c, 32 c, 97 c, 32 c, 98 c, 4 c, 50 c, 100 c, 117 c, 112 c, 14 c, 97 c, 32 c, 98 c, 32 c, 45 c, 45 c, 32 c, 97 c, 32 c, 98 c, 32 c, 97 c, 32 c, 98 c, 5 c, 50 c, 100 c, 114 c, 111 c, 112 c, 6 c, 97 c, 32 c, 98 c, 32 c, 45 c, 45 c, 1 c, 43 c, 8 c, 110 c, 32 c, 110 c, 32 c, 45 c, 45 c, 32 c, 110 c, 1 c, 45 c, 8 c, 110 c, 32 c, 110 c, 32 c, 45 c, 45 c, 32 c, 110 c, 1 c, 42 c, 8 c, 110 c, 32 c, 110 c, 32 c, 45 c, 45 c, 32 c, 110 c, 3 c, 97 c, 110 c, 100 c, 8 c, 110 c, 32 c, 110 c, 32 c, 45 c, 45 c, 32 c, 110 c, 2 c, 111 c, 114 c, 8 c, 110 c, 32 c, 110 c, 32 c, 45 c, 45 c, 32 c, 110 c, 3 c, 120 c, 111 c, 114 c, 8 c, 110 c, 32 c, 110 c, 32 c, 45 c, 45 c, 32 c, 110 c, 2 c, 49 c, 43 c, 6 c, 110 c, 32 c, 45 c, 45 c, 32 c, 110 c, 2 c, 49 c, 45 c, 6 c, 110 c, 32 c, 45 c, 45 c, 32 c, 110 c, 6 c, 110 c, 101 c, 103 c, 97 c, 116 c, 101 c, 6 c, 110 c, 32 c, 45 c, 45 c, 32 c, 110 c, 6 c, 105 c, 110 c, 118 c, 101 c, 114 c, 116 c, 6 c, 110 c, 32 c, 45 c, 45 c, 32 c, 110 c, 2 c, 48 c, 61 c, 6 c, 110 c, 32 c, 45 c, 45 c, 32 c, 102 c, 2 c, 48 c, 60 c, 6 c, 110 c, 32 c, 45 c, 45 c, 32 c, 102 c, 1 c, 61 c, 8 c, 110 c, 32 c, 110 c, 32 c, 45 c, 45 c, 32 c, 102 c, 1 c, 60 c, 8 c, 110 c, 32 c, 110 c, 32 c, 45 c, 45 c, 32 c, 102 c, 1 c, 62 c, 8 c, 110 c, 32 c, 110 c, 32 c, 45 c, 45 c, 32 c, 102 c, 0 c,
variable FSA  variable FSU  variable FNL  variable FNP  variable FSL  variable FSP  variable FP
: FIND-SIG {: a u :}  0 FSU !  PTAB FP !
   BEGIN FP @ c@ dup WHILE                       \ no locals inside the loop (corrupts frame)
     FNL !  FP @ 1 + FNP !
     FNP @ FNL @ + dup c@ FSL ! 1 + FSP !
     a u FNP @ FNL @ STR= IF FSP @ FSA ! FSL @ FSU ! THEN
     FSP @ FSL @ + FP !
   REPEAT drop  FSU @ ;
: DO-TOK {: a u :}
   a u FIND-SIG IF FSA @ FSU @ PARSE-SIG ELSE
   a u ALLDIG? IF s" -- n" PARSE-SIG ELSE -1 UNCK ! THEN THEN ;
variable TBASE variable TBLEN variable TI variable TSTART
: CHECK {: a u :} a TBASE ! u TBLEN ! NEW 0 TI ! BEGIN TI @ TBLEN @ < WHILE BEGIN TI @ TBLEN @ < TBASE @ TI @ + c@ 32 = and WHILE TI @ 1 + TI ! REPEAT TI @ TBLEN @ < IF TBASE @ TI @ + TSTART ! BEGIN TI @ TBLEN @ < TBASE @ TI @ + c@ 32 <> and WHILE TI @ 1 + TI ! REPEAT TSTART @ TBASE @ TI @ + TSTART @ - DO-TOK THEN REPEAT UNCK @ IF 1 ELSE OK @ THEN ;
