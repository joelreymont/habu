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
: DUP-E FRESH FRESH {: a s :} a MK-VAR s MK-ROW MK-PUSH a MK-VAR a MK-VAR s MK-ROW MK-PUSH MK-PUSH STEP ;
: ADD-E FRESH {: s :} 1 MK-CON 1 MK-CON s MK-ROW MK-PUSH MK-PUSH 1 MK-CON s MK-ROW MK-PUSH STEP ;
: ZEQ-E FRESH {: s :} 1 MK-CON s MK-ROW MK-PUSH 2 MK-CON s MK-ROW MK-PUSH STEP ;
variable SEQ
: STR= {: a u b v :} u v = IF -1 SEQ ! 0 BEGIN dup u < WHILE dup a + c@ over b + c@ <> IF 0 SEQ ! THEN 1 + REPEAT drop ELSE 0 SEQ ! THEN SEQ @ ;
: DO-TOK {: a u :} a u s" dup" STR= IF DUP-E ELSE a u s" +" STR= IF ADD-E ELSE a u s" *" STR= IF ADD-E ELSE a u s" 0=" STR= IF ZEQ-E ELSE -1 UNCK ! THEN THEN THEN THEN ;
variable TBASE variable TBLEN variable TI variable TSTART
: CHECK {: a u :} a TBASE ! u TBLEN ! NEW 0 TI ! BEGIN TI @ TBLEN @ < WHILE BEGIN TI @ TBLEN @ < TBASE @ TI @ + c@ 32 = and WHILE TI @ 1 + TI ! REPEAT TI @ TBLEN @ < IF TBASE @ TI @ + TSTART ! BEGIN TI @ TBLEN @ < TBASE @ TI @ + c@ 32 <> and WHILE TI @ 1 + TI ! REPEAT TSTART @ TBASE @ TI @ + TSTART @ - DO-TOK THEN REPEAT UNCK @ IF 1 ELSE OK @ THEN ;
