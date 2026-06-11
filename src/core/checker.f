0 constant T-CON  1 constant T-VAR  2 constant T-PTR  3 constant S-ROW  4 constant S-PUSH  -1 constant UNBOUND
2048 constant MAXTV            \ typevar pool (engine-sized bodies allocate hundreds)
create TVT MAXTV cells allot   create RVT MAXTV cells allot
: TVINIT 0 BEGIN dup cells TVT + UNBOUND swap ! dup cells RVT + UNBOUND swap ! 1 + dup MAXTV 1 - > UNTIL drop ;
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
: FRESH FV @ MAXTV 1 - > IF s" checker: out of typevars" 76 die THEN  FV @ dup 1 + FV ! ;
variable OK   variable DCUR   variable UNCK   variable BROW
: NEW -1 OK ! 0 UNCK ! 0 SPN ! 0 USP ! TVINIT 0 FV ! FRESH MK-ROW dup BROW ! DCUR ! ;
: STEP {: din dout :} DCUR @ din UNIFY OK @ and OK ! dout DCUR ! ;

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
   u 1 = c 114 = and IF 3 MK-CON ELSE          \ 'r' -> real/float (con 3)
   u 1 = c LOWER? and IF c VAR-OF ELSE          \ single letter -> type var
   1 MK-CON THEN THEN THEN THEN ;
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
\ prim sig table: records [nlen][name][slen][sig], 0-terminated — built from
\ readable strings (PT+ keeps the terminator as it appends).
create PTAB 1024 allot  variable PTP
: PT2+ {: a u :}  u PTP @ c!
   0 BEGIN dup u < WHILE  dup a + c@  over PTP @ + 1 + c!  1 + REPEAT drop
   PTP @ 1 + u + PTP !  0 PTP @ c! ;
: PT+ {: na nu sa su :}  na nu PT2+  sa su PT2+ ;
: PTABLE  PTAB PTP !  0 PTAB c!
   s" dup" s" a -- a a" PT+
   s" drop" s" a --" PT+
   s" swap" s" a b -- b a" PT+
   s" over" s" a b -- a b a" PT+
   s" nip" s" a b -- b" PT+
   s" tuck" s" a b -- b a b" PT+
   s" rot" s" a b c -- b c a" PT+
   s" -rot" s" a b c -- c a b" PT+
   s" 2dup" s" a b -- a b a b" PT+
   s" 2drop" s" a b --" PT+
   s" +" s" n n -- n" PT+
   s" -" s" n n -- n" PT+
   s" *" s" n n -- n" PT+
   s" and" s" n n -- n" PT+
   s" or" s" n n -- n" PT+
   s" xor" s" n n -- n" PT+
   s" 1+" s" n -- n" PT+
   s" 1-" s" n -- n" PT+
   s" negate" s" n -- n" PT+
   s" invert" s" n -- n" PT+
   s" 0=" s" n -- f" PT+
   s" 0<" s" n -- f" PT+
   s" =" s" n n -- f" PT+
   s" <" s" n n -- f" PT+
   s" >" s" n n -- f" PT+
   \ floats: r = real (concrete), distinct from n (int) and f (flag)
   s" f+" s" r r -- r" PT+    s" f-" s" r r -- r" PT+
   s" f*" s" r r -- r" PT+    s" f/" s" r r -- r" PT+
   s" fnegate" s" r -- r" PT+  s" fabs" s" r -- r" PT+  s" fsqrt" s" r -- r" PT+
   s" f<" s" r r -- f" PT+    s" f>" s" r r -- f" PT+   s" f=" s" r r -- f" PT+
   s" f0<" s" r -- f" PT+     s" f0=" s" r -- f" PT+
   s" s>f" s" n -- r" PT+     s" f>s" s" r -- n" PT+    s" f." s" r --" PT+ ;
PTABLE
variable FSA  variable FSU  variable FNL  variable FNP  variable FSL  variable FSP  variable FP
\ user sigs: certified words recorded as [len|name|len|sig]*, 0-terminated.
\ Appended by the renderer (RECXT hook); scanned after PTAB so later wins.
create USIGS 8192 allot   0 USIGS c!   variable UEND   0 UEND !
: UB! {: c :}  c USIGS UEND @ + c!  UEND @ 1 + UEND ! ;
: UBS {: a u :}  0 BEGIN dup u < WHILE  dup a + c@ UB!  1 + REPEAT drop ;
: USIG-ADD {: sa su na nu :}
   UEND @ nu + su + 3 + 8190 > IF s" checker: user sigs full" 76 die THEN
   nu UB!  na nu UBS  su UB!  sa su UBS  0 USIGS UEND @ + c! ;
: SCAN-SIGS {: tab a u :}  tab FP !
   BEGIN FP @ c@ dup WHILE                       \ no locals inside the loop (corrupts frame)
     FNL !  FP @ 1 + FNP !
     FNP @ FNL @ + dup c@ FSL ! 1 + FSP !
     a u FNP @ FNL @ STR= IF FSP @ FSA ! FSL @ FSU ! THEN
     FSP @ FSL @ + FP !
   REPEAT drop ;
: FIND-SIG {: a u :}  0 FSU !  PTAB a u SCAN-SIGS  USIGS a u SCAN-SIGS  FSU @ ;
variable FLD  variable FLI  variable FLO  variable FLC
: FLODIG? {: a u :}                        \ -?d+.d+ (one interior dot) -> float literal
   0 FLD !  0 FLI !  -1 FLO !
   u 3 < IF 0 FLO ! THEN
   a c@ 45 = IF 1 FLI ! THEN
   FLI @ BEGIN dup u < WHILE
     a over + c@ FLC !
     FLC @ 46 = IF FLD @ 0 > IF 0 FLO ! THEN FLD @ 1 + FLD !
     ELSE FLC @ 47 > FLC @ 58 < and 0= IF 0 FLO ! THEN THEN
     1 + REPEAT drop
   FLD @ 1 = FLO @ and
   u 0 > IF a u 1 - + c@ 46 = IF drop 0 THEN THEN
   a FLI @ + c@ 46 = IF drop 0 THEN ;
: DO-TOK {: a u :}
   a u FIND-SIG IF FSA @ FSU @ PARSE-SIG ELSE
   a u ALLDIG? IF s" -- n" PARSE-SIG ELSE
   a u FLODIG? IF s" -- r" PARSE-SIG ELSE -1 UNCK ! THEN THEN THEN ;
variable TBASE variable TBLEN variable TI variable TSTART
\ first token of the checked text is the word's NAME (skipped, kept for the
\ recorder); RECXT (installed by render.f) records certified sigs by name.
variable NMA  variable NMU  variable TOK0  variable RECXT  0 RECXT !
: DO-TOK1 {: a u :}  TOK0 @ IF a NMA ! u NMU ! 0 TOK0 ! ELSE a u DO-TOK THEN ;
: CHECK {: a u :} a TBASE ! u TBLEN ! NEW 0 TI ! 1 TOK0 ! 0 NMU ! BEGIN TI @ TBLEN @ < WHILE BEGIN TI @ TBLEN @ < TBASE @ TI @ + c@ 32 = and WHILE TI @ 1 + TI ! REPEAT TI @ TBLEN @ < IF TBASE @ TI @ + TSTART ! BEGIN TI @ TBLEN @ < TBASE @ TI @ + c@ 32 <> and WHILE TI @ 1 + TI ! REPEAT TSTART @ TBASE @ TI @ + TSTART @ - DO-TOK1 THEN REPEAT UNCK @ IF 1 ELSE OK @ THEN
   dup -1 = NMU @ 0 > and RECXT @ 0 <> and IF NMA @ NMU @ RECXT @ execute THEN ;
