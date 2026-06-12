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
2048 constant MAXPUSH          \ push records (engine-sized bodies need hundreds)
create SPA MAXPUSH 16 * allot   variable SPN
: MK-PUSH SPN @ MAXPUSH 1 - > IF s" checker: out of pushes" 76 die THEN
   SPN @ 2 * cells SPA + {: a :} a 8 + ! a ! SPN @ 3 lshift S-PUSH or SPN @ 1 + SPN ! ;
: P>TYPE PAY 2 * cells SPA + @ ;
: P>REST PAY 2 * cells SPA + 8 + @ ;
: ISVAR TAG T-VAR = ;
: ISROW TAG S-ROW = ;
: T-RES BEGIN dup ISVAR IF dup PAY TV@ dup UNBOUND = IF drop 0 ELSE nip -1 THEN ELSE 0 THEN WHILE REPEAT ;
: R-RES BEGIN dup ISROW IF dup PAY RV@ dup UNBOUND = IF drop 0 ELSE nip -1 THEN ELSE 0 THEN WHILE REPEAT ;
4096 constant MAXUWL           \ unify worklist cells (deep spines queue many pairs)
create UWL MAXUWL cells allot   variable USP   variable UOK
: U-PUSH USP @ MAXUWL 1 - > IF s" checker: unify worklist full" 76 die THEN
   USP @ cells UWL + ! USP @ 1 + USP ! ;
: U-POP USP @ 1 - USP ! USP @ cells UWL + @ ;
: PAIR swap U-PUSH U-PUSH ;
: UNPAIR U-POP U-POP swap ;
\ occurs check: binding a row var to a spine containing itself would make the
\ row cyclic (R-RES then loops forever) — mismatched branch depths trigger this.
: ROW-OCC? {: r s :}  s BEGIN R-RES dup TAG S-PUSH = WHILE P>REST REPEAT  r = ;
: U-ROW R-RES swap R-RES swap 2dup = IF 2drop ELSE
   over ISROW IF 2dup ROW-OCC? IF 2drop 0 UOK ! ELSE swap PAY RV! THEN ELSE
   dup ISROW IF 2dup swap ROW-OCC? IF 2drop 0 UOK ! ELSE PAY RV! THEN ELSE
   2dup P>TYPE swap P>TYPE swap PAIR P>REST swap P>REST swap PAIR THEN THEN THEN ;
: U-TYPE T-RES swap T-RES swap 2dup = IF 2drop ELSE over ISVAR IF swap PAY TV! ELSE dup ISVAR IF PAY TV! ELSE over PAY over PAY = IF 2drop ELSE 2drop 0 UOK ! THEN THEN THEN THEN ;
: UNIFY 0 USP ! -1 UOK ! PAIR BEGIN USP @ UOK @ and WHILE UNPAIR over TAG dup S-ROW = swap S-PUSH = or IF U-ROW ELSE U-TYPE THEN REPEAT UOK @ ;
variable FV
: FRESH FV @ MAXTV 1 - > IF s" checker: out of typevars" 76 die THEN  FV @ dup 1 + FV ! ;
variable OK   variable DCUR   variable UNCK   variable BROW
variable RCUR   variable RBROW
: NEW -1 OK ! 0 UNCK ! 0 SPN ! 0 USP ! TVINIT 0 FV !
   FRESH MK-ROW dup BROW ! DCUR !
   FRESH MK-ROW dup RBROW ! RCUR ! ;
variable WAS   variable DEXP   variable DACT   variable FAILSET
create FAILTK 64 allot   variable FAILTU
: STEP {: din dout :}
   DCUR @ WAS !
   DCUR @ din UNIFY
   dup 0=  FAILSET @ 0=  and  OK @ and  IF din DEXP !  WAS @ DACT !  -1 FAILSET ! THEN
   OK @ and OK !  dout DCUR ! ;

\ --- return row: >r r> r@ transfer types between DCUR and RCUR. A definition
\ must leave the return row exactly as it found it (ANS 3.2.3.3) — the final
\ balance check rejects net growth or borrowing; loop joins unify RCUR too.
: RS->R                                    \ >r : data top -> return row
   FRESH MK-VAR FRESH MK-ROW {: tv rest :}
   DCUR @  tv rest MK-PUSH  UNIFY OK @ and OK !
   rest DCUR !  tv RCUR @ MK-PUSH RCUR ! ;
: RSR>                                     \ r> : return top -> data row
   FRESH MK-VAR FRESH MK-ROW {: tv rest :}
   RCUR @  tv rest MK-PUSH  UNIFY OK @ and OK !
   rest RCUR !  tv DCUR @ MK-PUSH DCUR ! ;
: RSR@                                     \ r@ : peek return top
   FRESH MK-VAR FRESH MK-ROW {: tv rest :}
   RCUR @  tv rest MK-PUSH  UNIFY OK @ and OK !
   tv DCUR @ MK-PUSH DCUR ! ;
variable RSH
: RS-TOK? {: a u :}
   -1 RSH !
   a u s" >r" STR= IF RS->R ELSE
   a u s" r>" STR= IF RSR> ELSE
   a u s" r@" STR= IF RSR@ ELSE
   0 RSH ! THEN THEN THEN
   RSH @ ;

\ --- generic signature parser: build a step effect from a textual " in -- out "
\ stack effect. A single lowercase letter is a polymorphic type variable (shared
\ across in/out within one signature); `n` = int (con 1), `f` = flag (con 2);
\ anything else folds to int. Row variable is shared so the effect is row-polymorphic.
create NMAP 26 cells allot
: NMAP-RESET 0 BEGIN dup cells NMAP + UNBOUND swap ! 1 + dup 25 > UNTIL drop ;
: DIGIT? {: c :} c 47 > c 58 < and ;
: LOWER? {: c :} c 96 > c 123 < and ;
variable NRES  variable NDI  variable NDH
: HEXD? {: c :} c DIGIT?  c 96 > c 103 < and or  c 64 > c 71 < and or ;
\ int literal: d+ | -d+ | $h+ | -$h+ (the engine's number tokens)
: ALLDIG? {: a u :}
   0 NDI !  0 NDH !
   u 0 > IF a c@ 45 = IF 1 NDI ! THEN THEN
   u NDI @ > IF a NDI @ + c@ 36 = IF NDI @ 1 + NDI !  1 NDH ! THEN THEN
   u NDI @ - 0 > 0= IF 0 NRES ! ELSE -1 NRES !
     NDI @ BEGIN dup u < WHILE
       NDH @ IF dup a + c@ HEXD? 0= IF 0 NRES ! THEN
       ELSE dup a + c@ DIGIT? 0= IF 0 NRES ! THEN THEN
       1 + REPEAT drop THEN
   NRES @ ;
\ NB: avoid a 2nd {: :} group here — `{: c :} … {: i :}` mis-reads the slot in the
\ standalone, collapsing every var to one. Compute the slot address on the stack.
: VAR-OF {: c :}  c 97 - cells NMAP +  dup @ UNBOUND = IF FRESH over ! THEN  @ MK-VAR ;
\ NB: declare locals at word top, never inside IF/loop (corrupts the locals frame).
: TOK-TYPE {: a u :}  a c@ {: c :}
   u 1 = c 110 = and IF 1 MK-CON ELSE          \ 'n' -> int (con 1)
   u 1 = c 102 = and IF 1 MK-CON ELSE          \ 'f' -> flag = int (Forth flags are -1/0)
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
create PTAB 2048 allot  variable PTP
create SDQN 2 allot  115 SDQN c!  34 SDQN 1 + c!     \ the two chars of `s"`
: PT2+ {: a u :}
   PTP @ u + 2 + PTAB 2046 + > IF s" checker: prim table full" 76 die THEN
   u PTP @ c!
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
   s" <>" s" n n -- f" PT+
   s" <=" s" n n -- f" PT+
   s" >=" s" n n -- f" PT+
   s" /" s" n n -- n" PT+
   s" mod" s" n n -- n" PT+
   s" lshift" s" n n -- n" PT+
   s" rshift" s" n n -- n" PT+
   s" cells" s" n -- n" PT+
   s" @" s" n -- n" PT+
   s" !" s" a n --" PT+
   s" c@" s" n -- n" PT+
   s" c!" s" a n --" PT+
   s" ." s" n --" PT+
   s" .s" s" --" PT+
   s" here" s" -- n" PT+
   s" allot" s" n --" PT+
   s" ," s" n --" PT+
   s" c," s" n --" PT+
   s" type" s" n n --" PT+
   s" throw" s" n --" PT+
   \ floats: r = real (concrete), distinct from n (int) and f (flag)
   s" f+" s" r r -- r" PT+    s" f-" s" r r -- r" PT+
   s" f*" s" r r -- r" PT+    s" f/" s" r r -- r" PT+
   s" fnegate" s" r -- r" PT+  s" fabs" s" r -- r" PT+  s" fsqrt" s" r -- r" PT+
   s" f<" s" r r -- f" PT+    s" f>" s" r r -- f" PT+   s" f=" s" r r -- f" PT+
   s" f0<" s" r -- f" PT+     s" f0=" s" r -- f" PT+
   s" s>f" s" n -- r" PT+     s" f>s" s" r -- n" PT+    s" f." s" r --" PT+
   \ s" pushes addr+len; ['] pushes an xt. The engine consumes their payload
   \ inline, so only the bare token reaches the body capture.
   SDQN 2 PT2+  s" -- n n" PT2+
   s" [']" s" -- n" PT+
   s" [char]" s" -- n" PT+
   s" emit" s" n --" PT+
   s" cr" s" --" PT+
   s" space" s" --" PT+
   s" u." s" n --" PT+
   \ defining-word kinds (the engine hooks "NAME create" etc. so the name gets
   \ recorded): create/variable are addresses; a constant's cell is untyped.
   s" create" s" -- n" PT+
   s" variable" s" -- n" PT+
   s" constant" s" -- a" PT+ ;
PTABLE
variable FSA  variable FSU  variable FNL  variable FNP  variable FSL  variable FSP  variable FP
\ user sigs: certified words recorded as [len|name|len|sig]*, 0-terminated.
\ Appended by the renderer (RECXT hook); scanned after PTAB so later wins.
create USIGS 32768 allot   0 USIGS c!   variable UEND   0 UEND !
: UB! {: c :}  c USIGS UEND @ + c!  UEND @ 1 + UEND ! ;
: UBS {: a u :}  0 BEGIN dup u < WHILE  dup a + c@ UB!  1 + REPEAT drop ;
: USIG-ADD {: sa su na nu :}
   UEND @ nu + su + 3 + 32766 > IF s" checker: user sigs full" 76 die THEN
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
\ --- locals: {: a b :} pops and binds names to type vars; a reference pushes
\ its binding. Groups accumulate (a later group binds only its own names).
: CCOPY {: a d u :}  0 BEGIN dup u < WHILE  dup a + c@  over d + c!  1 + REPEAT drop ;
create LOCNB 256 allot   create LOCLN 16 cells allot   create LOCTV 16 cells allot
variable #LOC  variable LMODE  variable LGRP  variable LROW  variable LCH  variable LI  variable LRF
: LOC-ADD {: a u :}
   #LOC @ 15 >  u 16 >  or IF -1 UNCK ! ELSE
     a  LOCNB #LOC @ 16 * +  u CCOPY
     u #LOC @ cells LOCLN + !
     FRESH MK-VAR #LOC @ cells LOCTV + !
     #LOC @ 1 + #LOC ! THEN ;
: LOC-BIND
   FRESH dup LROW !  MK-ROW LCH !
   LGRP @ BEGIN dup #LOC @ < WHILE
     dup cells LOCTV + @  LCH @ MK-PUSH LCH !
     1 + REPEAT drop
   LCH @  LROW @ MK-ROW  STEP ;
: LOC-TOK {: a u :}
   a u s" :}" STR= IF 0 LMODE ! LOC-BIND ELSE
   a u s" --" STR= IF -1 UNCK ! ELSE
   a u LOC-ADD THEN THEN ;
: LOC-REF? {: a u :}
   0 LRF !  #LOC @ LI !
   BEGIN LI @ 0 >  LRF @ 0=  and WHILE
     LI @ 1 - LI !
     a u  LOCNB LI @ 16 * +  LI @ cells LOCLN + @  STR= IF
       LI @ cells LOCTV + @  DCUR @ MK-PUSH DCUR !  -1 LRF ! THEN
   REPEAT  LRF @ ;
\ --- control flow: branch states saved on a CF stack and unified at joins.
\ Both rows are snapshot: A/B = data, RA/RB = return (PLAN: net growth on
\ either row at a back edge is a row-occurs failure).
\ kinds: 1 if  2 if+else  3 begin  4 begin+while  5 do
create CFKND 32 cells allot   create CFSA 32 cells allot   create CFSB 32 cells allot
create CFRA 32 cells allot    create CFRB 32 cells allot
variable #CFC  variable CTMP  variable RTMP  variable CFH  variable INDO
: CF-PUSH {: k s0 s1 r0 r1 :}
   #CFC @ 31 > IF -1 UNCK ! ELSE
     k #CFC @ cells CFKND + !  s0 #CFC @ cells CFSA + !  s1 #CFC @ cells CFSB + !
     r0 #CFC @ cells CFRA + !  r1 #CFC @ cells CFRB + !
     #CFC @ 1 + #CFC ! THEN ;
: CF@K #CFC @ 1 - cells CFKND + @ ;
: CF@A #CFC @ 1 - cells CFSA + @ ;
: CF@B #CFC @ 1 - cells CFSB + @ ;
: CF@RA #CFC @ 1 - cells CFRA + @ ;
: CF@RB #CFC @ 1 - cells CFRB + @ ;
: CF-DROP #CFC @ 1 - #CFC ! ;
: CF-MT? #CFC @ 0 > 0= ;
: SUNI {: s :}
   DCUR @ s UNIFY
   dup 0=  FAILSET @ 0=  and  OK @ and  IF s DEXP !  DCUR @ DACT !  -1 FAILSET ! THEN
   OK @ and OK ! ;
: RSUNI {: s :}  RCUR @ s UNIFY OK @ and OK ! ;
: CF-IF  s" a --" PARSE-SIG  1 DCUR @ 0 RCUR @ 0 CF-PUSH ;
: CF-ELSE
   CF-MT? IF -1 UNCK ! ELSE CF@K 1 <> IF -1 UNCK ! ELSE
     DCUR @ CTMP !  CF@A DCUR !
     RCUR @ RTMP !  CF@RA RCUR !
     2 #CFC @ 1 - cells CFKND + !
     CTMP @ #CFC @ 1 - cells CFSB + !
     RTMP @ #CFC @ 1 - cells CFRB + !
   THEN THEN ;
: CF-THEN
   CF-MT? IF -1 UNCK ! ELSE
     CF@K 1 = IF CF@A SUNI CF@RA RSUNI CF-DROP ELSE
     CF@K 2 = IF CF@B SUNI CF@RB RSUNI CF-DROP ELSE -1 UNCK ! THEN THEN THEN ;
: CF-BEGIN  3 DCUR @ 0 RCUR @ 0 CF-PUSH ;
: CF-UNTIL
   s" a --" PARSE-SIG
   CF-MT? IF -1 UNCK ! ELSE CF@K 3 <> IF -1 UNCK ! ELSE
     CF@A SUNI  CF@A DCUR !  CF@RA RSUNI  CF@RA RCUR !  CF-DROP THEN THEN ;
: CF-AGAIN
   CF-MT? IF -1 UNCK ! ELSE CF@K 3 <> IF -1 UNCK ! ELSE
     CF@A SUNI  CF@A DCUR !  CF@RA RSUNI  CF@RA RCUR !  CF-DROP THEN THEN ;
: CF-WHILE
   s" a --" PARSE-SIG
   CF-MT? IF -1 UNCK ! ELSE CF@K 3 <> IF -1 UNCK ! ELSE
     4 #CFC @ 1 - cells CFKND + !
     DCUR @ #CFC @ 1 - cells CFSB + !
     RCUR @ #CFC @ 1 - cells CFRB + !
   THEN THEN ;
: CF-REPEAT
   CF-MT? IF -1 UNCK ! ELSE CF@K 4 <> IF -1 UNCK ! ELSE
     CF@A SUNI  CF@B DCUR !  CF@RA RSUNI  CF@RB RCUR !  CF-DROP THEN THEN ;
: CF-DO  s" n n --" PARSE-SIG  5 DCUR @ 0 RCUR @ 0 CF-PUSH ;
: CF-LOOP
   CF-MT? IF -1 UNCK ! ELSE CF@K 5 <> IF -1 UNCK ! ELSE
     CF@A SUNI  CF@A DCUR !  CF@RA RSUNI  CF@RA RCUR !  CF-DROP THEN THEN ;
: CF-+LOOP
   s" n --" PARSE-SIG
   CF-MT? IF -1 UNCK ! ELSE CF@K 5 <> IF -1 UNCK ! ELSE
     CF@A SUNI  CF@A DCUR !  CF@RA RSUNI  CF@RA RCUR !  CF-DROP THEN THEN ;
: CF-I
   0 INDO !  0 BEGIN dup #CFC @ < WHILE
     dup cells CFKND + @ 5 = IF -1 INDO ! THEN  1 + REPEAT drop
   INDO @ IF s" -- n" PARSE-SIG ELSE -1 UNCK ! THEN ;
: CF-J                                     \ needs two enclosing DO frames
   0 INDO !  0 BEGIN dup #CFC @ < WHILE
     dup cells CFKND + @ 5 = IF INDO @ 1 + INDO ! THEN  1 + REPEAT drop
   INDO @ 1 > IF s" -- n" PARSE-SIG ELSE -1 UNCK ! THEN ;
: CF-TOK? {: a u :}
   -1 CFH !
   a u s" if" STR= IF CF-IF ELSE
   a u s" else" STR= IF CF-ELSE ELSE
   a u s" then" STR= IF CF-THEN ELSE
   a u s" begin" STR= IF CF-BEGIN ELSE
   a u s" until" STR= IF CF-UNTIL ELSE
   a u s" again" STR= IF CF-AGAIN ELSE
   a u s" while" STR= IF CF-WHILE ELSE
   a u s" repeat" STR= IF CF-REPEAT ELSE
   a u s" do" STR= IF CF-DO ELSE
   a u s" ?do" STR= IF CF-DO ELSE
   a u s" loop" STR= IF CF-LOOP ELSE
   a u s" +loop" STR= IF CF-+LOOP ELSE
   a u s" i" STR= IF CF-I ELSE
   a u s" j" STR= IF CF-J ELSE
   0 CFH ! THEN THEN THEN THEN THEN THEN THEN THEN THEN THEN THEN THEN THEN THEN
   CFH @ ;
variable TBASE variable TBLEN variable TI variable TSTART
\ first token of the checked text is the word's NAME (skipped, kept for the
\ recorder); RECXT (installed by render.f) records certified sigs by name.
variable NMA  variable NMU  variable TOK0  variable RECXT  0 RECXT !
variable DIAGXT  0 DIAGXT !              \ reject-diagnostic hook (render.f installs)
\ the engine folds A-Z in keyword and dict matching — fold every token the same
\ way (into a scratch copy: the source text may live in the read-only image).
create TKF 64 allot   create NMB 64 allot   variable TFU
: TOKFOLD {: a u :}
   u 64 > IF 0 ELSE
     0 BEGIN dup u < WHILE
       dup a + c@  dup 64 >  over 91 <  and IF 32 or THEN
       over TKF + c!  1 +
     REPEAT drop  u TFU !  -1 THEN ;
\ TRUST: declare a word's effect without checking its body — the native escape
\ hatch (PLAN's TRUSTED:). Callers are checked against the declared sig.
\ Usage:  s" myword" s" n n -- n" trust
: trust {: na nu sa su :}
   na nu TOKFOLD 0= IF s" trust: name too long" 76 die THEN
   sa su  TKF TFU @  USIG-ADD ;
: DO-TOK1 {: a u :}
   a u TOKFOLD 0= IF -1 UNCK ! ELSE
   FAILSET @ 0= IF TKF FAILTK TFU @ CCOPY  TFU @ FAILTU ! THEN
   TOK0 @ IF TKF NMB TFU @ CCOPY  NMB NMA !  TFU @ NMU !  0 TOK0 ! ELSE
   LMODE @ IF TKF TFU @ LOC-TOK ELSE
   TKF TFU @ s" {:" STR= IF 1 LMODE !  #LOC @ LGRP ! ELSE
   TKF TFU @ CF-TOK? 0= IF
   TKF TFU @ RS-TOK? 0= IF
   TKF TFU @ LOC-REF? 0= IF
   TKF TFU @ DO-TOK THEN THEN THEN THEN THEN THEN THEN
   OK @ 0=  FAILSET @ 0=  and IF -1 FAILSET ! THEN ;
: CHECK {: a u :} a TBASE ! u TBLEN ! NEW 0 TI ! 1 TOK0 ! 0 NMU ! 0 #LOC ! 0 LMODE ! 0 #CFC !
   0 FAILSET ! 0 DEXP ! 0 DACT ! 0 FAILTU ! BEGIN TI @ TBLEN @ < WHILE BEGIN TI @ TBLEN @ < TBASE @ TI @ + c@ 32 = and WHILE TI @ 1 + TI ! REPEAT TI @ TBLEN @ < IF TBASE @ TI @ + TSTART ! BEGIN TI @ TBLEN @ < TBASE @ TI @ + c@ 32 <> and WHILE TI @ 1 + TI ! REPEAT TSTART @ TBASE @ TI @ + TSTART @ - DO-TOK1 THEN REPEAT
   LMODE @ 0 <>  #CFC @ 0 <>  or IF -1 UNCK ! THEN
   RCUR @ R-RES  RBROW @ R-RES  <> IF 0 OK ! THEN   \ return row must balance
   UNCK @ IF 1 ELSE OK @ THEN
   dup 0 =  DIAGXT @ 0 <>  and IF DIAGXT @ execute THEN
   dup -1 = NMU @ 0 > and RECXT @ 0 <> and IF NMA @ NMU @ RECXT @ execute THEN ;
