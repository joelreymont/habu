\ vs.fs — a register-allocating code generator for the standalone (caf's VS allocator,
\ ported): the data stack lives in REGISTERS (x9..x15), not memory. swap/over are free
\ (just relabel), arithmetic is reg->reg, and there is NO ldr/str traffic until the
\ pool spills. `5 dup *` becomes ~5 instructions instead of 16. Needs asm.fs + icode.fs.
\ Emitters are dispatched by xt (execute) from a table, so the dispatcher stays tiny.
create OPN 3 c, 100 c, 117 c, 112 c, 0 c, 4 c, 100 c, 114 c, 111 c, 112 c, 1 c, 4 c, 115 c, 119 c, 97 c, 112 c, 2 c, 4 c, 111 c, 118 c, 101 c, 114 c, 3 c, 3 c, 110 c, 105 c, 112 c, 4 c, 1 c, 43 c, 5 c, 1 c, 45 c, 6 c, 1 c, 42 c, 7 c, 3 c, 97 c, 110 c, 100 c, 8 c, 2 c, 111 c, 114 c, 9 c, 3 c, 120 c, 111 c, 114 c, 10 c, 6 c, 110 c, 101 c, 103 c, 97 c, 116 c, 101 c, 11 c, 2 c, 48 c, 60 c, 12 c, 2 c, 105 c, 102 c, 13 c, 4 c, 116 c, 104 c, 101 c, 110 c, 14 c, 2 c, 48 c, 61 c, 15 c, 5 c, 98 c, 101 c, 103 c, 105 c, 110 c, 16 c, 5 c, 117 c, 110 c, 116 c, 105 c, 108 c, 17 c, 1 c, 64 c, 18 c, 1 c, 33 c, 19 c, 2 c, 99 c, 64 c, 20 c, 2 c, 99 c, 33 c, 21 c, 4 c, 104 c, 101 c, 114 c, 101 c, 22 c, 1 c, 61 c, 23 c, 1 c, 60 c, 24 c, 1 c, 62 c, 25 c, 2 c, 49 c, 43 c, 26 c, 2 c, 49 c, 45 c, 27 c, 0 c,
28 constant NOPS
\ register pool x9..x15 (scratch in a leaf body — no calls)
create RPOOL 9 c, 10 c, 11 c, 12 c, 13 c, 14 c, 15 c,
7 constant NRP
create RFREE NRP allot
variable RAI  variable RRES
: RP-RESET 0 RAI ! BEGIN RAI @ NRP < WHILE 1 RAI @ RFREE + c! RAI @ 1 + RAI ! REPEAT ;
\ abstract value stack with CONSTANT FOLDING: each entry is a register, a known
\ compile-time constant, or a spilled memory slot. VTAG[k] 0=reg 1=con 2=mem;
\ VVAL[k]=reg number / constant value / (slot addressed via k).
create VTAG 64 allot   create VVAL 64 cells allot   variable VSP
\ spill the DEEPEST reg-resident VS entry to its canonical slot [x19,#k*8] and hand back
\ its freed register — used when the x9..x15 pool exhausts on a deep stack (>7 live).
variable SPK  variable SPR
: R-SPILL-DEEPEST  -1 SPK !  0 RAI !
   BEGIN RAI @ VSP @ < SPK @ -1 = and WHILE
     VTAG RAI @ + c@ 0= IF RAI @ SPK ! THEN  RAI @ 1 + RAI ! REPEAT
   VVAL SPK @ cells + @ SPR !
   SPR @ 19 SPK @ 8 * ENC-STR EMITW  2 VTAG SPK @ + c!  SPR @ ;
: R-ALLOC  -1 RRES !  0 RAI !
   BEGIN RAI @ NRP < RRES @ -1 = and WHILE
     RAI @ RFREE + c@ IF 0 RAI @ RFREE + c!  RAI @ RPOOL + c@ RRES ! THEN  RAI @ 1 + RAI !
   REPEAT  RRES @ -1 = IF R-SPILL-DEEPEST RRES ! THEN  RRES @ ;
: R-FREE {: r :}  0 RAI ! BEGIN RAI @ NRP < WHILE
     RAI @ RPOOL + c@ r = IF 1 RAI @ RFREE + c! THEN  RAI @ 1 + RAI ! REPEAT ;
: V-PUSHR {: r :}  0 VTAG VSP @ + c!  r VVAL VSP @ cells + !  VSP @ 1 + VSP ! ;
: V-PUSHC {: n :}  1 VTAG VSP @ + c!  n VVAL VSP @ cells + !  VSP @ 1 + VSP ! ;
: GMOV {: d s :}  d 31 s ENC-ORR EMITW ;               \ mov d, s  (orr d, xzr, s)
\ materialise VS[k] into a register if it is a constant (movz/movk); idempotent.
variable VFR  variable VFN
: V-FORCE {: k :}                                       \ tag 1=con (movz/movk), 2=mem (ldr slot)
   VTAG k + c@ 1 = IF  VVAL k cells + @ VFN !  R-ALLOC VFR !
     VFR @ VFN @ 65535 and 0 MOVZHW EMITW  VFR @ VFN @ 16 rshift 65535 and 1 MOVKHW EMITW
     0 VTAG k + c!  VFR @ VVAL k cells + !  THEN
   VTAG k + c@ 2 = IF  R-ALLOC VFR !  VFR @ 19 k 8 * ENC-LDR EMITW
     0 VTAG k + c!  VFR @ VVAL k cells + !  THEN ;
: V-REG {: k :}  k V-FORCE  VVAL k cells + @ ;          \ force + return register
: REG-COPY {: s :}  R-ALLOC {: r :}  r s GMOV  r V-PUSHR ;
\ spill the whole VS to canonical memory slots [x19,#k*8] (at control-flow boundaries).
variable SAI  variable SAR
: V-SPILL-ALL  0 SAI ! BEGIN SAI @ VSP @ < WHILE
     VTAG SAI @ + c@ 2 <> IF  SAI @ V-REG SAR !
       SAR @ 19 SAI @ 8 * ENC-STR EMITW  SAR @ R-FREE  2 VTAG SAI @ + c! THEN
     SAI @ 1 + SAI ! REPEAT ;
\ emitters
: G-LIT {: n :}  n V-PUSHC ;                            \ record the constant — no code yet
: G-DUP   VSP @ 1 - {: k :}
   VTAG k + c@ IF  VVAL k cells + @ V-PUSHC  ELSE  VVAL k cells + @ REG-COPY  THEN ;
: G-DROP  VSP @ 1 - {: k :}  VTAG k + c@ 0= IF VVAL k cells + @ R-FREE THEN  VSP @ 1 - VSP ! ;
: G-OVER  VSP @ 2 - {: k :}
   VTAG k + c@ IF  VVAL k cells + @ V-PUSHC  ELSE  VVAL k cells + @ REG-COPY  THEN ;
: G-SWAP  VSP @ 1 - {: kb :}  VSP @ 2 - {: ka :}
   VTAG ka + c@ {: ta :}  VVAL ka cells + @ {: va :}
   VTAG kb + c@ VTAG ka + c!  VVAL kb cells + @ VVAL ka cells + !
   ta VTAG kb + c!  va VVAL kb cells + ! ;
: G-NIP   G-SWAP G-DROP ;
\ binops: fold if both operands are constants, else reg-reg op (result in NOS's reg)
: BOTH-CON?  VTAG VSP @ 1 - + c@  VTAG VSP @ 2 - + c@  and ;
: AV  VVAL VSP @ 2 - cells + @ ;   : BV  VVAL VSP @ 1 - cells + @ ;
: FOLD2 {: res :}  VSP @ 2 - VSP !  res V-PUSHC ;
variable RBB  variable RAA
: REGOP {: encxt :}
   VSP @ 1 - V-REG RBB !   VSP @ 2 - V-REG RAA !
   RAA @ RAA @ RBB @ encxt execute EMITW
   RBB @ R-FREE  VSP @ 2 - VSP !  RAA @ V-PUSHR ;
: G-ADD  BOTH-CON? IF AV BV +   FOLD2 ELSE ['] ENC-ADD REGOP THEN ;
: G-SUB  BOTH-CON? IF AV BV -   FOLD2 ELSE ['] ENC-SUB REGOP THEN ;
: G-MUL  BOTH-CON? IF AV BV *   FOLD2 ELSE ['] ENC-MUL REGOP THEN ;
: G-AND  BOTH-CON? IF AV BV and FOLD2 ELSE ['] ENC-AND REGOP THEN ;
: G-OR   BOTH-CON? IF AV BV or  FOLD2 ELSE ['] ENC-ORR REGOP THEN ;
: G-XOR  BOTH-CON? IF AV BV xor FOLD2 ELSE ['] ENC-EOR REGOP THEN ;
\ negate (in-place, fold if constant) and 0< -> Forth flag 0/-1 in the top register
variable U1R  variable CMR
: G-NEGATE  VSP @ 1 - {: k :} VTAG k + c@ 1 = IF VVAL k cells + @ negate VVAL k cells + !
   ELSE k V-REG U1R ! U1R @ 31 U1R @ ENC-SUB EMITW THEN ;
: G-0<  VSP @ 1 - V-REG CMR !
   CMR @ 0 ENC-CMPI EMITW  CMR @ 11 ENC-CSET EMITW  CMR @ 31 CMR @ ENC-SUB EMITW ;
\ control flow: IF/THEN. Spill VS to canonical memory at the boundary so both paths
\ agree; pop the flag and cbz past the body. CF stack holds the merge label.
create CFLBL 32 cells allot   variable CFSP
variable IFR
: G-IF   VSP @ 1 - V-REG IFR !  VSP @ 1 - VSP !  V-SPILL-ALL  IFR @ R-FREE
   NEWLBL {: lend :}  IFR @ lend CBZ,  lend CFLBL CFSP @ cells + !  CFSP @ 1 + CFSP ! ;
: G-THEN  V-SPILL-ALL  CFSP @ 1 - CFSP !  CFLBL CFSP @ cells + @ LBL, ;
: G-0=  VSP @ 1 - V-REG CMR !
   CMR @ 0 ENC-CMPI EMITW  CMR @ 0 ENC-CSET EMITW  CMR @ 31 CMR @ ENC-SUB EMITW ;
\ BEGIN/UNTIL: spill to canonical memory at the loop top and the back-edge (so the
\ layout is invariant); UNTIL pops the flag and cbz's back to BEGIN while it is false.
: G-BEGIN  V-SPILL-ALL  NEWLBL {: lb :}  lb LBL,  lb CFLBL CFSP @ cells + !  CFSP @ 1 + CFSP ! ;
: G-UNTIL  VSP @ 1 - V-REG IFR !  VSP @ 1 - VSP !  V-SPILL-ALL
   CFSP @ 1 - CFSP !  IFR @ CFLBL CFSP @ cells + @ CBZ,  IFR @ R-FREE ;
\ memory ops (use variables, not 2nd-group locals). @ c@ are in-place; ! c! pop 2.
\ HERE pushes a scratch buffer at x19+256 (above the spill slots, which use 0..~7).
variable MRA  variable MRV
: G-@   VSP @ 1 - V-REG MRA !  MRA @ MRA @ 0 ENC-LDR  EMITW ;
: G-C@  VSP @ 1 - V-REG MRA !  MRA @ MRA @ 0 ENC-LDRB EMITW ;
: G-!   VSP @ 1 - V-REG MRA !  VSP @ 2 - V-REG MRV !
   MRV @ MRA @ 0 ENC-STR  EMITW  MRA @ R-FREE  MRV @ R-FREE  VSP @ 2 - VSP ! ;
: G-C!  VSP @ 1 - V-REG MRA !  VSP @ 2 - V-REG MRV !
   MRV @ MRA @ 0 ENC-STRB EMITW  MRA @ R-FREE  MRV @ R-FREE  VSP @ 2 - VSP ! ;
: G-HERE  R-ALLOC MRA !  MRA @ 19 256 ENC-ADDI EMITW  MRA @ V-PUSHR ;
\ binary comparisons ( a b -- flag ): cmp a,b; cset cond; 0-reg -> Forth flag 0/-1.
\ Fold path must match the runtime flag: host = / < / > already yield 0/-1 (no negate).
variable GCB  variable GCA
: GCMP {: cond :}  VSP @ 1 - V-REG GCB !  VSP @ 2 - V-REG GCA !
   GCA @ GCB @ ENC-CMP EMITW  GCA @ cond ENC-CSET EMITW  GCA @ 31 GCA @ ENC-SUB EMITW
   GCB @ R-FREE  VSP @ 2 - VSP !  GCA @ V-PUSHR ;
: G-EQ  BOTH-CON? IF AV BV = FOLD2 ELSE 0 GCMP THEN ;
: G-LT  BOTH-CON? IF AV BV < FOLD2 ELSE 11 GCMP THEN ;
: G-GT  BOTH-CON? IF AV BV > FOLD2 ELSE 12 GCMP THEN ;
\ 1+ / 1- : in-place fold if constant, else addi/subi on the forced register
: G-1+  VSP @ 1 - {: k :} VTAG k + c@ 1 = IF VVAL k cells + @ 1 + VVAL k cells + !
   ELSE k V-REG U1R !  U1R @ U1R @ 1 ENC-ADDI EMITW THEN ;
: G-1-  VSP @ 1 - {: k :} VTAG k + c@ 1 = IF VVAL k cells + @ 1 - VVAL k cells + !
   ELSE k V-REG U1R !  U1R @ U1R @ 1 ENC-SUBI EMITW THEN ;
\ dispatch table: index -> emitter xt
create XTS 32 cells allot
: VS-SETUP
   ['] G-DUP 0 cells XTS + !  ['] G-DROP 1 cells XTS + !  ['] G-SWAP 2 cells XTS + !
   ['] G-OVER 3 cells XTS + !  ['] G-NIP 4 cells XTS + !  ['] G-ADD 5 cells XTS + !
   ['] G-SUB 6 cells XTS + !  ['] G-MUL 7 cells XTS + !  ['] G-AND 8 cells XTS + !
   ['] G-OR 9 cells XTS + !  ['] G-XOR 10 cells XTS + !
   ['] G-NEGATE 11 cells XTS + !  ['] G-0< 12 cells XTS + !
   ['] G-IF 13 cells XTS + !  ['] G-THEN 14 cells XTS + !
   ['] G-0= 15 cells XTS + !  ['] G-BEGIN 16 cells XTS + !  ['] G-UNTIL 17 cells XTS + !
   ['] G-@ 18 cells XTS + !  ['] G-! 19 cells XTS + !  ['] G-C@ 20 cells XTS + !
   ['] G-C! 21 cells XTS + !  ['] G-HERE 22 cells XTS + !
   ['] G-EQ 23 cells XTS + !  ['] G-LT 24 cells XTS + !  ['] G-GT 25 cells XTS + !
   ['] G-1+ 26 cells XTS + !  ['] G-1- 27 cells XTS + ! ;
VS-SETUP
\ find op (a,u) in OPN -> index, or -1
variable VFI  variable VFP  variable VFNL
: VFIND {: a u :}  -1 VFI !  OPN VFP !
   BEGIN VFP @ c@ dup WHILE
     VFNL !  a u VFP @ 1 + VFNL @ STR= IF VFP @ 1 + VFNL @ + c@ VFI ! THEN
     VFP @ 1 + VFNL @ + 1 + VFP !
   REPEAT drop  VFI @ ;
: DIG2? {: c :} c 47 > c 58 < and ;
variable VAD
: ALLDG? {: a u :} u 0= IF 0 VAD ! ELSE -1 VAD ! 0 BEGIN dup u < WHILE dup a + c@ DIG2? 0= IF 0 VAD ! THEN 1 + REPEAT drop THEN VAD @ ;
variable VNV  variable VNI
: VNUM {: a u :} 0 VNV ! 0 VNI ! BEGIN VNI @ u < WHILE VNV @ 10 * a VNI @ + c@ 48 - + VNV ! VNI @ 1 + VNI ! REPEAT VNV @ ;
: GEN-VS-TOK {: a u :}  a u ALLDG? IF a u VNUM G-LIT ELSE a u VFIND dup 0 >= IF cells XTS + @ execute ELSE drop THEN THEN ;
variable VB  variable VL  variable VI  variable VSS
: VS-INIT  RP-RESET 0 VSP !  0 CFSP !  19 31 512 ENC-SUBI EMITW ;   \ frame: x19 = sp-512
: VS-WALK {: a u :}  a VB !  u VL !  0 VI !
   BEGIN VI @ VL @ < WHILE
     BEGIN VI @ VL @ < VB @ VI @ + c@ 32 = and WHILE VI @ 1 + VI ! REPEAT
     VI @ VL @ < IF VB @ VI @ + VSS !
       BEGIN VI @ VL @ < VB @ VI @ + c@ 32 <> and WHILE VI @ 1 + VI ! REPEAT
       VSS @ VB @ VI @ + VSS @ - GEN-VS-TOK THEN
   REPEAT ;
: VS-EXIT  VSP @ 1 - V-REG {: r :}  0 r GMOV  16 1 0 MOVZHW EMITW  0 ENC-SVC EMITW ;
: GEN-VS {: a u :}  VS-INIT  a u VS-WALK  VS-EXIT ;
\ runtime input pushed as a REGISTER (so conditionals aren't folded away)
variable GVN
: GEN-VS-N {: a u input :}  VS-INIT
   R-ALLOC GVN !  GVN @ input 65535 and 0 MOVZHW EMITW  GVN @ input 16 rshift 65535 and 1 MOVKHW EMITW
   GVN @ V-PUSHR  a u VS-WALK  VS-EXIT ;
