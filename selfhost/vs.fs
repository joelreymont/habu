\ vs.fs — a register-allocating code generator for the standalone (caf's VS allocator,
\ ported): the data stack lives in REGISTERS (x9..x15), not memory. swap/over are free
\ (just relabel), arithmetic is reg->reg, and there is NO ldr/str traffic until the
\ pool spills. `5 dup *` becomes ~5 instructions instead of 16. Needs asm.fs + icode.fs.
\ Emitters are dispatched by xt (execute) from a table, so the dispatcher stays tiny.
create OPN 3 c, 100 c, 117 c, 112 c, 0 c, 4 c, 100 c, 114 c, 111 c, 112 c, 1 c, 4 c, 115 c, 119 c, 97 c, 112 c, 2 c, 4 c, 111 c, 118 c, 101 c, 114 c, 3 c, 3 c, 110 c, 105 c, 112 c, 4 c, 1 c, 43 c, 5 c, 1 c, 45 c, 6 c, 1 c, 42 c, 7 c, 3 c, 97 c, 110 c, 100 c, 8 c, 2 c, 111 c, 114 c, 9 c, 3 c, 120 c, 111 c, 114 c, 10 c, 0 c, 
11 constant NOPS
\ register pool x9..x15 (scratch in a leaf body — no calls)
create RPOOL 9 c, 10 c, 11 c, 12 c, 13 c, 14 c, 15 c,
7 constant NRP
create RFREE NRP allot
variable RAI  variable RRES
: RP-RESET 0 RAI ! BEGIN RAI @ NRP < WHILE 1 RAI @ RFREE + c! RAI @ 1 + RAI ! REPEAT ;
: R-ALLOC  -1 RRES !  0 RAI !
   BEGIN RAI @ NRP < RRES @ -1 = and WHILE
     RAI @ RFREE + c@ IF 0 RAI @ RFREE + c!  RAI @ RPOOL + c@ RRES ! THEN  RAI @ 1 + RAI !
   REPEAT  RRES @ ;
: R-FREE {: r :}  0 RAI ! BEGIN RAI @ NRP < WHILE
     RAI @ RPOOL + c@ r = IF 1 RAI @ RFREE + c! THEN  RAI @ 1 + RAI ! REPEAT ;
\ abstract value stack: VS[k] = register holding logical item k
create VS 64 allot   variable VSP
: V-PUSH {: r :}  r VS VSP @ + c!  VSP @ 1 + VSP ! ;
: V-POP   VSP @ 1 - VSP !  VS VSP @ + c@ ;
: V-TOP   VS VSP @ 1 - + c@ ;
: V-NOS   VS VSP @ 2 - + c@ ;
\ emitters
: GMOV {: d s :}  d 31 s ENC-ORR EMITW ;               \ mov d, s  (orr d, xzr, s)
: G-LIT {: n :}  R-ALLOC {: r :}
   r n 65535 and 0 MOVZHW EMITW  r n 16 rshift 65535 and 1 MOVKHW EMITW  r V-PUSH ;
: G-DUP   V-TOP {: s :}  R-ALLOC {: r :}  r s GMOV  r V-PUSH ;
: G-DROP  V-POP R-FREE ;
: G-SWAP  V-TOP {: a :}  V-NOS {: b :}  a VS VSP @ 2 - + c!  b VS VSP @ 1 - + c! ;
: G-OVER  V-NOS {: s :}  R-ALLOC {: r :}  r s GMOV  r V-PUSH ;
: G-NIP   V-POP {: t :}  V-POP R-FREE  t V-PUSH ;
: G-ADD   V-POP {: b :}  V-POP {: a :}  a a b ENC-ADD EMITW  b R-FREE  a V-PUSH ;
: G-SUB   V-POP {: b :}  V-POP {: a :}  a a b ENC-SUB EMITW  b R-FREE  a V-PUSH ;
: G-MUL   V-POP {: b :}  V-POP {: a :}  a a b ENC-MUL EMITW  b R-FREE  a V-PUSH ;
: G-AND   V-POP {: b :}  V-POP {: a :}  a a b ENC-AND EMITW  b R-FREE  a V-PUSH ;
: G-OR    V-POP {: b :}  V-POP {: a :}  a a b ENC-ORR EMITW  b R-FREE  a V-PUSH ;
: G-XOR   V-POP {: b :}  V-POP {: a :}  a a b ENC-EOR EMITW  b R-FREE  a V-PUSH ;
\ dispatch table: index -> emitter xt
create XTS 16 cells allot
: VS-SETUP
   ['] G-DUP 0 cells XTS + !  ['] G-DROP 1 cells XTS + !  ['] G-SWAP 2 cells XTS + !
   ['] G-OVER 3 cells XTS + !  ['] G-NIP 4 cells XTS + !  ['] G-ADD 5 cells XTS + !
   ['] G-SUB 6 cells XTS + !  ['] G-MUL 7 cells XTS + !  ['] G-AND 8 cells XTS + !
   ['] G-OR 9 cells XTS + !  ['] G-XOR 10 cells XTS + ! ;
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
: GEN-VS {: a u :}
   RP-RESET 0 VSP !  a VB !  u VL !  0 VI !
   BEGIN VI @ VL @ < WHILE
     BEGIN VI @ VL @ < VB @ VI @ + c@ 32 = and WHILE VI @ 1 + VI ! REPEAT
     VI @ VL @ < IF VB @ VI @ + VSS !
       BEGIN VI @ VL @ < VB @ VI @ + c@ 32 <> and WHILE VI @ 1 + VI ! REPEAT
       VSS @ VB @ VI @ + VSS @ - GEN-VS-TOK THEN
   REPEAT
   V-POP {: r :}  0 r GMOV  16 1 0 MOVZHW EMITW  0 ENC-SVC EMITW ;
