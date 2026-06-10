\ regstack.fs — abstract value stack with register allocation (the real fix for
\ the T0/T1/T2 register-reuse wall). The top of the data stack lives in POOL
\ registers (or as compile-time constants); the rest stays in Xds memory. Pure
\ arithmetic/shuffle primitives operate on this stack with NO memory traffic —
\ distinct live values get distinct registers, so DUP's copy survives later ops.
\ walk.fs SPILLS the whole VS to memory before anything that isn't a VS primitive
\ (control flow, calls, return-stack ops, unsupported words), so those keep using
\ the proven memory path unchanged. Constants fold here too (a CON op CON folds).
\ Invariant: no two VS entries name the same register (DUP/OVER copy), so reusing
\ a popped operand register as an op's result is always safe.

require templ.fs

\ --- register pool (free inside an AOT word body between spills) ---
create RPOOL  13 , 14 , 15 , 20 , 21 , 22 , 23 , 24 ,
8 constant #RPOOL
create RFREE #RPOOL cells allot
: rp-reset ( -- )  #RPOOL 0 ?do  1 RFREE i cells + !  loop ;
: r-alloc ( -- r )
   #RPOOL 0 ?do  RFREE i cells + @ if  0 RFREE i cells + !  RPOOL i cells + @  unloop exit  then  loop
   1 abort" cg: register pool exhausted (stack too deep for the allocator)" ;
: r-free ( r -- )
   #RPOOL 0 ?do  RPOOL i cells + @ over = if  1 RFREE i cells + !  then  loop  drop ;

\ --- abstract value stack: entries are (tag,val); tag REG holds a register, CON a constant
64 constant VMAX
create VTAG VMAX cells allot   create VVAL VMAX cells allot   variable VSP
0 constant V-REG   1 constant V-CON
: v-reset ( -- )  0 VSP !  rp-reset ;
: v-pushx ( tag val -- )  VVAL VSP @ cells + !  VTAG VSP @ cells + !  1 VSP +! ;
: v-pushr ( r -- )  V-REG swap v-pushx ;
: v-pushc ( n -- )  V-CON swap v-pushx ;
: v-top-tag ( -- tag )  VSP @ 1- cells VTAG + @ ;
: v-top-val ( -- v )    VSP @ 1- cells VVAL + @ ;
: v-2con? ( -- f )
   VSP @ 2 < if false exit then
   VSP @ 1- cells VTAG + @ V-CON =  VSP @ 2 - cells VTAG + @ V-CON = and ;

\ pop the top as a (tag,val) pair; an empty VS loads the memory TOS into a fresh reg
: v-pop ( -- tag val )
   VSP @ 0= if  V-REG  r-alloc dup g-pop  exit then
   -1 VSP +!  VTAG VSP @ cells + @  VVAL VSP @ cells + @ ;
\ pop, materialising the value into a register (CON -> LIT, empty -> memory load)
: v-popr ( -- r )
   v-pop over V-REG = if  nip  else  ( V-CON n ) nip  r-alloc tuck swap LIT64,  then ;
: v-popc ( -- n )  -1 VSP +!  VVAL VSP @ cells + @ ;     \ pop a known CON

\ spill the whole VS to memory (bottom..top), then empty it
: v-spill ( -- )
   VSP @ 0 ?do
      VTAG i cells + @ V-REG = if  VVAL i cells + @ g-push  else  VVAL i cells + @ g-lit  then
   loop  v-reset ;

\ --- primitives (own wordlist; walk.fs prefers these, else spills + old prims) ---
: vbin {: emit fold -- :}                          \ emit:(rd rn rm) ic-gen  fold:(a b -- n)
   v-2con? if  v-popc {: b :} v-popc {: a :}  a b fold execute v-pushc
   else  v-popr {: b :} v-popr {: a :}  a a b emit execute  b r-free  a v-pushr  then ;
: vun {: emit fold -- :}            \ emit:(rd rn -- ) closes over an imm; fold:(a -- n)
   v-top-tag V-CON = VSP @ 0> and if  v-popc fold execute v-pushc
   else  v-popr {: r :}  r r emit execute  r v-pushr  then ;

\ comparisons -> Forth flag (0/-1). Distinct names (templ.fs has its own memory
\ g-cmp in the FORTH wordlist; these must not be shadowed by it).
: vcmp  ( cond -- )  v-popr {: b :} v-popr {: a :}  a b CMP,  a swap CSET,  a SP a SUB,  b r-free  a v-pushr ;
: vcmp0 ( cond -- )  v-popr {: a :}  a 0 CMPI,  a swap CSET,  a SP a SUB,  a v-pushr ;

\ ADD/SUB with a SMALL constant top operand -> immediate (ADDI/SUBI #imm12), no
\ materialisation. (igen = ['] ADDI, etc.)
4096 constant IMM12-MAX
: vaddsub {: rgen igen fold -- :}
   v-2con? if  v-popc {: b :} v-popc {: a :}  a b fold execute v-pushc exit then
   v-top-tag V-CON = if  v-top-val 0 IMM12-MAX within if
      v-popc {: k :} v-popr {: a :}  a a k igen execute  a v-pushr  exit then then
   v-popr {: b :} v-popr {: a :}  a a b rgen execute  b r-free  a v-pushr ;

\ AND/OR/EOR with a const operand that's a valid ARM logical immediate -> #imm form.
: vlogic {: rgen igen fold -- :}
   v-2con? if  v-popc {: b :} v-popc {: a :}  a b fold execute v-pushc exit then
   v-top-tag V-CON = if  v-top-val ENC-LOGIMM if  {: nis :}
      v-popc drop  v-popr {: a :}  a a nis igen execute  a v-pushr  exit
   else drop then then
   v-popr {: b :} v-popr {: a :}  a a b rgen execute  b r-free  a v-pushr ;

\ shuffle helpers (FORTH wordlist so CG-VS words can compose them — calling a
\ CG-VS word by NAME from inside CG-VS would resolve to gforth's builtin instead).
: v-drop1 ( -- )
   VSP @ 0= if  XDS XDS 8 SUBI,  exit then
   v-top-tag V-REG = if  v-popr r-free  else  v-popc drop  then ;
: v-dup1 ( -- )
   VSP @ 0= if  r-alloc {: r :} r g-pop  r v-pushr  r-alloc {: r2 :} r2 r MOV, r2 v-pushr  exit then
   v-top-tag V-CON = if  v-top-val v-pushc exit then
   v-top-val {: r :}  r-alloc {: r2 :} r2 r MOV,  r2 v-pushr ;
: v-swap1 ( -- )  v-pop 2>r  v-pop 2r>  v-pushx  v-pushx ;
: v-nip1  ( -- )  v-pop {: tb vb :}  v-pop {: ta va :}  ta V-REG = if va r-free then  tb vb v-pushx ;
: v-over1 ( -- )
   v-pop {: tb vb :}  v-pop {: ta va :}  ta va v-pushx  tb vb v-pushx
   ta V-CON = if  va v-pushc  else  r-alloc {: r :} r va MOV, r v-pushr  then ;
: v-rot1  ( -- )  v-pop {: tc vc :} v-pop {: tb vb :} v-pop {: ta va :}  tb vb v-pushx tc vc v-pushx ta va v-pushx ;
: v-mrot1 ( -- )  v-pop {: tc vc :} v-pop {: tb vb :} v-pop {: ta va :}  tc vc v-pushx ta va v-pushx tb vb v-pushx ;
: v-2swap1 ( -- )
   v-pop {: td vd :} v-pop {: tc vc :} v-pop {: tb vb :} v-pop {: ta va :}
   tc vc v-pushx td vd v-pushx ta va v-pushx tb vb v-pushx ;

\ floating point: each f64 is one data-stack cell holding the IEEE-754 bits. FP
\ prims move the bits X->D (FMOVXD), compute in the D-register file, and move the
\ result D->X (FMOVDX) back into a pool register. D0/D1 are scratch FP regs.
: vfbin {: emit -- :}                 \ emit:(Dd Dn Dm) e.g. ['] FADD,
   v-popr {: xb :} v-popr {: xa :}
   0 xa FMOVXD,  1 xb FMOVXD,  0 0 1 emit execute  xa 0 FMOVDX,
   xb r-free  xa v-pushr ;
: vfun {: emit -- :}                  \ emit:(Dd Dn) e.g. ['] FNEG,
   v-popr {: xa :}  0 xa FMOVXD,  0 0 emit execute  xa 0 FMOVDX,  xa v-pushr ;
: vfcmp {: cond -- :}                 \ FCMP a,b then flag 0/-1 (cond per FP semantics)
   v-popr {: xb :} v-popr {: xa :}
   0 xa FMOVXD,  1 xb FMOVXD,  0 1 FCMP,  xa cond CSET,  xa SP xa SUB,
   xb r-free  xa v-pushr ;
: vfcmp0 {: cond -- :}                \ FCMP a,#0.0 then flag 0/-1
   v-popr {: xa :}  0 xa FMOVXD,  0 FCMP0,  xa cond CSET,  xa SP xa SUB,  xa v-pushr ;

wordlist constant CG-VS
get-current  CG-VS set-current

: + ['] ADD, ['] ADDI, ['] + vaddsub ;    : - ['] SUB, ['] SUBI, ['] - vaddsub ;
: * ['] MUL, ['] * vbin ;
: AND ['] AND, ['] ANDI, ['] and vlogic ;  : OR ['] ORR, ['] ORRI, ['] or vlogic ;  : XOR ['] EOR, ['] EORI, ['] xor vlogic ;

: 1+ [: 1 ADDI, ;] ['] 1+ vun ;
: 1- [: 1 SUBI, ;] ['] 1- vun ;
: 2* [: 1 LSLI, ;] ['] 2* vun ;
: NEGATE
   v-top-tag V-CON = VSP @ 0> and if  v-popc negate v-pushc
   else  v-popr {: r :}  r SP r SUB,  r v-pushr  then ;
: INVERT
   v-top-tag V-CON = VSP @ 0> and if  v-popc invert v-pushc
   else  v-popr {: r :}  12 0 MOVN,  r r 12 EOR,  r v-pushr  then ;

: DUP v-dup1 ;   : DROP v-drop1 ;   : SWAP v-swap1 ;   : NIP v-nip1 ;   : OVER v-over1 ;
: ROT v-rot1 ;   : -ROT v-mrot1 ;   : 2SWAP v-2swap1 ;
: 2DUP v-over1 v-over1 ;   : 2DROP v-drop1 v-drop1 ;   : TUCK v-swap1 v-over1 ;

\ memory: pointer in a register, LDR/STR; reuse the popped register for the result.
: @  v-popr {: p :} p p 0 LDR,  p v-pushr ;
: c@ v-popr {: p :} p p 0 LDRB, p v-pushr ;
: !  v-popr {: p :} v-popr {: v :}  v p 0 STR,   p r-free v r-free ;
: c! v-popr {: p :} v-popr {: v :}  v p 0 STRB,  p r-free v r-free ;
: +! v-popr {: p :} v-popr {: n :}  r-alloc {: t :}  t p 0 LDR, t t n ADD, t p 0 STR,  p r-free n r-free t r-free ;
\ bump heap (HP = next-free pointer, set by g-heap-init at the program entry)
: HERE   r-alloc {: r :}  r HP 0 ADDI,  r v-pushr ;          \ push current HP
: ALLOT  v-popr {: n :}  HP HP n ADD,  n r-free ;            \ HP += n
: ,      v-popr {: x :}  x HP 0 STR,   HP HP 8 ADDI,  x r-free ;   \ store cell, HP += 8
: C,     v-popr {: x :}  x HP 0 STRB,  HP HP 1 ADDI,  x r-free ;   \ store byte, HP += 1

: LSHIFT
   v-2con? if  v-popc {: s :} v-popc {: v :}  v s lshift v-pushc exit then
   v-top-tag V-CON = if  v-popc {: k :} v-popr {: v :}  v v k LSLI,  v v-pushr
   else  v-popr {: s :} v-popr {: v :}  v v s LSLV,  s r-free  v v-pushr  then ;
: RSHIFT
   v-2con? if  v-popc {: s :} v-popc {: v :}  v s rshift v-pushc exit then
   v-top-tag V-CON = if  v-popc {: k :} v-popr {: v :}  v v k LSRI,  v v-pushr
   else  v-popr {: s :} v-popr {: v :}  v v s LSRV,  s r-free  v v-pushr  then ;

: < C-LT vcmp ;  : > C-GT vcmp ;  : = C-EQ vcmp ;  : <= C-LE vcmp ;  : >= C-GE vcmp ;  : <> C-NE vcmp ;
: U< C-CC vcmp ; : U> C-HI vcmp ;
: 0= C-EQ vcmp0 ; : 0< C-LT vcmp0 ; : 0> C-GT vcmp0 ; : 0<> C-NE vcmp0 ;

\ floating point (f64 bits in a data cell; F< uses MI, F> GT, F= EQ — FP flag semantics)
: F+ ['] FADD, vfbin ;  : F- ['] FSUB, vfbin ;  : F* ['] FMUL, vfbin ;  : F/ ['] FDIV, vfbin ;
: FNEGATE ['] FNEG, vfun ;  : FABS ['] FABS, vfun ;  : FSQRT ['] FSQRT, vfun ;
: F< C-MI vfcmp ;  : F> C-GT vfcmp ;  : F= C-EQ vfcmp ;
: F0< C-MI vfcmp0 ;  : F0= C-EQ vfcmp0 ;
: S>F v-popr {: x :}  0 x SCVTF,  x 0 FMOVDX,  x v-pushr ;
: F>S v-popr {: x :}  0 x FMOVXD,  x 0 FCVTZS,  x v-pushr ;

set-current
