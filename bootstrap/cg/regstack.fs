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

: RP-RESET ( -- )  #RPOOL 0 ?do  1 RFREE i cells + !  loop ;

: R-ALLOC ( -- r )
   #RPOOL 0 ?do  RFREE i cells + @ if  0 RFREE i cells + !  RPOOL i cells + @  unloop exit  then  loop
   1 abort" cg: register pool exhausted (stack too deep for the allocator)" ;

: R-FREE ( r -- )
   #RPOOL 0 ?do  RPOOL i cells + @ over = if  1 RFREE i cells + !  then  loop  drop ;

\ --- FP register pool (D0..D7; the whole program is one entry, so any D-reg is
\ ours to clobber). FP-resident values live here, avoiding GP<->FP round-trips
\ between chained float ops. ---
create DPOOL  0 , 1 , 2 , 3 , 4 , 5 , 6 , 7 ,
8 constant #DPOOL
create DFREE #DPOOL cells allot

: DP-RESET ( -- )  #DPOOL 0 ?do  1 DFREE i cells + !  loop ;

: D-ALLOC ( -- d )
   #DPOOL 0 ?do  DFREE i cells + @ if  0 DFREE i cells + !  DPOOL i cells + @  unloop exit  then  loop
   1 abort" cg: FP register pool exhausted (float stack too deep for the allocator)" ;

: D-FREE ( d -- )
   #DPOOL 0 ?do  DPOOL i cells + @ over = if  1 DFREE i cells + !  then  loop  drop ;

\ --- abstract value stack: entries are (tag,val). REG=a GP register, CON=a
\ compile-time constant, FREG=a D (FP) register holding an f64's bits.
64 constant VMAX
create VTAG VMAX cells allot   create VVAL VMAX cells allot   variable VSP
0 constant V-REG   1 constant V-CON   2 constant V-FREG

: V-RESET ( -- )  0 VSP !  RP-RESET  DP-RESET ;

\ --- register-loop eligibility (cgloop.fs) ---
\ A register-resident loop body must keep its loop-carried values in registers and
\ never reach below the carry into memory. RL-ACTIVE is set while emitting such a
\ body; any empty-VS memory access then trips RL-FAIL, marking the loop ineligible
\ (cgloop.fs rolls back to the memory path). This is sound where the old VSP-delta
\ check was not: a body that underflows but nets back to the same depth is caught.
variable RL-ACTIVE   variable RL-FAIL

: RL-MEM ( -- )  RL-ACTIVE @ if  RL-FAIL on  then ;   \ memory touched below the carry

: V-PUSHX ( tag val -- )  VVAL VSP @ cells + !  VTAG VSP @ cells + !  1 VSP +! ;

: V-PUSHR ( r -- )  V-REG swap V-PUSHX ;

: V-PUSHC ( n -- )  V-CON swap V-PUSHX ;

: V-PUSHF ( d -- )  V-FREG swap V-PUSHX ;

: V-TOP-TAG ( -- tag )  VSP @ 1- cells VTAG + @ ;

: V-TOP-VAL ( -- v )    VSP @ 1- cells VVAL + @ ;

: V-2CON? ( -- f )
   VSP @ 2 < if false exit then
   VSP @ 1- cells VTAG + @ V-CON =  VSP @ 2 - cells VTAG + @ V-CON = and ;

\ pop the top as a (tag,val) pair; an empty VS loads the memory TOS into a fresh reg
: V-POP ( -- tag val )
   VSP @ 0= if  RL-MEM  V-REG  R-ALLOC dup G-POP  exit then
   -1 VSP +!  VTAG VSP @ cells + @  VVAL VSP @ cells + @ ;

\ pop, materialising the value into a GP register. CON -> LIT, empty -> memory
\ load, FREG -> FMOVDX the bits out of the D-register (freeing it).
: V-POPR ( -- r )
   V-POP {: t v :}
   t V-REG  = if  v exit then
   t V-FREG = if  R-ALLOC {: r :}  r v FMOVDX,  v D-FREE  r exit then
   R-ALLOC {: r :}  r v LIT64,  r ;       \ V-CON

: V-POPC ( -- n )  -1 VSP +!  VVAL VSP @ cells + @ ;     \ pop a known CON

\ pop, materialising into a D (FP) register. FREG is already there; REG/CON/empty
\ get FMOVXD'd in (the bits become a float in the D-file).
: V-POPD ( -- d )
   V-TOP-TAG V-FREG = VSP @ 0> and if  V-POPC exit then    \ val IS the d-reg
   V-POPR {: x :}  D-ALLOC {: d :}  d x FMOVXD,  x R-FREE  d ;

\ spill the whole VS to memory (bottom..top), then empty it. FP-resident entries
\ are moved D->GP first (FMOVDX into a scratch GP reg) so the bits reach memory.
: V-SPILL ( -- )
   VSP @ 0 ?do
      VTAG i cells + @ {: t :}  VVAL i cells + @ {: v :}
      t V-REG  = if  v G-PUSH  else
      t V-FREG = if  R-ALLOC {: r :}  r v FMOVDX,  r G-PUSH  r R-FREE  v D-FREE  else
      v G-LIT  then then
   loop  V-RESET ;

\ --- primitives (own wordlist; walk.fs prefers these, else spills + old prims) ---
: VBIN {: emit fold -- :}                          \ emit:(rd rn rm) ic-gen  fold:(a b -- n)
   V-2CON? if  V-POPC {: b :} V-POPC {: a :}  a b fold execute V-PUSHC
   else  V-POPR {: b :} V-POPR {: a :}  a a b emit execute  b R-FREE  a V-PUSHR  then ;

: VUN {: emit fold -- :}            \ emit:(rd rn -- ) closes over an imm; fold:(a -- n)
   V-TOP-TAG V-CON = VSP @ 0> and if  V-POPC fold execute V-PUSHC
   else  V-POPR {: r :}  r r emit execute  r V-PUSHR  then ;

\ comparisons -> Forth flag (0/-1). Distinct names (templ.fs has its own memory
\ g-cmp in the FORTH wordlist; these must not be shadowed by it).
: VCMP  ( cond -- )  V-POPR {: b :} V-POPR {: a :}  a b CMP,  a swap CSET,  a SP a SUB,  b R-FREE  a V-PUSHR ;

: VCMP0 ( cond -- )  V-POPR {: a :}  a 0 CMPI,  a swap CSET,  a SP a SUB,  a V-PUSHR ;

\ ADD/SUB with a SMALL constant top operand -> immediate (ADDI/SUBI #imm12), no
\ materialisation. (igen = ['] ADDI, etc.)
4096 constant IMM12-MAX

: VADDSUB {: rgen igen fold -- :}
   V-2CON? if  V-POPC {: b :} V-POPC {: a :}  a b fold execute V-PUSHC exit then
   V-TOP-TAG V-CON = if  V-TOP-VAL 0 IMM12-MAX within if
      V-POPC {: k :} V-POPR {: a :}  a a k igen execute  a V-PUSHR  exit then then
   V-POPR {: b :} V-POPR {: a :}  a a b rgen execute  b R-FREE  a V-PUSHR ;

\ AND/OR/EOR with a const operand that's a valid ARM logical immediate -> #imm form.
: VLOGIC {: rgen igen fold -- :}
   V-2CON? if  V-POPC {: b :} V-POPC {: a :}  a b fold execute V-PUSHC exit then
   V-TOP-TAG V-CON = if  V-TOP-VAL ENC-LOGIMM if  drop
      V-POPC {: m :}  V-POPR {: a :}  a a m igen execute  a V-PUSHR  exit
   else drop then then
   V-POPR {: b :} V-POPR {: a :}  a a b rgen execute  b R-FREE  a V-PUSHR ;

\ shuffle helpers (FORTH wordlist so CG-VS words can compose them — calling a
\ CG-VS word by NAME from inside CG-VS would resolve to gforth's builtin instead).
: V-DROP1 ( -- )
   VSP @ 0= if  RL-MEM  XDS XDS 8 SUBI,  exit then
   V-TOP-TAG {: t :}
   t V-REG  = if  V-POPR R-FREE  exit then
   t V-FREG = if  V-POPC D-FREE  exit then
   V-POPC drop ;                                  \ V-CON

: V-DUP1 ( -- )
   VSP @ 0= if  RL-MEM  R-ALLOC {: r :} r G-POP  r V-PUSHR  R-ALLOC {: r2 :} r2 r MOV, r2 V-PUSHR  exit then
   V-TOP-TAG {: t :}
   t V-CON  = if  V-TOP-VAL V-PUSHC exit then
   t V-FREG = if  V-TOP-VAL {: d :}  D-ALLOC {: d2 :} d2 d FMOVDD,  d2 V-PUSHF  exit then
   V-TOP-VAL {: r :}  R-ALLOC {: r2 :} r2 r MOV,  r2 V-PUSHR ;

: V-SWAP1 ( -- )  V-POP 2>r  V-POP 2r>  V-PUSHX  V-PUSHX ;

: V-NIP1  ( -- )  V-POP {: tb vb :}  V-POP {: ta va :}
   ta V-REG = if va R-FREE then  ta V-FREG = if va D-FREE then  tb vb V-PUSHX ;

: V-OVER1 ( -- )
   V-POP {: tb vb :}  V-POP {: ta va :}  ta va V-PUSHX  tb vb V-PUSHX
   ta V-CON  = if  va V-PUSHC exit then
   ta V-FREG = if  D-ALLOC {: d :} d va FMOVDD,  d V-PUSHF  exit then
   R-ALLOC {: r :} r va MOV, r V-PUSHR ;

: V-ROT1  ( -- )  V-POP {: tc vc :} V-POP {: tb vb :} V-POP {: ta va :}  tb vb V-PUSHX tc vc V-PUSHX ta va V-PUSHX ;

: V-MROT1 ( -- )  V-POP {: tc vc :} V-POP {: tb vb :} V-POP {: ta va :}  tc vc V-PUSHX ta va V-PUSHX tb vb V-PUSHX ;

: V-2SWAP1 ( -- )
   V-POP {: td vd :} V-POP {: tc vc :} V-POP {: tb vb :} V-POP {: ta va :}
   tc vc V-PUSHX td vd V-PUSHX ta va V-PUSHX tb vb V-PUSHX ;

\ floating point: each f64 is one data-stack cell of IEEE-754 bits. FP prims keep
\ results FP-resident (V-FREG, in a D-register) so chained ops (F+ F* F-) stay in
\ the D-file with no GP round-trips; V-POPD FMOVs a non-resident operand in, the
\ result pushes as V-FREG, and v-popr/v-spill FMOV the bits out only when a GP
\ consumer or control-flow boundary needs them.
: VFBIN {: emit -- :}                 \ emit:(Dd Dn Dm) e.g. ['] FADD,
   V-POPD {: db :} V-POPD {: da :}  da da db emit execute  db D-FREE  da V-PUSHF ;

: VFUN {: emit -- :}                  \ emit:(Dd Dn) e.g. ['] FNEG,
   V-POPD {: da :}  da da emit execute  da V-PUSHF ;

: VFCMP {: cond -- :}                 \ FCMP a,b -> GP flag 0/-1 (cond per FP semantics)
   V-POPD {: db :} V-POPD {: da :}  da db FCMP,
   R-ALLOC {: r :}  r cond CSET,  r SP r SUB,  da D-FREE db D-FREE  r V-PUSHR ;

: VFCMP0 {: cond -- :}                \ FCMP a,#0.0 -> GP flag 0/-1
   V-POPD {: da :}  da FCMP0,  R-ALLOC {: r :}  r cond CSET,  r SP r SUB,  da D-FREE  r V-PUSHR ;

wordlist constant CG-VS
get-current  CG-VS set-current

: + ['] ADD, ['] ADDI, ['] + VADDSUB ;

: - ['] SUB, ['] SUBI, ['] - VADDSUB ;

: * ['] MUL, ['] * VBIN ;

: AND ['] AND, ['] ANDI, ['] and VLOGIC ;

: OR ['] ORR, ['] ORRI, ['] or VLOGIC ;

: XOR ['] EOR, ['] EORI, ['] xor VLOGIC ;

: 1+ [: 1 ADDI, ;] ['] 1+ VUN ;

: 1- [: 1 SUBI, ;] ['] 1- VUN ;

: 2* [: 1 LSLI, ;] ['] 2* VUN ;

: NEGATE
   V-TOP-TAG V-CON = VSP @ 0> and if  V-POPC negate V-PUSHC
   else  V-POPR {: r :}  r SP r SUB,  r V-PUSHR  then ;

: INVERT
   V-TOP-TAG V-CON = VSP @ 0> and if  V-POPC invert V-PUSHC
   else  V-POPR {: r :}  12 0 MOVN,  r r 12 EOR,  r V-PUSHR  then ;

: DUP V-DUP1 ;

: DROP V-DROP1 ;

: SWAP V-SWAP1 ;

: NIP V-NIP1 ;

: OVER V-OVER1 ;

: ROT V-ROT1 ;

: -ROT V-MROT1 ;

: 2SWAP V-2SWAP1 ;

: 2DUP V-OVER1 V-OVER1 ;

: 2DROP V-DROP1 V-DROP1 ;

: TUCK V-SWAP1 V-OVER1 ;

\ memory: pointer in a register, LDR/STR; reuse the popped register for the result.
: @  V-POPR {: p :} p p 0 LDR,  p V-PUSHR ;

: c@ V-POPR {: p :} p p 0 LDRB, p V-PUSHR ;

: !  V-POPR {: p :} V-POPR {: v :}  v p 0 STR,   p R-FREE v R-FREE ;

: c! V-POPR {: p :} V-POPR {: v :}  v p 0 STRB,  p R-FREE v R-FREE ;

: +! V-POPR {: p :} V-POPR {: n :}  R-ALLOC {: t :}  t p 0 LDR, t t n ADD, t p 0 STR,  p R-FREE n R-FREE t R-FREE ;

\ bump heap (HP = next-free pointer, set by G-HEAP-INIT at the program entry)
: HERE   R-ALLOC {: r :}  r HP 0 ADDI,  r V-PUSHR ;          \ push current HP

: ALLOT  V-POPR {: n :}  HP HP n ADD,  n R-FREE ;            \ HP += n

: ,      V-POPR {: x :}  x HP 0 STR,   HP HP 8 ADDI,  x R-FREE ;   \ store cell, HP += 8

: C,     V-POPR {: x :}  x HP 0 STRB,  HP HP 1 ADDI,  x R-FREE ;   \ store byte, HP += 1

: LSHIFT
   V-2CON? if  V-POPC {: s :} V-POPC {: v :}  v s lshift V-PUSHC exit then
   V-TOP-TAG V-CON = if  V-POPC {: k :} V-POPR {: v :}  v v k LSLI,  v V-PUSHR
   else  V-POPR {: s :} V-POPR {: v :}  v v s LSLV,  s R-FREE  v V-PUSHR  then ;

: RSHIFT
   V-2CON? if  V-POPC {: s :} V-POPC {: v :}  v s rshift V-PUSHC exit then
   V-TOP-TAG V-CON = if  V-POPC {: k :} V-POPR {: v :}  v v k LSRI,  v V-PUSHR
   else  V-POPR {: s :} V-POPR {: v :}  v v s LSRV,  s R-FREE  v V-PUSHR  then ;

: < C-LT VCMP ;

: > C-GT VCMP ;

: = C-EQ VCMP ;

: <= C-LE VCMP ;

: >= C-GE VCMP ;

: <> C-NE VCMP ;

: U< C-CC VCMP ;

: U> C-HI VCMP ;

: 0= C-EQ VCMP0 ;

: 0< C-LT VCMP0 ;

: 0> C-GT VCMP0 ;

: 0<> C-NE VCMP0 ;

\ floating point (f64 bits in a data cell; F< uses MI, F> GT, F= EQ — FP flag semantics)
: F+ ['] FADD, VFBIN ;

: F- ['] FSUB, VFBIN ;

: F* ['] FMUL, VFBIN ;

: F/ ['] FDIV, VFBIN ;

: FNEGATE ['] FNEG, VFUN ;

: FABS ['] FABS, VFUN ;

: FSQRT ['] FSQRT, VFUN ;

: F< C-MI VFCMP ;

: F> C-GT VFCMP ;

: F= C-EQ VFCMP ;

: F0< C-MI VFCMP0 ;

: F0= C-EQ VFCMP0 ;

: S>F V-POPR {: x :}  D-ALLOC {: d :}  d x SCVTF,  x R-FREE  d V-PUSHF ;   \ int(GP) -> float(D)

: F>S V-POPD {: d :}  R-ALLOC {: x :}  x d FCVTZS,  d D-FREE  x V-PUSHR ;  \ float(D) -> int(GP)

\ loop index -> fresh VS register (register-resident loops keep the index in LIDX)
: I  R-ALLOC {: r :}  r LIDX MOV,  r V-PUSHR ;

set-current
