\ select.f - instruction selection: read a frozen straight-line HIR module and
\ build the frozen A64IR module its operations select to.
\
\ The condition table pairs a source relation with a machine condition, and for
\ a FLOAT comparison it must be one that is false on unordered - mi, gt or
\ equal - because an Fcmp raises unordered for a NaN and the engine's own float
\ comparisons answer false there. src/compiler/native/a64ir.f has the bit table.
\
\ A fused two-way branch wires the CONDITION-TRUE arm as the FIRST successor,
\ which is measured: the other order costs the loop rows four to six per cent.
\
\ The two modules number their symbols, types and blocks separately, so every
\ identity is carried across by translation and never by ordinal.

require lib/prelude.f
require lib/errors.f
require src/compiler/digest.f
require src/compiler/ir/id.f
require src/compiler/ir/context.f
require src/compiler/ir/symbol.f
require src/compiler/ir/type.f
require src/compiler/ir/source.f
require src/compiler/ir/schema.f
require src/compiler/ir/fun.f
require src/compiler/ir/build.f
require src/compiler/a64-effect.f
require src/compiler/native/hir.f
require src/compiler/native/a64ir.f
require src/compiler/native/clobber.f
require src/compiler/native/frame.f
require src/compiler/native/frozen.f
require src/compiler/native/dict.f
require src/compiler/native/trap.f

package A64SEL
using NFROZEN
public

\ One table and not three, because the three questions the passes below ask are
\ the same question about the same operation.
ENUM cmpkind DERIVE eq
   none
   gpr
   freg
   fzero
;ENUM

private

\ ---- the bound source dialect ------------------------------------------------
46 constant OPCODES-N
0 constant O-CONST
1 constant O-ADD
2 constant O-SUB
3 constant O-MUL
4 constant O-RETURN
5 constant O-LT
6 constant O-LE
7 constant O-BR
8 constant O-BRZ
9 constant O-MEM
10 constant O-LOAD
11 constant O-STORE
12 constant O-DIV
13 constant O-BLOAD
14 constant O-BSTORE
15 constant O-EQ
16 constant O-CALL
17 constant O-WORDCALL
18 constant O-GT
19 constant O-GE
20 constant O-NE
21 constant O-AND
22 constant O-OR
23 constant O-XOR
24 constant O-LSHIFT
25 constant O-RSHIFT
26 constant O-INVERT
27 constant O-FCONST
28 constant O-FADD
29 constant O-FSUB
30 constant O-FMUL
31 constant O-FDIV
32 constant O-FNEG
33 constant O-FABS
34 constant O-FSQRT
35 constant O-INTREAL
36 constant O-REALINT
37 constant O-BITSREAL
38 constant O-REALBITS
39 constant O-FLT
40 constant O-FGT
41 constant O-FEQ
42 constant O-FLTZ
43 constant O-FEQZ
44 constant O-TRAP
45 constant O-QUOT

0 constant BOUND-NO
1 constant BOUND-YES

128 constant NAME-CAP

here CELL 1- and CELL swap - CELL 1- and allot
variable BND-MODE
BOUND-NO BND-MODE !

1 TYPED-BUFFER BND-MOD IR-ID:ir-module-id
OPCODES-N TYPED-BUFFER BND-OP IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-VAL IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-ADDR IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-FUN IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-ENTRY IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-IN IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-OUT IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-MEM IR-ID:ir-type-id
1 TYPED-BUFFER BND-REAL IR-ID:ir-type-id

1 TYPED-BUFFER S-CTX IR-CTX:ctx
1 TYPED-BUFFER S-BLD IR-BUILD:builder
1 TYPED-BUFFER S-SID IR-ID:ir-source-id
1 TYPED-BUFFER S-ACC IR-ID:ir-value-id
1 TYPED-BUFFER S-TOK IR-ID:ir-value-id
1 TYPED-BUFFER S-ARGS A64EFF:placeseq
1 TYPED-BUFFER S-OUTS A64EFF:placeseq
1 TYPED-BUFFER S-TRT A64EFF:traits
1 TYPED-BUFFER S-POOL A64EFF:gprs
1 TYPED-BUFFER S-FPOOL A64EFF:fprs
1 TYPED-BUFFER S-FTOK IR-ID:ir-value-id
1 TYPED-BUFFER S-FUN IR-ID:ir-fun-id
1 TYPED-BUFFER S-BLK IR-ID:ir-block-id
variable S-FRAME                     \ the frame the contract declares, in bytes
variable S-DECL-IN                   \ how many values the contract declares the emission takes
variable S-DECL-OUT                  \ and how many it declares it leaves
variable N-CALLS                     \ calls this selection built
variable N-TAILS                     \ tail branches this selection built
variable TAIL-SITE                   \ the operation this function leaves through, or -1
variable N-TRAPS                     \ trap branches this selection built
variable S-TAIL                      \ whether the contract says control leaves through a callee
variable S-DSTACK                    \ whether the contract declares the data-stack convention
variable S-LSAVE                     \ whether the contract's prologue keeps the caller's return address
variable FUSE-AT                     \ where in this block the fused comparison is, or -1
VMAX TYPED-BUFFER VMAP IR-ID:ir-value-id
create VSET VMAX cells allot
create VREAL VMAX cells allot                      \ this cell was placed in the D file
create NAMEBUF NAME-CAP allot

\ ---- what the if-conversion below is working on ------------------------------
\ Package storage rather than per-call, under the single-task discipline.
16 constant SEL-WIDTH-MAX            \ values a converted selection may hand its join
16 constant SEL-BLOCK-MAX            \ blocks a converted region may absorb
4 constant SEL-CARRY-MAX             \ values it may still hold where its selects are made

here CELL 1- and CELL swap - CELL 1- and allot
create R-PRED NFROZEN:BMAX cells allot     \ predecessors this block has
create R-FROM NFROZEN:BMAX cells allot     \ and the last one seen, which is the only
create R-ABSORB NFROZEN:BMAX cells allot   \ this block is inside a converted region
create R-OWNER NFROZEN:BMAX cells allot    \ and this is the head that absorbed it
create R-HEAD NFROZEN:BMAX cells allot     \ this block heads a converted region
create R-EXIT NFROZEN:BMAX cells allot     \ and the region leaves through this block
create R-ORD NFROZEN:BMAX cells allot      \ the machine block this source block became
create R-MARK NFROZEN:BMAX cells allot     \ membership while one region is being tried
create R-QB NFROZEN:BMAX cells allot       \ blocks still to classify
create R-QP NFROZEN:BMAX cells allot       \ and the block each was reached from
create R-LIST NFROZEN:BMAX cells allot     \ the members the current try has taken
variable R-QN
variable R-QI
variable R-LIST-N
variable R-CARRY                     \ values the try's arms still hold where its selects are made
variable R-CARRY-D                   \ how many of them are doubles
variable R-LOCAL                     \ and the widest arm's own locals, one arm's worth at a time
variable R-LOCAL-D                   \ how many of those are doubles
variable R-BLOCAL                    \ the same pair for the one arm being counted
variable R-BLOCAL-D
variable R-JOIN                      \ the exit the current try has found, or -1
variable R-WIDTH                     \ how many values the region hands its exit
variable R-WIDTH-D                   \ how many of those values are doubles
variable R-EXIT-BK                   \ the exit of the region being emitted
variable R-NEXT                      \ the next machine block ordinal to hand out
variable R-S0                        \ the successors of the branch being selected
variable R-S1
variable R-BASE                      \ where this function's blocks start in the module
variable R-NEWBASE                   \ and where they start in the module being built
NFROZEN:BMAX SEL-WIDTH-MAX * TYPED-BUFFER RSEL IR-ID:ir-value-id
1 TYPED-BUFFER R-JB IR-ID:ir-block-id

\ ---- what the data-stack residency pass answers ------------------------------
\ A slot of the caller's data stack holds a value of the source module, and this
\ is the window that fact is tracked over: past it, never resident.
64 constant DSLOT-MAX                \ slots one routine's residency is tracked over
-1 constant DNONE                    \ this slot holds nothing this pass can name
-2 constant DANY                     \ nothing has been said about this slot yet
63 constant DELIDE-MAX               \ store positions one run's elision mask holds

here CELL 1- and CELL swap - CELL 1- and allot
create D-IN NFROZEN:BMAX DSLOT-MAX * cells allot   \ what each slot holds at a block's head
create D-OUT NFROZEN:BMAX DSLOT-MAX * cells allot  \ and at its end
create D-CUR DSLOT-MAX cells allot                 \ the running answer inside one block
create D-NEED VMAX cells allot                     \ this value reaches a register
create D-ORDER-SET NFROZEN:BMAX cells allot        \ an edge has said which order this block is entered with
variable D-MOVED                                   \ a fixpoint round changed something
NFROZEN:BMAX TYPED-BUFFER D-ORDER IR-ID:ir-value-id  \ and that order

\ ---- where the routine's data-stack pointer stands ---------------------------
\ The pointer is a register, so it stands at ONE place for the whole body and
\ the survey chooses the place the fewest adjustments are needed from.
256 constant DREQ-MAX                \ places one routine's survey holds

here CELL 1- and CELL swap - CELL 1- and allot
create D-REQ DREQ-MAX cells allot                  \ the places, with repeats
variable D-REQ-N
variable D-REQ-OVER                                \ the survey ran past its size
variable D-POS                                     \ where the body's pointer stands
variable D-COST                                    \ what standing there costs
variable D-RETS                                    \ returns seen while surveying

\ ---- the slots, read back ----------------------------------------------------
: CTX ( -- IR-CTX:ctx )              0 S-CTX @ ;
: BLD ( -- IR-BUILD:builder )        0 S-BLD @ ;
: SID ( -- IR-ID:ir-source-id )      0 S-SID @ ;
: ACC ( -- IR-ID:ir-value-id )       0 S-ACC @ ;
: ACC! ( IR-ID:ir-value-id -- )      0 S-ACC ! ;
: TOK ( -- IR-ID:ir-value-id )       0 S-TOK @ ;
: TOK! ( IR-ID:ir-value-id -- )      0 S-TOK ! ;
: ARGS ( -- A64EFF:placeseq )        0 S-ARGS @ ;
: OUTS ( -- A64EFF:placeseq )        0 S-OUTS @ ;
: TRAITS ( -- A64EFF:traits )        0 S-TRT @ ;
: TAIL? ( -- bool )                  S-TAIL @ 0<> ;
: FRAME ( -- n )                     S-FRAME @ ;
: FTOK ( -- IR-ID:ir-value-id )      0 S-FTOK @ ;
: FTOK! ( IR-ID:ir-value-id -- )     0 S-FTOK ! ;
: FUN ( -- IR-ID:ir-fun-id )         0 S-FUN @ ;
: BLK ( -- IR-ID:ir-block-id )       0 S-BLK @ ;

\ ---- the source dialect's opcode family --------------------------------------
: SLOT-OF ( HIR:opcode -- n )
   MATCH HIR:opcode
      const  OF O-CONST  ENDOF
      add    OF O-ADD    ENDOF
      sub    OF O-SUB    ENDOF
      mul    OF O-MUL    ENDOF
      div    OF O-DIV    ENDOF
      lt     OF O-LT     ENDOF
      le     OF O-LE     ENDOF
      gt     OF O-GT     ENDOF
      ge     OF O-GE     ENDOF
      equal  OF O-EQ     ENDOF
      ne     OF O-NE     ENDOF
      and    OF O-AND    ENDOF
      or     OF O-OR     ENDOF
      xor    OF O-XOR    ENDOF
      lshift OF O-LSHIFT ENDOF
      rshift OF O-RSHIFT ENDOF
      invert OF O-INVERT ENDOF
      br     OF O-BR     ENDOF
      brz    OF O-BRZ    ENDOF
      mem    OF O-MEM    ENDOF
      load   OF O-LOAD   ENDOF
      store  OF O-STORE  ENDOF
      bload  OF O-BLOAD  ENDOF
      bstore OF O-BSTORE ENDOF
      call   OF O-CALL   ENDOF
      wordcall OF O-WORDCALL ENDOF
      return OF O-RETURN ENDOF
      trap   OF O-TRAP   ENDOF
      fconst   OF O-FCONST   ENDOF
      fadd     OF O-FADD     ENDOF
      fsub     OF O-FSUB     ENDOF
      fmul     OF O-FMUL     ENDOF
      fdiv     OF O-FDIV     ENDOF
      fneg     OF O-FNEG     ENDOF
      fabs     OF O-FABS     ENDOF
      fsqrt    OF O-FSQRT    ENDOF
      intreal  OF O-INTREAL  ENDOF
      realint  OF O-REALINT  ENDOF
      bitsreal OF O-BITSREAL ENDOF
      realbits OF O-REALBITS ENDOF
      flt      OF O-FLT      ENDOF
      fgt      OF O-FGT      ENDOF
      feq      OF O-FEQ      ENDOF
      fltz     OF O-FLTZ     ENDOF
      feqz     OF O-FEQZ     ENDOF
      quot     OF O-QUOT     ENDOF
   ;MATCH ;

: SLOT-OPCODE ( n -- HIR:opcode )
   case
      O-CONST  of HIR-OPCODE:CONST  endof
      O-ADD    of HIR-OPCODE:ADD    endof
      O-SUB    of HIR-OPCODE:SUB    endof
      O-MUL    of HIR-OPCODE:MUL    endof
      O-DIV    of HIR-OPCODE:DIV    endof
      O-LT     of HIR-OPCODE:LT     endof
      O-LE     of HIR-OPCODE:LE     endof
      O-GT     of HIR-OPCODE:GT     endof
      O-GE     of HIR-OPCODE:GE     endof
      O-EQ     of HIR-OPCODE:EQUAL endof
      O-NE     of HIR-OPCODE:NE     endof
      O-AND    of HIR-OPCODE:AND    endof
      O-OR     of HIR-OPCODE:OR     endof
      O-XOR    of HIR-OPCODE:XOR    endof
      O-LSHIFT of HIR-OPCODE:LSHIFT endof
      O-RSHIFT of HIR-OPCODE:RSHIFT endof
      O-INVERT of HIR-OPCODE:INVERT endof
      O-BR     of HIR-OPCODE:BR     endof
      O-BRZ    of HIR-OPCODE:BRZ    endof
      O-MEM    of HIR-OPCODE:MEM    endof
      O-LOAD   of HIR-OPCODE:LOAD   endof
      O-STORE  of HIR-OPCODE:STORE  endof
      O-BLOAD  of HIR-OPCODE:BLOAD  endof
      O-BSTORE of HIR-OPCODE:BSTORE endof
      O-CALL   of HIR-OPCODE:CALL   endof
      O-WORDCALL of HIR-OPCODE:WORDCALL endof
      O-RETURN of HIR-OPCODE:RETURN endof
      O-TRAP   of HIR-OPCODE:TRAP   endof
      O-FCONST   of HIR-OPCODE:FCONST   endof
      O-FADD     of HIR-OPCODE:FADD     endof
      O-FSUB     of HIR-OPCODE:FSUB     endof
      O-FMUL     of HIR-OPCODE:FMUL     endof
      O-FDIV     of HIR-OPCODE:FDIV     endof
      O-FNEG     of HIR-OPCODE:FNEG     endof
      O-FABS     of HIR-OPCODE:FABS     endof
      O-FSQRT    of HIR-OPCODE:FSQRT    endof
      O-INTREAL  of HIR-OPCODE:INTREAL  endof
      O-REALINT  of HIR-OPCODE:REALINT  endof
      O-BITSREAL of HIR-OPCODE:BITSREAL endof
      O-REALBITS of HIR-OPCODE:REALBITS endof
      O-QUOT     of HIR-OPCODE:QUOT     endof
      O-FLT      of HIR-OPCODE:FLT      endof
      O-FGT      of HIR-OPCODE:FGT      endof
      O-FEQ      of HIR-OPCODE:FEQ      endof
      O-FLTZ     of HIR-OPCODE:FLTZ     endof
      O-FEQZ     of HIR-OPCODE:FEQZ     endof
      E-A64SEL-OPCODE throw
   endcase ;

\ A symbol naming no member is an operation this pass has no rule for.
: OPCODE-SLOT ( IR-ID:ir-symbol-id -- n )
   {: sym:IR-ID:ir-symbol-id :}
   -1
   OPCODES-N 0 ?do
      sym i BND-OP @ SAME-SYM? if drop i leave then
   loop
   dup 0 < if E-A64SEL-OPCODE throw then ;

: OP-SLOT ( IR-ID:ir-op-id -- n )
   OPCODE-AT OPCODE-SLOT ;

\ ---- the value map -----------------------------------------------------------
\ Which value of the NEW module a value of the source module selected to.
: VCLEAR ( -- )
   VMAX 0 ?do
      0 i cells VSET + !
      0 i cells VREAL + !
   loop ;

: VSLOT ( IR-ID:ir-value-id -- n )
   IR-ID:VALUE-LOCAL
   dup 0 < over VMAX >= or if E-A64SEL-CAP throw then ;

: VBIND ( IR-ID:ir-value-id IR-ID:ir-value-id -- )
   {: src:IR-ID:ir-value-id new:IR-ID:ir-value-id :}
   src VSLOT {: k:n :}
   new k VMAP !
   1 k cells VSET + ! ;

: VOF ( IR-ID:ir-value-id -- IR-ID:ir-value-id )
   VSLOT {: k:n :}
   k cells VSET + @ 0= if E-A64SEL-SHAPE throw then
   k VMAP @ ;

\ ---- which register file a value's machine value is in -----------------------
\ The source module's TYPE is the answer for every value but one kind.
: FPLACE! ( IR-ID:ir-value-id -- )
   VSLOT cells VREAL + 1 swap ! ;

: FPLACED? ( IR-ID:ir-value-id -- bool )
   VSLOT cells VREAL + @ 0<> ;

\ ---- reading the frozen module -----------------------------------------------
: SRC-CK ( IR-ID:ir-source-id -- )
   IR-ID:SOURCE-LOCAL 0<> if E-A64SEL-SHAPE throw then ;

: OP-SPAN ( IR-ID:ir-op-id -- IR-SOURCE:span )
   {: id:IR-ID:ir-op-id :}
   id SPAN-AT IR--SOURCE-SPAN:UNMAKE
   {: src:IR-ID:ir-source-id st:n ln:n :}
   src SRC-CK
   BLD SID st ln IR-BUILD:ADD-SPAN ;

: FUN-SPAN ( IR-ID:ir-fun-id -- IR-SOURCE:span )
   {: f:IR-ID:ir-fun-id :}
   V-FUNR VW MKEY f IR-FUN:FSPAN@ IR--SOURCE-SPAN:UNMAKE
   {: src:IR-ID:ir-source-id st:n ln:n :}
   src SRC-CK
   BLD SID st ln IR-BUILD:ADD-SPAN ;

: BLOCK-SPAN ( IR-ID:ir-block-id -- IR-SOURCE:span )
   {: bk:IR-ID:ir-block-id :}
   V-BLKR VW MKEY bk IR-FUN:FBLOCK-SPAN@ IR--SOURCE-SPAN:UNMAKE
   {: src:IR-ID:ir-source-id st:n ln:n :}
   src SRC-CK
   BLD SID st ln IR-BUILD:ADD-SPAN ;

: OPERAND ( IR-ID:ir-op-id n -- IR-ID:ir-value-id )
   OPERAND-AT VOF ;

\ The TYPE the source module gives it, held against the identity this pass
\ interned - never a position or an opcode.
: TOKEN? ( IR-ID:ir-value-id -- bool )
   VALUE-TYPE-AT  0 BND-MEM @  SAME-TYPE? ;

\ Asked exactly as TOKEN? is, off the source module's own type.
: REAL? ( IR-ID:ir-value-id -- bool )
   VALUE-TYPE-AT  0 BND-REAL @  SAME-TYPE? ;

\ ---- the residency answers, read back ----------------------------------------
: DSLOT-CK ( n -- n )
   dup 0 < if E-A64SEL-CAP throw then ;

: DIN-WINDOW? ( n -- bool )
   DSLOT-CK DSLOT-MAX < ;

: DRES@ ( n -- n )
   dup DIN-WINDOW? 0= if drop DNONE exit then
   cells D-CUR + @ ;

: DRES! ( IR-ID:ir-value-id n -- )
   {: v:IR-ID:ir-value-id s:n :}
   s DIN-WINDOW? 0= if exit then
   v VSLOT s cells D-CUR + ! ;

\ Whether the cell already held it, which is the whole of what makes a store
\ droppable.
: DPUT? ( IR-ID:ir-value-id n -- bool )
   {: v:IR-ID:ir-value-id s:n :}
   s DRES@  v VSLOT =  {: had:bool :}
   v s DRES!
   had ;

: DKILL ( -- )
   DSLOT-MAX 0 ?do DNONE i cells D-CUR + ! loop ;

\ A memory order holds no register and is never dropped.
: DNEED? ( IR-ID:ir-value-id -- bool )
   dup TOKEN? if drop true exit then
   VSLOT cells D-NEED + @ 0<> ;

: DNEED+ ( IR-ID:ir-value-id -- )
   dup DNEED? if drop exit then
   VSLOT cells D-NEED + 1 swap !
   1 D-MOVED ! ;

\ ---- staging one machine operation -------------------------------------------
\ Every machine operation carries the span of the source operation it selects
\ from, so a diagnostic points at the source and not at the machine.
: OPEN ( IR-ID:ir-op-id A64IR:opcode -- )
   {: id:IR-ID:ir-op-id o:A64IR:opcode :}
   CTX BLD  CTX BLD o A64IR:OPCODE  IR-BUILD:BEGIN-OP
   CTX BLD  id OP-SPAN  IR-BUILD:SET-OP-SPAN ;

: RESULT+ ( -- )
   CTX BLD  CTX BLD A64IR:GPR-TYPE  IR-BUILD:ADD-RESULT ;

: FRESULT+ ( -- )
   CTX BLD  CTX BLD A64IR:FPR-TYPE  IR-BUILD:ADD-RESULT ;

\ Which file a memory access TRANSFERS is read off the FORM the caller staged.
: XFER-RESULT+ ( A64IR:opcode -- )
   {: o:A64IR:opcode :}
   o A64IR-OPCODE:FALOAD A64IR-OPCODE:EQ
   o A64IR-OPCODE:FDLOAD A64IR-OPCODE:EQ or if FRESULT+ exit then
   RESULT+ ;

: TOKEN+ ( -- )
   CTX BLD  CTX BLD A64IR:MEM-TYPE  IR-BUILD:ADD-RESULT ;

: OPERAND+ ( IR-ID:ir-value-id -- )
   CTX BLD rot IR-BUILD:ADD-OPERAND ;

: CLOSE-VALUE ( -- )
   CTX BLD IR-BUILD:END-OP {: id:IR-ID:ir-op-id :}
   CTX BLD id 0 IR-BUILD:OP-RESULT@ ACC! ;

\ ---- the routine's convention, read once -------------------------------------
: SLOT-POSITIONS ( A64EFF:placeseq -- n )
   {: s:A64EFF:placeseq :}
   s A64EFF:SEQ-SLOTS {: sl:n :}
   sl 0= if 0 exit then
   sl s A64EFF:SEQ-LEN <> if E-A64SEL-PLACE throw then
   sl ;

\ THE CONTRACT SAYS SO and this pass does not work it out.
: DSTACK? ( -- bool )
   S-DSTACK @ 0<> ;

\ The declaration decides whether the frame and the link save are built.
: CALLS? ( -- bool )
   TRAITS A64EFF:T-CALL A64EFF:TRAITS-HAS? ;

\ It is LINK-SAVED? and not CALLS? that decides the pair: the pair exists to
\ make `link preserved` true.
: LINK-SAVED? ( -- bool )
   S-LSAVE @ 0<> ;

\ A call site hands the callee its arguments through the caller's data stack, so
\ a contract that declares a call has to declare that convention.
: CONTRACT-CK ( -- )
   CALLS? 0= if exit then
   DSTACK? 0= if E-A64SEL-CALL throw then
   LINK-SAVED? 0= if exit then
   FRAME A64IR:SLOT-WIDTH < if E-A64SEL-CALL throw then ;

\ ---- each function's own boundary --------------------------------------------
\ The CONTRACT describes the published word and the MODULE describes every
\ function, so only function zero is held against the contract.
: FUN-SLOTS ( n -- A64EFF:placeseq )
   {: k:n :}
   A64EFF:SEQ-NONE
   k 0 ?do i A64EFF:SEQ-WITH-SLOT loop ;

: DECL-CK ( n n -- )
   {: in:n out:n :}
   in S-DECL-IN @ <> if E-A64SEL-PLACE throw then
   out S-DECL-OUT @ <> if E-A64SEL-PLACE throw then ;

: FUN-PLACES! ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id ord:n :}
   DSTACK? 0= if exit then
   f NFROZEN:FUN-ARITY {: in:n out:n :}
   ord 0= if in out DECL-CK then
   in FUN-SLOTS 0 S-ARGS !
   out FUN-SLOTS 0 S-OUTS ! ;

\ A contract that declares a call for a body containing none reserves a frame
\ nothing needs.
: CALLED-CK ( -- )
   CALLS? if
      N-CALLS @ 0= if E-A64SEL-CALL throw then exit
   then
   N-CALLS @ 0<> if E-A64SEL-CALL throw then ;

\ The contract declares how control leaves before an operation is selected.
: TAILED-CK ( -- )
   TAIL? if
      N-TAILS @ 1 <> if E-A64SEL-TAIL throw then exit
   then
   N-TAILS @ 0<> if E-A64SEL-TAIL throw then ;

\ ---- which cells are placed in the floating file -----------------------------
\ Both shapes are a reinterpretation that does not have to cost an instruction.
0 constant PLACE-LOAD
1 constant PLACE-STORE

: PLACE-POS? ( n n n -- bool )
   {: k:n ix:n p:n :}
   p PLACE-LOAD = if k O-BITSREAL = ix 0= and exit then
   k O-STORE = ix 0= and
   k O-RETURN = DSTACK? and or ;

: PLACE-OP? ( IR-ID:ir-value-id IR-ID:ir-op-id n -- n )
   {: v:IR-ID:ir-value-id id:IR-ID:ir-op-id p:n :}
   id OPCODE-AT OPCODE-SLOT {: k:n :}
   0
   id OPERANDS-OF 0 ?do
      id i OPERAND-AT v SAME-VALUE? if
         k i p PLACE-POS? if 1+ else drop -1 leave then
      then
   loop ;

: PLACE-BLOCK? ( IR-ID:ir-value-id IR-ID:ir-block-id n -- n )
   {: v:IR-ID:ir-value-id bk:IR-ID:ir-block-id p:n :}
   0
   bk OP-COUNT 0 ?do
      v  bk i OP-AT  p PLACE-OP?
      dup 0 < if drop drop -1 leave then
      +
   loop ;

\ The count and the refusal are one answer, because the rule is about every use.
: PLACE-USES ( IR-ID:ir-value-id n -- n )
   {: v:IR-ID:ir-value-id p:n :}
   0
   FUN BLOCK-COUNT 0 ?do
      v  FUN i BLOCK-AT  p PLACE-BLOCK?
      dup 0 < if drop drop -1 leave then
      +
   loop ;

: PLACE? ( IR-ID:ir-value-id n -- bool )
   PLACE-USES 0 > ;

: LOAD-PLACE? ( IR-ID:ir-value-id -- bool )
   PLACE-LOAD PLACE? ;

: STORE-PLACE? ( IR-ID:ir-value-id -- bool )
   PLACE-STORE PLACE? ;

\ ---- the four data-stack operations ------------------------------------------
: DPLACED ( n -- n )
   D-POS @ - ;

: DSLOT-ATTR+ ( n -- )
   {: off:n :}
   CTX BLD  CTX BLD A64IR:KEY-DSLOT  CTX BLD off DPLACED A64IR:DSLOT-ATTR
   IR-BUILD:ADD-ATTR ;

: DBYTES-ATTR+ ( n -- )
   {: at:n :}
   CTX BLD  CTX BLD A64IR:KEY-DBYTES  CTX BLD at DPLACED A64IR:DBYTES-ATTR
   IR-BUILD:ADD-ATTR ;

\ The pointer is placed where the BODY addresses from, and the order of every
\ data-stack access starts here.
: EMIT-DTAKE ( IR-ID:ir-op-id n -- )
   {: at:IR-ID:ir-op-id bytes:n :}
   at A64IR-OPCODE:DTAKE OPEN
   TOKEN+
   bytes DBYTES-ATTR+
   CTX BLD IR-BUILD:END-OP {: id:IR-ID:ir-op-id :}
   CTX BLD id 0 IR-BUILD:OP-RESULT@ TOK! ;

\ Which FILE it lands in is the placement's answer, not the source type's.
: EMIT-DLOAD ( IR-ID:ir-op-id n A64IR:opcode -- IR-ID:ir-value-id )
   {: at:IR-ID:ir-op-id off:n o:A64IR:opcode :}
   at o OPEN
   TOK OPERAND+
   o XFER-RESULT+
   TOKEN+
   off DSLOT-ATTR+
   CTX BLD IR-BUILD:END-OP {: id:IR-ID:ir-op-id :}
   CTX BLD id 1 IR-BUILD:OP-RESULT@ TOK!
   CTX BLD id 0 IR-BUILD:OP-RESULT@ ;

: EMIT-DSTORE ( IR-ID:ir-op-id IR-ID:ir-value-id n A64IR:opcode -- )
   {: at:IR-ID:ir-op-id v:IR-ID:ir-value-id off:n o:A64IR:opcode :}
   at o OPEN
   v OPERAND+
   TOK OPERAND+
   TOKEN+
   off DSLOT-ATTR+
   CTX BLD IR-BUILD:END-OP {: id:IR-ID:ir-op-id :}
   CTX BLD id 0 IR-BUILD:OP-RESULT@ TOK! ;

: DLOAD-FORM ( bool -- A64IR:opcode )
   if A64IR-OPCODE:FDLOAD exit then A64IR-OPCODE:DLOAD ;

: DSTORE-FORM ( bool -- A64IR:opcode )
   if A64IR-OPCODE:FDSTORE exit then A64IR-OPCODE:DSTORE ;

\ The pointer is left one past the results, which is the moment they become the
\ caller's.
: EMIT-DPUBLISH ( IR-ID:ir-op-id n -- )
   {: at:IR-ID:ir-op-id bytes:n :}
   at A64IR-OPCODE:DPUBLISH OPEN
   TOK OPERAND+
   bytes DBYTES-ATTR+
   CTX BLD IR-BUILD:END-OP drop ;

\ ---- the routine's frame, and the return address in it -----------------------
\ A routine that calls has its own return address destroyed by the call.
: FRAME-ATTR+ ( n -- )
   {: size:n :}
   CTX BLD  CTX BLD A64IR:KEY-FRAME  CTX BLD size A64IR:FRAME-ATTR
   IR-BUILD:ADD-ATTR ;

: SLOT-ATTR+ ( n -- )
   {: off:n :}
   CTX BLD  CTX BLD A64IR:KEY-SLOT  CTX BLD off A64IR:SLOT-ATTR
   IR-BUILD:ADD-ATTR ;

: EMIT-RESERVE ( IR-ID:ir-op-id -- )
   {: at:IR-ID:ir-op-id :}
   at A64IR-OPCODE:RESERVE OPEN
   TOKEN+
   FRAME FRAME-ATTR+
   CTX BLD IR-BUILD:END-OP {: id:IR-ID:ir-op-id :}
   CTX BLD id 0 IR-BUILD:OP-RESULT@ FTOK! ;

: EMIT-RELEASE ( IR-ID:ir-op-id -- )
   {: at:IR-ID:ir-op-id :}
   at A64IR-OPCODE:RELEASE OPEN
   FTOK OPERAND+
   FRAME FRAME-ATTR+
   CTX BLD IR-BUILD:END-OP drop ;

\ Neither takes the register as an operand: it is x30, named by the form.
: EMIT-LINK ( IR-ID:ir-op-id A64IR:opcode -- )
   {: at:IR-ID:ir-op-id o:A64IR:opcode :}
   at o OPEN
   FTOK OPERAND+
   TOKEN+
   A64FRAME:LINK-SLOT SLOT-ATTR+
   CTX BLD IR-BUILD:END-OP {: id:IR-ID:ir-op-id :}
   CTX BLD id 0 IR-BUILD:OP-RESULT@ FTOK! ;

\ Built on LINK-SAVED? and not CALLS?.
: PROLOGUE ( IR-ID:ir-op-id -- )
   {: at:IR-ID:ir-op-id :}
   LINK-SAVED? 0= if exit then
   at EMIT-RESERVE
   at A64IR-OPCODE:LINKSAVE EMIT-LINK ;

: EPILOGUE ( IR-ID:ir-op-id -- )
   {: at:IR-ID:ir-op-id :}
   LINK-SAVED? 0= if exit then
   at A64IR-OPCODE:LINKLOAD EMIT-LINK
   at EMIT-RELEASE ;

\ ---- which operation this function leaves through ----------------------------
\ ONE answer for the whole function, and four passes read it.
: TAIL-OP? ( IR-ID:ir-op-id -- bool )
   {: id:IR-ID:ir-op-id :}
   TAIL-SITE @ 0 < if false exit then
   id IR-ID:OP-LOCAL TAIL-SITE @ = ;

: TAIL-HERE? ( -- bool )
   TAIL-SITE @ 0 < if false exit then
   BLK OP-COUNT {: n:n :}
   n 2 < if false exit then
   BLK n 2 - OP-AT TAIL-OP? ;

\ ---- selecting a call --------------------------------------------------------
\ A call site publishes its arguments through the caller's data stack and takes
\ its results back out of the same cells.
: DBACK-ATTR+ ( n -- )
   {: at:n :}
   CTX BLD  CTX BLD A64IR:KEY-DBACK  CTX BLD at DPLACED A64IR:DBACK-ATTR
   IR-BUILD:ADD-ATTR ;

: CALL-LIVE ( IR-ID:ir-op-id n n -- n )
   {: id:IR-ID:ir-op-id a:n r:n :}
   id OPERANDS-OF 1- a - {: k:n :}
   k 0 < if E-A64SEL-CALL throw then
   id RESULTS-OF 1- r - k <> if E-A64SEL-CALL throw then
   k ;

: EMIT-BL ( IR-ID:ir-op-id n n -- )
   {: at:IR-ID:ir-op-id give:n back:n :}
   at A64IR-OPCODE:CALL OPEN
   TOK OPERAND+
   TOKEN+
   give DBYTES-ATTR+
   back DBACK-ATTR+
   CTX BLD IR-BUILD:END-OP {: id:IR-ID:ir-op-id :}
   CTX BLD id 0 IR-BUILD:OP-RESULT@ TOK! ;

: EMIT-WBL ( IR-ID:ir-op-id n n n -- )
   {: at:IR-ID:ir-op-id give:n back:n entry:n :}
   at A64IR-OPCODE:WORDCALL OPEN
   TOK OPERAND+
   TOKEN+
   give DBYTES-ATTR+
   back DBACK-ATTR+
   CTX BLD  CTX BLD A64IR:KEY-ENTRY  CTX BLD entry A64IR:ENTRY-ATTR
   IR-BUILD:ADD-ATTR
   CTX BLD IR-BUILD:END-OP {: id:IR-ID:ir-op-id :}
   CTX BLD id 0 IR-BUILD:OP-RESULT@ TOK! ;

\ ---- how many live values may stay where they are ----------------------------
: BITS-N ( n -- n )
   {: v:n :}
   0
   A64EFF:FILE-SIZE 0 ?do
      v 1 i lshift and 0<> if 1+ then
   loop ;

: GPR-ROOM ( n -- n )
   {: e:n :}
   0 S-POOL @ A64EFF:GPRS-N {: p:n :}
   e 0 S-POOL @ NCLOB:GPR-CLOB A64EFF:GPRS-N {: c:n :}
   p c invert and BITS-N ;

: FPR-ROOM ( n -- n )
   {: e:n :}
   0 S-FPOOL @ A64EFF:FPRS-N {: p:n :}
   e 0 S-FPOOL @ NCLOB:FPR-CLOB A64EFF:FPRS-N {: c:n :}
   p c invert and BITS-N ;

variable KEPT-G
variable KEPT-F

: KEEP-N ( IR-ID:ir-op-id n n -- n )
   {: id:IR-ID:ir-op-id e:n k:n :}
   e GPR-ROOM {: groom:n :}
   e FPR-ROOM {: froom:n :}
   0 KEPT-G !
   0 KEPT-F !
   0
   k 0 ?do
      id k i - OPERAND-AT REAL? if
         KEPT-F @ 1+ froom > if leave then
         KEPT-F @ 1+ KEPT-F !
      else
         KEPT-G @ 1+ groom > if leave then
         KEPT-G @ 1+ KEPT-G !
      then
      1+
   loop ;

\ ---- what a call site publishes, and what it takes back ----------------------
\ Which value goes into which slot is written down once; three readers stand on it.
: DSAVE-VAL ( IR-ID:ir-op-id n n n -- IR-ID:ir-value-id )
   {: id:IR-ID:ir-op-id kk:n m:n i:n :}
   i kk < if id i 1+ OPERAND-AT exit then
   id m i + 1+ OPERAND-AT ;

: DBACK-VAL ( IR-ID:ir-op-id n n n -- IR-ID:ir-value-id )
   {: id:IR-ID:ir-op-id kk:n m:n i:n :}
   i kk < if id i 1+ RESULT-AT exit then
   id m i + 1+ RESULT-AT ;

\ ---- the residency of one call site ------------------------------------------
\ One bit per position of the store run, set where the cell already held it.
: DBIT? ( n n -- bool )
   {: mask:n i:n :}
   i DELIDE-MAX >= if false exit then
   mask 1 i lshift and 0<> ;

\ A call site's saves are always in the GENERAL file, which follows from the
\ placement rule rather than being a case left out of it.
: CALL-SAVE ( IR-ID:ir-op-id n n n n -- )
   {: id:IR-ID:ir-op-id kk:n m:n a:n mask:n :}
   kk a + 0 ?do
      mask i DBIT? 0= if
         id  id kk m i DSAVE-VAL VOF  i A64IR:SLOT-WIDTH *
         false DSTORE-FORM  EMIT-DSTORE
      then
   loop ;

: CALL-RESTORE ( IR-ID:ir-op-id n n n -- )
   {: id:IR-ID:ir-op-id kk:n m:n r:n :}
   kk r + 0 ?do
      id kk m i DBACK-VAL {: v:IR-ID:ir-value-id :}
      v DNEED? if
         v LOAD-PLACE? if v FPLACE! then
         v  id  i A64IR:SLOT-WIDTH *  v FPLACED? DLOAD-FORM  EMIT-DLOAD  VBIND
      then
   loop
   m 0 ?do
      id kk i + 1+ RESULT-AT   id kk i + 1+ OPERAND   VBIND
   loop
   id 0 RESULT-AT  TOK  VBIND
   N-CALLS @ 1+ N-CALLS ! ;

\ A call to THIS routine keeps nothing: its contract destroys exactly the
\ registers the allocator hands out.
: SELF-SHAPE ( IR-ID:ir-op-id -- n n n n )
   {: id:IR-ID:ir-op-id :}
   ARGS SLOT-POSITIONS {: a:n :}
   OUTS SLOT-POSITIONS {: r:n :}
   id a r CALL-LIVE {: k:n :}
   a r k 0 ;

: EMIT-CALL ( IR-ID:ir-op-id n -- )
   {: id:IR-ID:ir-op-id mask:n :}
   id SELF-SHAPE {: a:n r:n kk:n m:n :}
   id 0 OPERAND TOK!
   id kk m a mask CALL-SAVE
   id  kk a + A64IR:SLOT-WIDTH *  kk r + A64IR:SLOT-WIDTH *  EMIT-BL
   id kk m r CALL-RESTORE ;

\ ---- selecting a call to another word ----------------------------------------
: ATTR-SLOT-OF ( IR-ID:ir-op-id IR-ID:ir-symbol-id -- n )
   {: id:IR-ID:ir-op-id want:IR-ID:ir-symbol-id :}
   -1
   id ATTRS-OF 0 ?do
      id i ATTR-KEY-AT want SAME-SYM? if drop i leave then
   loop
   dup 0 < if E-A64SEL-ATTR throw then ;

: ATTR-INT-OF ( IR-ID:ir-op-id IR-ID:ir-symbol-id -- n )
   {: id:IR-ID:ir-op-id want:IR-ID:ir-symbol-id :}
   id  id want ATTR-SLOT-OF  ATTR-INT-AT ;

: WORD-ENTRY ( IR-ID:ir-op-id -- n )
   0 BND-ENTRY @ ATTR-INT-OF ;

: WORD-SHAPE ( IR-ID:ir-op-id -- n n n n )
   {: id:IR-ID:ir-op-id :}
   id 0 BND-IN @ ATTR-INT-OF {: a:n :}
   id 0 BND-OUT @ ATTR-INT-OF {: r:n :}
   id a r CALL-LIVE {: k:n :}
   id  id WORD-ENTRY  k KEEP-N {: m:n :}
   a r  k m -  m ;

\ The shape of the SITE is not the shape of the operation at the one operation a
\ routine leaves through.
: SITE-SHAPE ( IR-ID:ir-op-id -- n n n n )
   {: id:IR-ID:ir-op-id :}
   id WORD-SHAPE {: a:n r:n kk:n m:n :}
   id TAIL-OP? 0= if a r kk m exit then
   a r 0  kk m + ;

: EMIT-WORD-CALL ( IR-ID:ir-op-id n -- )
   {: id:IR-ID:ir-op-id mask:n :}
   id SITE-SHAPE {: a:n r:n kk:n m:n :}
   id WORD-ENTRY {: e:n :}
   id 0 OPERAND TOK!
   id kk m a mask CALL-SAVE
   id  kk a + A64IR:SLOT-WIDTH *  kk r + A64IR:SLOT-WIDTH *
   e EMIT-WBL
   id kk m r CALL-RESTORE ;

\ ---- the converted region's literal memo -------------------------------------
\ One number materialised twice in one converted region is one value.
32 constant RLIT-MAX                 \ distinct constants one region's memo holds
-1 constant RLIT-SHUT                \ the count when no region is being emitted

here CELL 1- and CELL swap - CELL 1- and allot
create RLIT-SLOT RLIT-MAX cells allot   \ which member of the source family
create RLIT-VAL RLIT-MAX cells allot    \ the number it carried
create RLIT-KIND RLIT-MAX cells allot   \ and what that number IS
RLIT-MAX TYPED-BUFFER RLIT-ID IR-ID:ir-value-id
variable RLIT-N

RLIT-SHUT RLIT-N !

: RLIT-OPEN ( -- )
   0 RLIT-N ! ;

: RLIT-CLOSE ( -- )
   RLIT-SHUT RLIT-N ! ;

: RLIT-ON? ( -- bool )
   RLIT-N @ 0 >= ;

: RLIT-FIND ( n n n -- n )
   {: slot:n kind:n val:n :}
   -1
   RLIT-N @ 0 ?do
      i cells RLIT-SLOT + @ slot =
      i cells RLIT-VAL + @ val = and
      i cells RLIT-KIND + @ kind = and if drop i leave then
   loop ;

: RLIT-REMEMBER ( n n n IR-ID:ir-value-id -- )
   {: slot:n kind:n val:n id:IR-ID:ir-value-id :}
   RLIT-N @ RLIT-MAX >= if exit then
   slot RLIT-N @ cells RLIT-SLOT + !
   val RLIT-N @ cells RLIT-VAL + !
   kind RLIT-N @ cells RLIT-KIND + !
   id RLIT-N @ RLIT-ID !
   RLIT-N @ 1+ RLIT-N ! ;

\ ---- selecting a constant ----------------------------------------------------
: CONST-VALUE ( IR-ID:ir-op-id -- n )
   {: id:IR-ID:ir-op-id :}
   id  0 BND-VAL @  ATTR-INT-OF ;

: CONST-ADDR ( IR-ID:ir-op-id -- n )
   {: id:IR-ID:ir-op-id :}
   id  0 BND-ADDR @  ATTR-INT-OF ;

: MOVE-WIDE ( IR-ID:ir-op-id A64IR:opcode n n bool n -- )
   {: id:IR-ID:ir-op-id o:A64IR:opcode imm:n sh:n keep:bool kind:n :}
   id o OPEN
   keep if CTX BLD ACC IR-BUILD:ADD-OPERAND then
   RESULT+
   CTX BLD  CTX BLD A64IR:KEY-IMM    CTX BLD imm A64IR:IMM-ATTR    IR-BUILD:ADD-ATTR
   CTX BLD  CTX BLD A64IR:KEY-SHIFT  CTX BLD sh A64IR:SHIFT-ATTR   IR-BUILD:ADD-ATTR
   CTX BLD  CTX BLD A64IR:KEY-ADDR   CTX BLD kind A64IR:ADDR-ATTR  IR-BUILD:ADD-ATTR
   CLOSE-VALUE ;

\ A move-wide writes one sixteen-bit half; the shorter of the zero-based and
\ ones-based chains is chosen rather than the first one.
A64IR:IMM-LIMIT 1- constant ONES-HALF

\ The zero chain writes the lowest half and every higher half that is not zero.
: MOVZ-COST ( n -- n )
   {: v:n :}
   1
   A64IR:HALVES 1 ?do
      v i A64IR:HALF-OF 0<> if 1+ then
   loop ;

\ The lowest half that is not already all ones, so the movn does work a movk
\ would otherwise have to do.
: MOVN-HALF ( n -- n )
   {: v:n :}
   0
   A64IR:HALVES 0 ?do
      v i A64IR:HALF-OF ONES-HALF <> if drop i leave then
   loop ;

: MOVN-COST ( n -- n )
   {: v:n :}
   v MOVN-HALF {: s:n :}
   1
   A64IR:HALVES 0 ?do
      i s <> if
         v i A64IR:HALF-OF ONES-HALF <> if 1+ then
      then
   loop ;

: MATERIALISE-Z ( IR-ID:ir-op-id n -- )
   {: id:IR-ID:ir-op-id v:n :}
   id A64IR-OPCODE:MOVZ  v 0 A64IR:HALF-OF  0 A64IR:HALF-SHIFT  false
   A64IR:ADDR-NONE MOVE-WIDE
   A64IR:HALVES 1 ?do
      v i A64IR:HALF-OF 0<> if
         id A64IR-OPCODE:MOVK  v i A64IR:HALF-OF  i A64IR:HALF-SHIFT  true
         A64IR:ADDR-NONE MOVE-WIDE
      then
   loop ;

: MATERIALISE-N ( IR-ID:ir-op-id n -- )
   {: id:IR-ID:ir-op-id v:n :}
   v MOVN-HALF {: s:n :}
   id A64IR-OPCODE:MOVN
   v s A64IR:HALF-OF ONES-HALF xor  s A64IR:HALF-SHIFT  false
   A64IR:ADDR-NONE MOVE-WIDE
   A64IR:HALVES 0 ?do
      i s <> if
         v i A64IR:HALF-OF ONES-HALF <> if
            id A64IR-OPCODE:MOVK  v i A64IR:HALF-OF  i A64IR:HALF-SHIFT  true
            A64IR:ADDR-NONE MOVE-WIDE
         then
      then
   loop ;

\ ---- the carrier a relocation pass can find and rewrite -----------------------
\ An ADDRESS chain is ALWAYS four lanes whatever its halves are, because the
\ relocation pass rewrites four immediates and may not decode region bytes.
: MATERIALISE-ADDR ( IR-ID:ir-op-id n n -- )
   {: id:IR-ID:ir-op-id v:n kind:n :}
   id A64IR-OPCODE:MOVZ  v 0 A64IR:HALF-OF  0 A64IR:HALF-SHIFT  false
   kind MOVE-WIDE
   A64IR:HALVES 1 ?do
      id A64IR-OPCODE:MOVK  v i A64IR:HALF-OF  i A64IR:HALF-SHIFT  true
      kind MOVE-WIDE
   loop ;

\ An address takes the fixed carrier; an ordinary number takes the shorter chain.
: MATERIALISE ( IR-ID:ir-op-id n n -- )
   {: id:IR-ID:ir-op-id v:n kind:n :}
   kind A64IR:ADDR-NONE <> if id v kind MATERIALISE-ADDR exit then
   v MOVN-COST  v MOVZ-COST  < if id v MATERIALISE-N exit then
   id v MATERIALISE-Z ;

: CONST-KIND-OF ( IR-ID:ir-op-id -- n )
   {: id:IR-ID:ir-op-id :}
   id OP-SLOT {: sl:n :}
   sl O-CONST = if id CONST-ADDR exit then
   sl O-FCONST = if HIR:ADDR-NONE exit then
   E-A64SEL-ATTR throw ;

: RLIT-REUSED? ( IR-ID:ir-op-id -- bool )
   {: id:IR-ID:ir-op-id :}
   RLIT-ON? 0= if false exit then
   id OP-SLOT  id CONST-KIND-OF  id CONST-VALUE  RLIT-FIND {: k:n :}
   k 0 < if false exit then
   id 0 RESULT-AT  k RLIT-ID @  VBIND
   true ;

: RLIT-NOTE ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   RLIT-ON? 0= if exit then
   id OP-SLOT  id CONST-KIND-OF  id CONST-VALUE  ACC  RLIT-REMEMBER ;

: EMIT-CONST ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id RLIT-REUSED? if exit then
   id  id CONST-VALUE  id CONST-ADDR  MATERIALISE
   id 0 RESULT-AT  ACC  VBIND
   id RLIT-NOTE ;

: EMIT-FCONST ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id RLIT-REUSED? if exit then
   id  id CONST-VALUE  A64IR:ADDR-NONE  MATERIALISE
   id A64IR-OPCODE:FMOVXD OPEN
   CTX BLD ACC IR-BUILD:ADD-OPERAND
   FRESULT+
   CLOSE-VALUE
   id 0 RESULT-AT  ACC  VBIND
   id RLIT-NOTE ;

\ ---- selecting the arithmetic ------------------------------------------------
: EMIT-BINARY ( IR-ID:ir-op-id A64IR:opcode -- )
   {: id:IR-ID:ir-op-id o:A64IR:opcode :}
   id o OPEN
   CTX BLD  id 0 OPERAND  IR-BUILD:ADD-OPERAND
   CTX BLD  id 1 OPERAND  IR-BUILD:ADD-OPERAND
   RESULT+
   CLOSE-VALUE
   id 0 RESULT-AT  ACC  VBIND ;

: EMIT-FBINARY ( IR-ID:ir-op-id A64IR:opcode -- )
   {: id:IR-ID:ir-op-id o:A64IR:opcode :}
   id o OPEN
   CTX BLD  id 0 OPERAND  IR-BUILD:ADD-OPERAND
   CTX BLD  id 1 OPERAND  IR-BUILD:ADD-OPERAND
   FRESULT+
   CLOSE-VALUE
   id 0 RESULT-AT  ACC  VBIND ;

: EMIT-FUNARY ( IR-ID:ir-op-id A64IR:opcode -- )
   {: id:IR-ID:ir-op-id o:A64IR:opcode :}
   id o OPEN
   CTX BLD  id 0 OPERAND  IR-BUILD:ADD-OPERAND
   FRESULT+
   CLOSE-VALUE
   id 0 RESULT-AT  ACC  VBIND ;

: EMIT-UNARY ( IR-ID:ir-op-id A64IR:opcode -- )
   {: id:IR-ID:ir-op-id o:A64IR:opcode :}
   id o OPEN
   CTX BLD  id 0 OPERAND  IR-BUILD:ADD-OPERAND
   RESULT+
   CLOSE-VALUE
   id 0 RESULT-AT  ACC  VBIND ;

\ ---- selecting the memory operations -----------------------------------------
\ The source order and the machine order are ONE order.
: EMIT-MEM ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   DSTACK? 0= if E-A64SEL-MEM throw then
   id 0 RESULT-AT  TOK  VBIND ;

: EMIT-ALOAD ( IR-ID:ir-op-id A64IR:opcode -- )
   {: id:IR-ID:ir-op-id o:A64IR:opcode :}
   id o OPEN
   CTX BLD  id 0 OPERAND  IR-BUILD:ADD-OPERAND
   CTX BLD  id 1 OPERAND  IR-BUILD:ADD-OPERAND
   o XFER-RESULT+
   TOKEN+
   CTX BLD IR-BUILD:END-OP {: nid:IR-ID:ir-op-id :}
   CTX BLD nid 1 IR-BUILD:OP-RESULT@ {: tk:IR-ID:ir-value-id :}
   tk TOK!
   id 1 RESULT-AT tk VBIND
   id 0 RESULT-AT  CTX BLD nid 0 IR-BUILD:OP-RESULT@  VBIND ;

: EMIT-ASTORE ( IR-ID:ir-op-id A64IR:opcode -- )
   {: id:IR-ID:ir-op-id o:A64IR:opcode :}
   id o OPEN
   CTX BLD  id 0 OPERAND  IR-BUILD:ADD-OPERAND
   CTX BLD  id 1 OPERAND  IR-BUILD:ADD-OPERAND
   CTX BLD  id 2 OPERAND  IR-BUILD:ADD-OPERAND
   TOKEN+
   CTX BLD IR-BUILD:END-OP {: nid:IR-ID:ir-op-id :}
   CTX BLD nid 0 IR-BUILD:OP-RESULT@ {: tk:IR-ID:ir-value-id :}
   tk TOK!
   id 0 RESULT-AT tk VBIND ;

\ ---- the cell load and store, in the file the placement chose ----------------
: EMIT-CELL-LOAD ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id 0 RESULT-AT LOAD-PLACE? if
      id 0 RESULT-AT FPLACE!
      id A64IR-OPCODE:FALOAD EMIT-ALOAD exit
   then
   id A64IR-OPCODE:ALOAD EMIT-ALOAD ;

: EMIT-CELL-STORE ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id 0 OPERAND-AT FPLACED? if
      id A64IR-OPCODE:FASTORE EMIT-ASTORE exit
   then
   id A64IR-OPCODE:ASTORE EMIT-ASTORE ;

\ ---- the two reinterpretations -----------------------------------------------
: EMIT-BITSREAL ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id 0 OPERAND-AT FPLACED? if
      id 0 RESULT-AT  id 0 OPERAND  VBIND exit
   then
   id A64IR-OPCODE:FMOVXD EMIT-FUNARY ;

: EMIT-REALBITS ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id 0 RESULT-AT STORE-PLACE? if
      id 0 RESULT-AT FPLACE!
      id 0 RESULT-AT  id 0 OPERAND  VBIND exit
   then
   id A64IR-OPCODE:FMOVDX EMIT-UNARY ;

\ ---- selecting the return ----------------------------------------------------
\ Under a register convention the values still live where control leaves become
\ the return's operands; under the data-stack one they are stored to cells.
: EMIT-EXIT ( IR-ID:ir-op-id n -- )
   {: id:IR-ID:ir-op-id mask:n :}
   OUTS SLOT-POSITIONS {: r:n :}
   r 0 ?do
      mask i DBIT? 0= if
         id  id i OPERAND  OUTS i A64EFF:SEQ-SLOT@ A64IR:SLOT-WIDTH *
         id i OPERAND-AT FPLACED? DSTORE-FORM  EMIT-DSTORE
      then
   loop
   id  r A64IR:SLOT-WIDTH *  EMIT-DPUBLISH ;

: EMIT-RETURN ( IR-ID:ir-op-id n -- )
   {: id:IR-ID:ir-op-id mask:n :}
   DSTACK? if id mask EMIT-EXIT then
   id EPILOGUE
   id A64IR-OPCODE:RET OPEN
   DSTACK? 0= if
      id OPERANDS-OF {: k:n :}
      k 0 ?do
         CTX BLD  id i OPERAND  IR-BUILD:ADD-OPERAND
      loop
   then
   CTX BLD IR-BUILD:END-OP drop ;

\ ---- selecting a comparison --------------------------------------------------
\ Read off the operation's own condition attribute.
: COMPARE-COND ( IR-ID:ir-op-id -- A64IR:cond )
   OPCODE-AT OPCODE-SLOT
   case
      O-LT of A64IR-COND:LT    endof
      O-LE of A64IR-COND:LE    endof
      O-GT of A64IR-COND:GT    endof
      O-GE of A64IR-COND:GE    endof
      O-EQ of A64IR-COND:EQUAL endof
      O-NE of A64IR-COND:NE    endof
      O-FLT  of A64IR-COND:MI    endof
      O-FGT  of A64IR-COND:GT    endof
      O-FEQ  of A64IR-COND:EQUAL endof
      O-FLTZ of A64IR-COND:MI    endof
      O-FEQZ of A64IR-COND:EQUAL endof
      E-A64SEL-OPCODE throw
   endcase ;

: COMPARE-KIND ( HIR:opcode -- A64SEL:cmpkind )
   MATCH HIR:opcode
      const  OF A64SEL-CMPKIND:NONE ENDOF
      add    OF A64SEL-CMPKIND:NONE ENDOF
      sub    OF A64SEL-CMPKIND:NONE ENDOF
      mul    OF A64SEL-CMPKIND:NONE ENDOF
      div    OF A64SEL-CMPKIND:NONE ENDOF
      lt     OF A64SEL-CMPKIND:GPR  ENDOF
      le     OF A64SEL-CMPKIND:GPR  ENDOF
      gt     OF A64SEL-CMPKIND:GPR  ENDOF
      ge     OF A64SEL-CMPKIND:GPR  ENDOF
      equal  OF A64SEL-CMPKIND:GPR  ENDOF
      ne     OF A64SEL-CMPKIND:GPR  ENDOF
      and    OF A64SEL-CMPKIND:NONE ENDOF
      or     OF A64SEL-CMPKIND:NONE ENDOF
      xor    OF A64SEL-CMPKIND:NONE ENDOF
      lshift OF A64SEL-CMPKIND:NONE ENDOF
      rshift OF A64SEL-CMPKIND:NONE ENDOF
      invert OF A64SEL-CMPKIND:NONE ENDOF
      mem    OF A64SEL-CMPKIND:NONE ENDOF
      load   OF A64SEL-CMPKIND:NONE ENDOF
      store  OF A64SEL-CMPKIND:NONE ENDOF
      bload  OF A64SEL-CMPKIND:NONE ENDOF
      bstore OF A64SEL-CMPKIND:NONE ENDOF
      br     OF A64SEL-CMPKIND:NONE ENDOF
      brz    OF A64SEL-CMPKIND:NONE ENDOF
      call   OF A64SEL-CMPKIND:NONE ENDOF
      wordcall OF A64SEL-CMPKIND:NONE ENDOF
      return OF A64SEL-CMPKIND:NONE ENDOF
      trap   OF A64SEL-CMPKIND:NONE ENDOF
      fconst   OF A64SEL-CMPKIND:NONE ENDOF
      fadd     OF A64SEL-CMPKIND:NONE ENDOF
      fsub     OF A64SEL-CMPKIND:NONE ENDOF
      fmul     OF A64SEL-CMPKIND:NONE ENDOF
      fdiv     OF A64SEL-CMPKIND:NONE ENDOF
      fneg     OF A64SEL-CMPKIND:NONE ENDOF
      fabs     OF A64SEL-CMPKIND:NONE ENDOF
      fsqrt    OF A64SEL-CMPKIND:NONE ENDOF
      flt      OF A64SEL-CMPKIND:FREG  ENDOF
      fgt      OF A64SEL-CMPKIND:FREG  ENDOF
      feq      OF A64SEL-CMPKIND:FREG  ENDOF
      fltz     OF A64SEL-CMPKIND:FZERO ENDOF
      feqz     OF A64SEL-CMPKIND:FZERO ENDOF
      intreal  OF A64SEL-CMPKIND:NONE ENDOF
      realint  OF A64SEL-CMPKIND:NONE ENDOF
      bitsreal OF A64SEL-CMPKIND:NONE ENDOF
      realbits OF A64SEL-CMPKIND:NONE ENDOF
      quot     OF A64SEL-CMPKIND:NONE ENDOF
   ;MATCH ;

: SLOT-KIND ( n -- A64SEL:cmpkind )
   SLOT-OPCODE COMPARE-KIND ;

: COMPARE-SLOT? ( n -- bool )
   SLOT-KIND A64SEL-CMPKIND:NONE A64SEL-CMPKIND:EQ 0= ;

\ The zero comparisons read ONE, because the instruction compares against an
\ immediate the form carries.
: KIND-OPERANDS ( A64SEL:cmpkind -- n )
   MATCH A64SEL:cmpkind
      none  OF E-A64SEL-OPCODE throw ENDOF
      gpr   OF 2 ENDOF
      freg  OF 2 ENDOF
      fzero OF 1 ENDOF
   ;MATCH ;

: KIND-FLAG-OPCODE ( A64SEL:cmpkind -- A64IR:opcode )
   MATCH A64SEL:cmpkind
      none  OF E-A64SEL-OPCODE throw ENDOF
      gpr   OF A64IR-OPCODE:FLAG   ENDOF
      freg  OF A64IR-OPCODE:FFLAG  ENDOF
      fzero OF A64IR-OPCODE:FFLAGZ ENDOF
   ;MATCH ;

: KIND-FUSED-OPCODE ( A64SEL:cmpkind -- A64IR:opcode )
   MATCH A64SEL:cmpkind
      none  OF E-A64SEL-OPCODE throw ENDOF
      gpr   OF A64IR-OPCODE:CMPBR   ENDOF
      freg  OF A64IR-OPCODE:FCMPBR  ENDOF
      fzero OF A64IR-OPCODE:FCMPBRZ ENDOF
   ;MATCH ;

: OP-KIND ( IR-ID:ir-op-id -- A64SEL:cmpkind )
   OPCODE-AT OPCODE-SLOT SLOT-KIND ;

\ One source comparison is one machine comparison, three instructions and one
\ operation, because the flags between them may not be separated.
: EMIT-FLAG ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id OP-KIND {: k:A64SEL:cmpkind :}
   id k KIND-FLAG-OPCODE OPEN
   k KIND-OPERANDS 0 ?do
      CTX BLD  id i OPERAND  IR-BUILD:ADD-OPERAND
   loop
   RESULT+
   CTX BLD  CTX BLD A64IR:KEY-COND
   CTX BLD  id COMPARE-COND  A64IR:COND-ATTR  IR-BUILD:ADD-ATTR
   CLOSE-VALUE
   id 0 RESULT-AT  ACC  VBIND ;

\ ---- selecting the branches --------------------------------------------------
\ A successor names a block of the source module and this pass rebuilds them one
\ for one, so the ordinal carries across.
: BLOCK-ORD-CK ( n -- n )
   dup 0 < over NFROZEN:BMAX >= or if E-A64SEL-CAP throw then ;

: R-ORD-OF ( n -- n )
   BLOCK-ORD-CK cells R-ORD + @
   dup 0 < if E-A64SEL-SHAPE throw then ;

\ ---- which block arguments are built -----------------------------------------
: DIN-AT ( n n -- n )
   {: b:n s:n :}
   s DIN-WINDOW? 0= if DNONE exit then
   b BLOCK-ORD-CK DSLOT-MAX * s + cells D-IN + @ ;

: DIN-AT! ( n n n -- )
   {: v:n b:n s:n :}
   s DIN-WINDOW? 0= if exit then
   v  b BLOCK-ORD-CK DSLOT-MAX * s + cells D-IN + ! ;

: DIN-HOLDS? ( n IR-ID:ir-value-id -- bool )
   {: b:n v:IR-ID:ir-value-id :}
   v VSLOT {: k:n :}
   false
   DSLOT-MAX 0 ?do
      b i DIN-AT k = if drop true leave then
   loop ;

\ Not built when the value lives in a data-stack slot however control arrived
\ AND nothing reads it out of a register.
: DDROP? ( n n -- bool )
   {: b:n i:n :}
   FUN b BLOCK-AT i ARG-AT {: v:IR-ID:ir-value-id :}
   v DNEED? if false exit then
   b v DIN-HOLDS? ;

\ ---- the order a block is entered with ---------------------------------------
\ Every data-stack access threads one order, so a block takes the order its
\ edges stated.
: ORDER-ARG? ( n -- bool )
   {: b:n :}
   false
   FUN b BLOCK-AT ARG-COUNT 0 ?do
      FUN b BLOCK-AT i ARG-AT TOKEN?  b i DDROP? 0=  and if drop true leave then
   loop ;

: ORDER-SET? ( n -- bool )
   BLOCK-ORD-CK cells D-ORDER-SET + @ 0<> ;

: ORDER@ ( n -- IR-ID:ir-value-id )
   BLOCK-ORD-CK D-ORDER @ ;

: ORDER! ( n -- )
   {: b:n :}
   TOK b BLOCK-ORD-CK D-ORDER !
   1 b BLOCK-ORD-CK cells D-ORDER-SET + ! ;

: ORDER-SAME? ( n -- bool )
   ORDER@ IR-ID:VALUE-LOCAL  TOK IR-ID:VALUE-LOCAL  = ;

: ORDER-CLEAR ( -- )
   NFROZEN:BMAX 0 ?do
      0 i cells D-ORDER-SET + !
   loop ;

: ORDER-EDGE! ( n -- )
   {: b:n :}
   DSTACK? 0= if exit then
   b ORDER-ARG? if exit then
   b ORDER-SET? 0= if b ORDER! exit then
   b ORDER-SAME? 0= if E-A64SEL-ORDER throw then ;

\ A block no edge reached is one control cannot arrive at.
: ORDER-ENTER ( n -- )
   {: b:n :}
   DSTACK? 0= if exit then
   b ORDER-ARG? if exit then
   b ORDER-SET? 0= if E-A64SEL-ORDER throw then
   b ORDER@ TOK! ;

\ A block names itself twice over: by its ordinal in the module, which a
\ successor carries, and by its ordinal in its own function.
: SUCC-IDX ( IR-ID:ir-op-id n -- n )
   SUCC-AT IR-ID:BLOCK-LOCAL  R-BASE @ -  BLOCK-ORD-CK ;

: SUCCESSOR-ORD+ ( n -- )
   {: b:n :}
   b ORDER-EDGE!
   CTX BLD
   BLD IR-BUILD:MODULE-KEY  R-NEWBASE @ b R-ORD-OF +  IR-ID:PACK-BLOCK
   IR-BUILD:ADD-SUCCESSOR ;

: SUCCESSOR+ ( IR-ID:ir-op-id n -- )
   {: id:IR-ID:ir-op-id i:n :}
   id i SUCC-IDX SUCCESSOR-ORD+ ;

\ ---- splitting the edges that carry values -----------------------------------
\ A block argument and every value handed to it must end in one register, so
\ every argument-carrying edge is split into one copy per argument.
64 constant EDGE-MAX
EDGE-MAX TYPED-BUFFER EDGE-V IR-ID:ir-value-id

\ The copy is made in the FILE the value lives in.
: EMIT-COPY ( IR-ID:ir-op-id IR-ID:ir-value-id bool -- IR-ID:ir-value-id )
   {: at:IR-ID:ir-op-id v:IR-ID:ir-value-id real:bool :}
   real if
      at A64IR-OPCODE:FMOVDD OPEN
      CTX BLD v IR-BUILD:ADD-OPERAND
      FRESULT+
      CLOSE-VALUE
      ACC exit
   then
   at A64IR-OPCODE:MOV OPEN
   CTX BLD v IR-BUILD:ADD-OPERAND
   RESULT+
   CLOSE-VALUE
   ACC ;

: EDGE-VALUE ( IR-ID:ir-op-id n -- IR-ID:ir-value-id )
   {: id:IR-ID:ir-op-id i:n :}
   id i OPERAND-AT TOKEN? if id i OPERAND exit then
   id  id i OPERAND  id i OPERAND-AT REAL?  EMIT-COPY ;

: BR-TARGET ( IR-ID:ir-op-id -- n )
   0 SUCC-IDX ;

: EMIT-BR ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id OPERANDS-OF {: k:n :}
   k EDGE-MAX > if E-A64SEL-CAP throw then
   id BR-TARGET {: tb:n :}
   0
   k 0 ?do
      tb i DDROP? 0= if
         id i EDGE-VALUE  over EDGE-V !
         1+
      then
   loop
   {: n:n :}
   id A64IR-OPCODE:BR OPEN
   n 0 ?do
      CTX BLD  i EDGE-V @  IR-BUILD:ADD-OPERAND
   loop
   id 0 SUCCESSOR+
   CTX BLD IR-BUILD:END-OP drop ;

: EMIT-BRZ ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id A64IR-OPCODE:BRZ OPEN
   CTX BLD  id 0 OPERAND  IR-BUILD:ADD-OPERAND
   id 0 SUCCESSOR+
   id 1 SUCCESSOR+
   CTX BLD IR-BUILD:END-OP drop ;

\ ---- the selection table -----------------------------------------------------
\ The selector may not lower a may-trap source operation to a form that does not
\ reproduce the trap.
: TRAP-PRESERVED? ( HIR:opcode -- bool )
   MATCH HIR:opcode
      const  OF false ENDOF
      add    OF false ENDOF
      sub    OF false ENDOF
      mul    OF false ENDOF
      div    OF true  ENDOF
      lt     OF false ENDOF
      le     OF false ENDOF
      gt     OF false ENDOF
      ge     OF false ENDOF
      equal  OF false ENDOF
      ne     OF false ENDOF
      and    OF false ENDOF
      or     OF false ENDOF
      xor    OF false ENDOF
      lshift OF false ENDOF
      rshift OF false ENDOF
      invert OF false ENDOF
      mem    OF false ENDOF
      load   OF false ENDOF
      store  OF false ENDOF
      bload  OF false ENDOF
      bstore OF false ENDOF
      br     OF false ENDOF
      brz    OF false ENDOF
      call   OF true  ENDOF
      wordcall OF true ENDOF
      return OF false ENDOF
      trap   OF true  ENDOF
      fconst   OF false ENDOF
      fadd     OF false ENDOF
      fsub     OF false ENDOF
      fmul     OF false ENDOF
      fdiv     OF false ENDOF
      fneg     OF false ENDOF
      fabs     OF false ENDOF
      fsqrt    OF false ENDOF
      intreal  OF false ENDOF
      realint  OF false ENDOF
      bitsreal OF false ENDOF
      realbits OF false ENDOF
      flt      OF false ENDOF
      fgt      OF false ENDOF
      feq      OF false ENDOF
      fltz     OF false ENDOF
      feqz     OF false ENDOF
      quot     OF false ENDOF
   ;MATCH ;

: TRAP-CK ( HIR:opcode IR-ID:ir-symbol-id -- HIR:opcode )
   {: o:HIR:opcode sym:IR-ID:ir-symbol-id :}
   V-SCHR VW sym IR-SCHEMA:FTRAPS? if
      o TRAP-PRESERVED? 0= if E-A64SEL-TRAP throw then
   then
   o ;

\ ---- fusing a comparison into the branch that tests it -----------------------
: USES-IN-OP ( IR-ID:ir-value-id IR-ID:ir-op-id -- n )
   {: v:IR-ID:ir-value-id id:IR-ID:ir-op-id :}
   0
   id OPERANDS-OF 0 ?do
      id i OPERAND-AT v SAME-VALUE? if 1+ then
   loop ;

: USES-IN-BLOCK ( IR-ID:ir-value-id IR-ID:ir-block-id -- n )
   {: v:IR-ID:ir-value-id bk:IR-ID:ir-block-id :}
   0
   bk OP-COUNT 0 ?do
      v  bk i OP-AT  USES-IN-OP  +
   loop ;

\ An operand is the only place a value can be used.
: USES-OF ( IR-ID:ir-value-id -- n )
   {: v:IR-ID:ir-value-id :}
   0
   FUN BLOCK-COUNT 0 ?do
      v  FUN i BLOCK-AT  USES-IN-BLOCK  +
   loop ;

\ Read once per block, before a single operation of the block is selected.
: FUSE-INDEX ( IR-ID:ir-block-id -- n )
   {: bk:IR-ID:ir-block-id :}
   bk OP-COUNT {: n:n :}
   n 2 < if -1 exit then
   bk n 1- OP-AT OPCODE-AT OPCODE-SLOT O-BRZ <> if -1 exit then
   bk n 2 - OP-AT {: d:IR-ID:ir-op-id :}
   d OPCODE-AT OPCODE-SLOT COMPARE-SLOT? 0= if -1 exit then
   d RESULTS-OF 1 <> if -1 exit then
   d 0 RESULT-AT  bk n 1- OP-AT 0 OPERAND-AT  SAME-VALUE? 0= if -1 exit then
   d 0 RESULT-AT USES-OF 1 <> if -1 exit then
   d OPCODE-AT {: sym:IR-ID:ir-symbol-id :}
   sym OPCODE-SLOT SLOT-OPCODE  sym TRAP-CK  drop
   n 2 - ;

: FUSE-SCAN ( IR-ID:ir-block-id -- )
   FUSE-INDEX FUSE-AT ! ;


\ ---- which selections become a select instead of a branch --------------------
: TERM-OP ( IR-ID:ir-block-id -- IR-ID:ir-op-id )
   {: bk:IR-ID:ir-block-id :}
   bk OP-COUNT {: n:n :}
   n 1 < if E-A64SEL-SHAPE throw then
   bk n 1- OP-AT ;

: BRZ-TERM? ( IR-ID:ir-block-id -- bool )
   TERM-OP OP-SLOT O-BRZ = ;

: BR-TERM? ( IR-ID:ir-block-id -- bool )
   TERM-OP OP-SLOT O-BR = ;

\ May this run on a path the program would not have taken? Both halves of the
\ answer are the source dialect's own declarations.
: SPECULABLE? ( IR-ID:ir-op-id -- bool )
   OPCODE-AT {: sym:IR-ID:ir-symbol-id :}
   V-SCHR VW sym IR-SCHEMA:FTRAPS? if false exit then
   V-SCHR VW sym IR-SCHEMA:FEFFECT@
   IR--SCHEMA-EFFECT:PURE IR--SCHEMA-EFFECT:EQ ;

: MEMBER-OK? ( IR-ID:ir-block-id -- bool )
   {: bk:IR-ID:ir-block-id :}
   bk BR-TERM? bk BRZ-TERM? or 0= if false exit then
   bk OP-COUNT 1- {: n:n :}
   true
   n 0 ?do
      bk i OP-AT SPECULABLE? 0= if drop false leave then
   loop ;

\ ---- whether the routine has registers for the form at all -------------------
\ A block's select fuses with the same comparison its branch would.
1 constant SELZ-CMP-REGS             \ the cell a zero-test select reads
2 constant SEL-ARM-REGS              \ the two any of them chooses between

: GPR-POOL-N ( -- n )
   0 S-POOL @ A64EFF:GPRS-N BITS-N ;

: FPR-POOL-N ( -- n )
   0 S-FPOOL @ A64EFF:FPRS-N BITS-N ;

: FUSED-GPR? ( IR-ID:ir-block-id n -- bool )
   {: bk:IR-ID:ir-block-id fz:n :}
   bk fz OP-AT OP-KIND  A64SEL-CMPKIND:GPR A64SEL-CMPKIND:EQ ;

: SEL-CMP-G ( IR-ID:ir-block-id -- n )
   {: bk:IR-ID:ir-block-id :}
   bk FUSE-INDEX {: fz:n :}
   fz 0 < if SELZ-CMP-REGS exit then
   bk fz FUSED-GPR? 0= if 0 exit then
   bk fz OP-AT OP-KIND KIND-OPERANDS ;

: SEL-CMP-F ( IR-ID:ir-block-id -- n )
   {: bk:IR-ID:ir-block-id :}
   bk FUSE-INDEX {: fz:n :}
   fz 0 < if 0 exit then
   bk fz FUSED-GPR? if 0 exit then
   bk fz OP-AT OP-KIND KIND-OPERANDS ;

: SEL-NEED-G ( IR-ID:ir-block-id -- n )
   {: bk:IR-ID:ir-block-id :}
   bk BRZ-TERM? 0= if 0 exit then
   bk SEL-CMP-G
   R-WIDTH @ R-WIDTH-D @ > if SEL-ARM-REGS + then ;

: SEL-NEED-F ( IR-ID:ir-block-id -- n )
   {: bk:IR-ID:ir-block-id :}
   bk BRZ-TERM? 0= if 0 exit then
   bk SEL-CMP-F
   R-WIDTH-D @ 0 > if SEL-ARM-REGS + then ;

\ ---- what the arms of a region really hold at its selects --------------------

\ Which values a converted region holds at once is DECIDED and not assumed.
: R-OP-READS? ( IR-ID:ir-op-id IR-ID:ir-value-id -- bool )
   {: o:IR-ID:ir-op-id v:IR-ID:ir-value-id :}
   false
   o OPERANDS-OF 0 ?do
      o i OPERAND-AT v SAME-VALUE? if drop true leave then
   loop ;

: R-BLOCK-READS? ( IR-ID:ir-block-id IR-ID:ir-value-id -- bool )
   {: bk:IR-ID:ir-block-id v:IR-ID:ir-value-id :}
   false
   bk OP-COUNT 0 ?do
      bk i OP-AT v R-OP-READS? if drop true leave then
   loop ;

: R-OTHERS-READ? ( IR-ID:ir-fun-id n IR-ID:ir-value-id -- bool )
   {: f:IR-ID:ir-fun-id b:n v:IR-ID:ir-value-id :}
   false
   R-LIST-N @ 0 ?do
      i cells R-LIST + @ {: m:n :}
      m b <> if
         f m BLOCK-AT v R-BLOCK-READS? if drop true leave then
      then
   loop ;

: R-CARRIED? ( IR-ID:ir-fun-id n IR-ID:ir-value-id -- bool )
   {: f:IR-ID:ir-fun-id b:n v:IR-ID:ir-value-id :}
   f b BLOCK-AT {: bk:IR-ID:ir-block-id :}
   bk TERM-OP v R-OP-READS? if true exit then
   bk FUSE-INDEX {: fz:n :}
   fz 0 >= if
      bk fz OP-AT v R-OP-READS? if true exit then
   then
   f b v R-OTHERS-READ? ;

: R-CARRY-NOTE ( IR-ID:ir-value-id -- )
   {: v:IR-ID:ir-value-id :}
   R-CARRY @ 1+ R-CARRY !
   v REAL? if R-CARRY-D @ 1+ R-CARRY-D ! then ;

: R-LOCAL-NOTE ( IR-ID:ir-value-id -- )
   {: v:IR-ID:ir-value-id :}
   R-BLOCAL @ 1+ R-BLOCAL !
   v REAL? if R-BLOCAL-D @ 1+ R-BLOCAL-D ! then ;

: R-DEF-NOTE ( IR-ID:ir-fun-id n IR-ID:ir-value-id -- )
   {: f:IR-ID:ir-fun-id b:n v:IR-ID:ir-value-id :}
   f b v R-CARRIED? if v R-CARRY-NOTE exit then
   v R-LOCAL-NOTE ;

\ ---- the literals the region's memo will make one ----------------------------
: R-FOLDABLE? ( IR-ID:ir-op-id -- bool )
   OP-SLOT {: s:n :}
   s O-CONST =  s O-FCONST =  or ;

: R-FOLD-IN? ( IR-ID:ir-block-id n n n -- bool )
   {: bk:IR-ID:ir-block-id k:n s:n v:n :}
   false
   k 0 ?do
      bk i OP-AT {: o:IR-ID:ir-op-id :}
      o R-FOLDABLE? if
         o OP-SLOT s =  o CONST-VALUE v =  and if drop true leave then
      then
   loop ;

: R-FOLD-BEFORE? ( IR-ID:ir-fun-id n n n IR-ID:ir-op-id -- bool )
   {: f:IR-ID:ir-fun-id h:n b:n k:n o:IR-ID:ir-op-id :}
   o OP-SLOT {: s:n :}
   o CONST-VALUE {: v:n :}
   f h BLOCK-AT {: hb:IR-ID:ir-block-id :}
   hb  hb OP-COUNT 1-  s v R-FOLD-IN? if true exit then
   f b BLOCK-AT k s v R-FOLD-IN? if true exit then
   false
   R-LIST-N @ 0 ?do
      i cells R-LIST + @ {: m:n :}
      m b < if
         f m BLOCK-AT {: mb:IR-ID:ir-block-id :}
         mb  mb OP-COUNT 1-  s v R-FOLD-IN? if drop true leave then
      then
   loop ;

: R-FOLD-AFTER? ( IR-ID:ir-fun-id n IR-ID:ir-op-id -- bool )
   {: f:IR-ID:ir-fun-id b:n o:IR-ID:ir-op-id :}
   o OP-SLOT {: s:n :}
   o CONST-VALUE {: v:n :}
   false
   R-LIST-N @ 0 ?do
      i cells R-LIST + @ {: m:n :}
      m b > if
         f m BLOCK-AT {: mb:IR-ID:ir-block-id :}
         mb  mb OP-COUNT 1-  s v R-FOLD-IN? if drop true leave then
      then
   loop ;

: R-OP-DEFS ( IR-ID:ir-fun-id n n n IR-ID:ir-op-id -- )
   {: f:IR-ID:ir-fun-id h:n b:n k:n o:IR-ID:ir-op-id :}
   o R-FOLDABLE? if
      f h b k o R-FOLD-BEFORE? if exit then
      f b o R-FOLD-AFTER? if
         o RESULTS-OF 0 ?do  o i RESULT-AT R-CARRY-NOTE  loop
         exit
      then
   then
   o RESULTS-OF 0 ?do
      f b  o i RESULT-AT  R-DEF-NOTE
   loop ;

: R-COUNT-BLOCK ( IR-ID:ir-fun-id n n -- )
   {: f:IR-ID:ir-fun-id h:n b:n :}
   0 R-BLOCAL !  0 R-BLOCAL-D !
   f b BLOCK-AT {: bk:IR-ID:ir-block-id :}
   bk FUSE-INDEX {: fz:n :}
   bk OP-COUNT 1- {: k:n :}
   k 0 ?do
      i fz <> if  f h b i  bk i OP-AT  R-OP-DEFS  then
   loop
   R-BLOCAL @ R-LOCAL @ max R-LOCAL !
   R-BLOCAL-D @ R-LOCAL-D @ max R-LOCAL-D ! ;

: R-COUNT-DEFS ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id h:n :}
   0 R-CARRY !  0 R-CARRY-D !  0 R-LOCAL !  0 R-LOCAL-D !
   R-LIST-N @ 0 ?do
      f  h  i cells R-LIST + @  R-COUNT-BLOCK
   loop ;

\ ---- the per-block rows, read and written ------------------------------------
: R-PRED@ ( n -- n )     BLOCK-ORD-CK cells R-PRED + @ ;
: R-FROM@ ( n -- n )     BLOCK-ORD-CK cells R-FROM + @ ;
: R-ABSORB? ( n -- bool ) BLOCK-ORD-CK cells R-ABSORB + @ 0<> ;
: R-OWNER@ ( n -- n )    BLOCK-ORD-CK cells R-OWNER + @ ;
: R-HEAD? ( n -- bool )  BLOCK-ORD-CK cells R-HEAD + @ 0<> ;
: R-EXIT@ ( n -- n )     BLOCK-ORD-CK cells R-EXIT + @ ;
: R-MARK? ( n -- bool )  BLOCK-ORD-CK cells R-MARK + @ 0<> ;

: R-RESET1 ( n -- )
   {: b:n :}
   0 b cells R-PRED + !
   -1 b cells R-FROM + !
   0 b cells R-ABSORB + !
   -1 b cells R-OWNER + !
   0 b cells R-HEAD + !
   -1 b cells R-EXIT + !
   -1 b cells R-ORD + !
   0 b cells R-MARK + ! ;

: R-RESET ( -- )
   NFROZEN:BMAX 0 ?do i R-RESET1 loop ;

\ ---- counting the predecessors -----------------------------------------------
: PRED-NOTE ( n n -- )
   {: p:n t:n :}
   t BLOCK-ORD-CK cells R-PRED + dup @ 1+ swap !
   p t BLOCK-ORD-CK cells R-FROM + ! ;

: PRED-NOTE-OP ( n IR-ID:ir-op-id -- )
   {: home:n t:IR-ID:ir-op-id :}
   t SUCCS-OF 0 ?do
      home  t i SUCC-IDX  PRED-NOTE
   loop ;

: PRED-SCAN ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   f BLOCK-COUNT 0 ?do
      i  f i BLOCK-AT TERM-OP  PRED-NOTE-OP
   loop ;

\ ---- growing one region ------------------------------------------------------
: R-TRY-RESET ( -- )
   0 R-QN !
   0 R-QI !
   0 R-LIST-N !
   0 R-CARRY !
   0 R-CARRY-D !
   0 R-LOCAL !
   0 R-LOCAL-D !
   -1 R-JOIN !
   0 R-WIDTH-D !
   NFROZEN:BMAX 0 ?do 0 i cells R-MARK + ! loop ;

: R-PUSH ( n n -- )
   {: p:n b:n :}
   R-QN @ NFROZEN:BMAX >= if E-A64SEL-CAP throw then
   b R-QB R-QN @ cells + !
   p R-QP R-QN @ cells + !
   R-QN @ 1+ R-QN ! ;

: R-TAKE ( n -- )
   {: b:n :}
   R-LIST-N @ NFROZEN:BMAX >= if E-A64SEL-CAP throw then
   1 b BLOCK-ORD-CK cells R-MARK + !
   b R-LIST R-LIST-N @ cells + !
   R-LIST-N @ 1+ R-LIST-N ! ;

: R-JOIN-OK? ( n -- bool )
   {: b:n :}
   R-JOIN @ 0 < if b R-JOIN ! true exit then
   R-JOIN @ b = ;

: R-CLASSIFY ( IR-ID:ir-fun-id n n -- bool )
   {: f:IR-ID:ir-fun-id p:n b:n :}
   b p <= if false exit then
   b R-MARK? if false exit then
   b R-PRED@ 1 <> if b R-JOIN-OK? exit then
   f b BLOCK-AT {: bk:IR-ID:ir-block-id :}
   bk MEMBER-OK? 0= if false exit then
   b R-TAKE
   bk TERM-OP {: t:IR-ID:ir-op-id :}
   t SUCCS-OF 0 ?do
      b  t i SUCC-IDX  R-PUSH
   loop
   true ;

: R-GROW ( IR-ID:ir-fun-id -- bool )
   {: f:IR-ID:ir-fun-id :}
   true
   begin
      dup  R-QI @ R-QN @ <  and
   while
      drop
      R-QI @ {: k:n :}
      k 1+ R-QI !
      f  k cells R-QP + @  k cells R-QB + @  R-CLASSIFY
   repeat ;

\ ---- what a grown region still has to satisfy --------------------------------
: R-WIDTH-OK? ( IR-ID:ir-fun-id -- bool )
   {: f:IR-ID:ir-fun-id :}
   f R-JOIN @ BLOCK-AT {: jb:IR-ID:ir-block-id :}
   jb ARG-COUNT {: w:n :}
   w SEL-WIDTH-MAX > if false exit then
   w R-WIDTH !
   0
   w 0 ?do
      jb i ARG-AT REAL? if 1+ then
   loop
   R-WIDTH-D !
   true ;

: R-EDGE-OK? ( IR-ID:ir-fun-id n -- bool )
   {: f:IR-ID:ir-fun-id b:n :}
   R-WIDTH @ 0= if true exit then
   f b BLOCK-AT {: bk:IR-ID:ir-block-id :}
   bk BRZ-TERM? 0= if true exit then
   bk TERM-OP {: t:IR-ID:ir-op-id :}
   true
   t SUCCS-OF 0 ?do
      t i SUCC-IDX R-MARK? 0= if drop false leave then
   loop ;

: R-EDGES-OK? ( IR-ID:ir-fun-id n -- bool )
   {: f:IR-ID:ir-fun-id h:n :}
   f h R-EDGE-OK? 0= if false exit then
   true
   R-LIST-N @ 0 ?do
      f  i cells R-LIST + @  R-EDGE-OK? 0= if drop false leave then
   loop ;

: R-NEED-G ( IR-ID:ir-fun-id n -- n )
   {: f:IR-ID:ir-fun-id h:n :}
   f h BLOCK-AT SEL-NEED-G
   R-LIST-N @ 0 ?do
      f  i cells R-LIST + @  BLOCK-AT SEL-NEED-G  max
   loop ;

: R-NEED-F ( IR-ID:ir-fun-id n -- n )
   {: f:IR-ID:ir-fun-id h:n :}
   f h BLOCK-AT SEL-NEED-F
   R-LIST-N @ 0 ?do
      f  i cells R-LIST + @  BLOCK-AT SEL-NEED-F  max
   loop ;

\ A converted region computes every arm's values whether or not the path is
\ taken, so the pressure it makes is held against the pool per file.
: R-PRESSURE-OK? ( -- bool )
   GPR-POOL-N
   R-CARRY @ R-CARRY-D @ -  R-LOCAL @ R-LOCAL-D @ -  +
   R-WIDTH @ R-WIDTH-D @ -  +  >= 0= if false exit then
   FPR-POOL-N  R-CARRY-D @ R-LOCAL-D @ +  R-WIDTH-D @ +  >= ;

: R-POOL-OK? ( IR-ID:ir-fun-id n -- bool )
   {: f:IR-ID:ir-fun-id h:n :}
   R-WIDTH @ 0= if true exit then
   GPR-POOL-N  f h R-NEED-G  < if false exit then
   FPR-POOL-N  f h R-NEED-F  < if false exit then
   R-PRESSURE-OK? ;

: R-SHAPE-OK? ( -- bool )
   R-LIST-N @ 0= if false exit then
   R-LIST-N @ SEL-BLOCK-MAX > if false exit then
   R-JOIN @ 0 >= ;

: R-CARRY-OK? ( -- bool )
   R-CARRY @ SEL-CARRY-MAX <= ;

: R-COMMIT ( n -- )
   {: h:n :}
   R-LIST-N @ 0 ?do
      i cells R-LIST + @ {: b:n :}
      1 b BLOCK-ORD-CK cells R-ABSORB + !
      h b BLOCK-ORD-CK cells R-OWNER + !
   loop
   1 h BLOCK-ORD-CK cells R-HEAD + !
   R-JOIN @ h BLOCK-ORD-CK cells R-EXIT + ! ;

\ Nothing is written outside this pass's scratch rows until every question has
\ been answered.
: R-TRY ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id h:n :}
   f h BLOCK-AT {: hb:IR-ID:ir-block-id :}
   hb BRZ-TERM? 0= if exit then
   hb TERM-OP {: t:IR-ID:ir-op-id :}
   t 0 SUCC-IDX {: s0:n :}
   t 1 SUCC-IDX {: s1:n :}
   s0 s1 = if exit then
   R-TRY-RESET
   h s0 R-PUSH
   h s1 R-PUSH
   f R-GROW 0= if exit then
   R-SHAPE-OK? 0= if exit then
   f h R-COUNT-DEFS
   R-CARRY-OK? 0= if exit then
   f R-WIDTH-OK? 0= if exit then
   f h R-EDGES-OK? 0= if exit then
   f h R-POOL-OK? 0= if exit then
   h R-COMMIT ;

\ ---- the plan for one function -----------------------------------------------
: R-NUMBER ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   0 R-NEXT !
   f BLOCK-COUNT 0 ?do
      i R-ABSORB? 0= if
         R-NEXT @ i BLOCK-ORD-CK cells R-ORD + !
         R-NEXT @ 1+ R-NEXT !
      then
   loop ;

\ A function's blocks run on from its base without a gap.
: R-BASE! ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   f 0 BLOCK-AT IR-ID:BLOCK-LOCAL R-BASE !
   f BLOCK-COUNT 0 ?do
      f i BLOCK-AT IR-ID:BLOCK-LOCAL  R-BASE @ -  i <>
      if E-A64SEL-SHAPE throw then
   loop ;

: PLAN-REGIONS ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   R-RESET
   f R-BASE!
   f PRED-SCAN
   f BLOCK-COUNT 0 ?do
      i R-ABSORB? 0= if f i R-TRY then
   loop
   f R-NUMBER ;

\ A Habu flag is all bits set when the relation HOLDS and hir.brz goes to its
\ FIRST successor on zero, so the fused branch swaps them.
: EMIT-CMPBR ( IR-ID:ir-op-id IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id cm:IR-ID:ir-op-id :}
   cm OP-KIND {: k:A64SEL:cmpkind :}
   id k KIND-FUSED-OPCODE OPEN
   k KIND-OPERANDS 0 ?do
      CTX BLD  cm i OPERAND  IR-BUILD:ADD-OPERAND
   loop
   CTX BLD  CTX BLD A64IR:KEY-COND
   CTX BLD  cm COMPARE-COND  A64IR:COND-ATTR  IR-BUILD:ADD-ATTR
   id 1 SUCCESSOR+
   id 0 SUCCESSOR+
   CTX BLD IR-BUILD:END-OP drop ;

: EMIT-BRANCH ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   FUSE-AT @ 0 < if id EMIT-BRZ exit then
   id  BLK FUSE-AT @ OP-AT  EMIT-CMPBR ;

\ The operation immediately in front of the terminator of a block whose
\ terminator is the return.
: TAIL-AT? ( IR-ID:ir-block-id n -- bool )
   {: bk:IR-ID:ir-block-id at:n :}
   TAIL? 0= if false exit then
   bk OP-COUNT {: n:n :}
   n 2 < if false exit then
   at n 2 - <> if false exit then
   bk at OP-AT OP-SLOT O-WORDCALL <> if false exit then
   bk n 1- OP-AT OP-SLOT O-RETURN = ;

: VALUE-READ? ( IR-ID:ir-fun-id IR-ID:ir-value-id -- bool )
   {: f:IR-ID:ir-fun-id v:IR-ID:ir-value-id :}
   false
   f BLOCK-COUNT 0 ?do
      f i BLOCK-AT {: bk:IR-ID:ir-block-id :}
      bk OP-COUNT 0 ?do
         bk i OP-AT {: id:IR-ID:ir-op-id :}
         id OPERANDS-OF 0 ?do
            id i OPERAND-AT v SAME-VALUE? if drop true then
         loop
      loop
   loop ;

\ Nothing the site would carry is read again, which is the one thing a tail
\ branch needs that the source operation does not say.
: TAIL-DEAD-CK ( IR-ID:ir-fun-id IR-ID:ir-op-id n -- )
   {: f:IR-ID:ir-fun-id id:IR-ID:ir-op-id k:n :}
   k 0 ?do
      f  id i 1+ RESULT-AT  VALUE-READ? if E-A64SEL-TAIL throw then
   loop ;

: TAIL-POS ( IR-ID:ir-block-id -- n )
   {: bk:IR-ID:ir-block-id :}
   bk OP-COUNT {: n:n :}
   n 2 < if -1 exit then
   bk n 2 - TAIL-AT? 0= if -1 exit then
   n 2 - ;

: SITE-CROSSED ( IR-ID:ir-op-id -- n )
   WORD-SHAPE {: kk:n m:n :}
   2drop
   kk m + ;

\ A SECOND site in one function is refused rather than one of them chosen.
: TAIL-SITE-TRY ( IR-ID:ir-fun-id IR-ID:ir-block-id -- )
   {: f:IR-ID:ir-fun-id bk:IR-ID:ir-block-id :}
   bk TAIL-POS {: at:n :}
   at 0 < if exit then
   TAIL-SITE @ 0 >= if E-A64SEL-TAIL throw then
   bk at OP-AT {: id:IR-ID:ir-op-id :}
   id IR-ID:OP-LOCAL TAIL-SITE !
   f id  id SITE-CROSSED  TAIL-DEAD-CK ;

\ One derivation per function, because four passes ask the same question.
: TAIL-SITE! ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id ord:n :}
   -1 TAIL-SITE !
   TAIL? 0= if exit then
   ord 0<> if exit then
   f BLOCK-COUNT 0 ?do  f  f i BLOCK-AT  TAIL-SITE-TRY  loop ;

\ ---- leaving through the callee ----------------------------------------------
\ A tail call is not a call site with the end cut off: the pointer must already
\ stand at the callee's entry base.
: TAIL-CK ( n n -- )
   {: a:n r:n :}
   a ARGS SLOT-POSITIONS <> if E-A64SEL-TAIL throw then
   r OUTS SLOT-POSITIONS <> if E-A64SEL-TAIL throw then
   a A64IR:SLOT-WIDTH * DPLACED 0<> if E-A64SEL-TAIL throw then
   r A64IR:SLOT-WIDTH * DPLACED 0<> if E-A64SEL-TAIL throw then ;

: EMIT-TAIL-BR ( IR-ID:ir-op-id n -- )
   {: at:IR-ID:ir-op-id entry:n :}
   at A64IR-OPCODE:TAILCALL OPEN
   TOK OPERAND+
   CTX BLD  CTX BLD A64IR:KEY-ENTRY  CTX BLD entry A64IR:ENTRY-ATTR
   IR-BUILD:ADD-ATTR
   CTX BLD IR-BUILD:END-OP drop ;

\ The epilogue stands in FRONT of the branch and not after it.
: EMIT-TAIL-CALL ( IR-ID:ir-op-id n -- )
   {: id:IR-ID:ir-op-id mask:n :}
   id SITE-SHAPE {: a:n r:n kk:n m:n :}
   a r TAIL-CK
   id 0 OPERAND TOK!
   id kk m a mask CALL-SAVE
   id EPILOGUE
   id  id WORD-ENTRY  EMIT-TAIL-BR
   N-TAILS @ 1+ N-TAILS ! ;


\ ---- leaving through the routine that ends the process -----------------------
\ src/compiler/native/trap.f owns ONE routine tree-wide, resolved by name.
: TRAP-ENTRY ( -- n )
   NTRAP:ROUTINE$ NDICT:CALL-TARGET {: e:n :}
   e 0= if E-A64SEL-TRAP throw then
   e ;

\ Under the trap form's own key and not the one a tail branch carries.
: EMIT-TRAP-BR ( IR-ID:ir-op-id n -- )
   {: at:IR-ID:ir-op-id entry:n :}
   at A64IR-OPCODE:TRAP OPEN
   TOK OPERAND+
   CTX BLD  CTX BLD A64IR:KEY-TRAP-ENTRY  CTX BLD entry A64IR:ENTRY-ATTR
   IR-BUILD:ADD-ATTR
   A64IR:SLOT-WIDTH DBYTES-ATTR+
   CTX BLD IR-BUILD:END-OP drop ;

\ A call site with nothing to save and nothing to take back.
: EMIT-TRAP ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   DSTACK? 0= if E-A64SEL-TRAP throw then
   id  id 0 OPERAND  0  false DSTORE-FORM  EMIT-DSTORE
   id TRAP-ENTRY EMIT-TRAP-BR
   N-TRAPS @ 1+ N-TRAPS ! ;

: EMIT-CALL-OR-TAIL ( IR-ID:ir-op-id n -- )
   {: id:IR-ID:ir-op-id mask:n :}
   id TAIL-OP? if id mask EMIT-TAIL-CALL exit then
   id mask EMIT-WORD-CALL ;

: EMIT-RETURN-OR-TAILED ( IR-ID:ir-op-id n -- )
   {: id:IR-ID:ir-op-id mask:n :}
   TAIL-HERE? if exit then
   id mask EMIT-RETURN ;

\ ---- selecting the address of another function of this emission ---------------
\ The ordinal rides across unchanged: there is no address until the emitter has
\ laid the emission out.
: QUOT-FUN ( IR-ID:ir-op-id -- n )
   {: id:IR-ID:ir-op-id :}
   id ATTRS-OF 1 <> if E-A64SEL-ATTR throw then
   id 0 ATTR-KEY-AT  0 BND-FUN @  SAME-SYM?
   0= if E-A64SEL-ATTR throw then
   id 0 ATTR-INT-AT ;

: EMIT-QUOT ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id A64IR-OPCODE:CODEADDR OPEN
   RESULT+
   CTX BLD  CTX BLD A64IR:KEY-FUN
   CTX BLD  id QUOT-FUN  A64IR:FUN-ATTR  IR-BUILD:ADD-ATTR
   CLOSE-VALUE
   id 0 RESULT-AT  ACC  VBIND ;

\ ---- the effect of one source operation on the map ---------------------------
\ Every walk over a block's operations goes through DOP-XFER, so the fixpoint
\ and the emission read one transfer.
: DSAVE-XFER ( IR-ID:ir-op-id n n n -- n )
   {: id:IR-ID:ir-op-id kk:n m:n a:n :}
   0
   kk a + 0 ?do
      id kk m i DSAVE-VAL  i  DPUT? if
         i DELIDE-MAX < if 1 i lshift or then
      then
   loop ;

\ Every slot the callee could have written stops holding anything this routine
\ can name.
: DBACK-XFER ( IR-ID:ir-op-id n n n -- )
   {: id:IR-ID:ir-op-id kk:n m:n r:n :}
   DKILL
   kk r + 0 ?do
      id kk m i DBACK-VAL  i  DRES!
   loop ;

: DEXIT-XFER ( IR-ID:ir-op-id n -- n )
   {: id:IR-ID:ir-op-id r:n :}
   id OPERANDS-OF r <> if E-A64SEL-PLACE throw then
   0
   r 0 ?do
      id i OPERAND-AT  OUTS i A64EFF:SEQ-SLOT@  DPUT? if
         i DELIDE-MAX < if 1 i lshift or then
      then
   loop ;

: DCALL-XFER ( IR-ID:ir-op-id n n n n -- n )
   {: id:IR-ID:ir-op-id a:n r:n kk:n m:n :}
   id kk m a DSAVE-XFER {: mask:n :}
   id kk m r DBACK-XFER
   mask ;

\ An addressed store has no arm: it destroys nothing this map holds.
: DOP-XFER ( IR-ID:ir-op-id -- n )
   {: id:IR-ID:ir-op-id :}
   id OP-SLOT {: s:n :}
   s O-CALL = if id  id SELF-SHAPE  DCALL-XFER exit then
   s O-WORDCALL = if id  id SITE-SHAPE  DCALL-XFER exit then
   s O-RETURN = if
      DSTACK? 0= if 0 exit then
      id  OUTS SLOT-POSITIONS  DEXIT-XFER exit
   then
   0 ;

\ ---- one source operation, lowered -------------------------------------------
\ The transfer is applied HERE and once.
: RULE ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id OPCODE-AT {: sym:IR-ID:ir-symbol-id :}
   sym OPCODE-SLOT SLOT-OPCODE  sym TRAP-CK
   id DOP-XFER {: mask:n :}
   MATCH HIR:opcode
      const  OF id EMIT-CONST ENDOF
      add    OF id A64IR-OPCODE:ADD EMIT-BINARY ENDOF
      sub    OF id A64IR-OPCODE:SUB EMIT-BINARY ENDOF
      mul    OF id A64IR-OPCODE:MUL EMIT-BINARY ENDOF
      div    OF id A64IR-OPCODE:SDIV EMIT-BINARY ENDOF
      lt     OF id EMIT-FLAG ENDOF
      le     OF id EMIT-FLAG ENDOF
      gt     OF id EMIT-FLAG ENDOF
      ge     OF id EMIT-FLAG ENDOF
      equal  OF id EMIT-FLAG ENDOF
      ne     OF id EMIT-FLAG ENDOF
      and    OF id A64IR-OPCODE:AND EMIT-BINARY ENDOF
      or     OF id A64IR-OPCODE:ORR EMIT-BINARY ENDOF
      xor    OF id A64IR-OPCODE:EOR EMIT-BINARY ENDOF
      lshift OF id A64IR-OPCODE:LSLV EMIT-BINARY ENDOF
      rshift OF id A64IR-OPCODE:LSRV EMIT-BINARY ENDOF
      invert OF id A64IR-OPCODE:MVN EMIT-UNARY ENDOF
      mem    OF id EMIT-MEM ENDOF
      load   OF id EMIT-CELL-LOAD ENDOF
      store  OF id EMIT-CELL-STORE ENDOF
      bload  OF id A64IR-OPCODE:ABLOAD EMIT-ALOAD ENDOF
      bstore OF id A64IR-OPCODE:ABSTORE EMIT-ASTORE ENDOF
      br     OF id EMIT-BR ENDOF
      brz    OF id EMIT-BRANCH ENDOF
      call   OF id mask EMIT-CALL ENDOF
      wordcall OF id mask EMIT-CALL-OR-TAIL ENDOF
      return OF id mask EMIT-RETURN-OR-TAILED ENDOF
      trap   OF id EMIT-TRAP ENDOF
      fconst   OF id EMIT-FCONST ENDOF
      fadd     OF id A64IR-OPCODE:FADD EMIT-FBINARY ENDOF
      fsub     OF id A64IR-OPCODE:FSUB EMIT-FBINARY ENDOF
      fmul     OF id A64IR-OPCODE:FMUL EMIT-FBINARY ENDOF
      fdiv     OF id A64IR-OPCODE:FDIV EMIT-FBINARY ENDOF
      fneg     OF id A64IR-OPCODE:FNEG EMIT-FUNARY ENDOF
      fabs     OF id A64IR-OPCODE:FABS EMIT-FUNARY ENDOF
      fsqrt    OF id A64IR-OPCODE:FSQRT EMIT-FUNARY ENDOF
      flt      OF id EMIT-FLAG ENDOF
      fgt      OF id EMIT-FLAG ENDOF
      feq      OF id EMIT-FLAG ENDOF
      fltz     OF id EMIT-FLAG ENDOF
      feqz     OF id EMIT-FLAG ENDOF
      intreal  OF id A64IR-OPCODE:SCVTF EMIT-FUNARY ENDOF
      realint  OF id A64IR-OPCODE:FCVTZS EMIT-UNARY ENDOF
      bitsreal OF id EMIT-BITSREAL ENDOF
      realbits OF id EMIT-REALBITS ENDOF
      quot     OF id EMIT-QUOT ENDOF
   ;MATCH ;

\ ---- which slot holds which value, over the whole routine ---------------------
\ A slot of the caller's data stack holds a value of the source module.
: DOUT-AT ( n n -- n )
   {: b:n s:n :}
   s DIN-WINDOW? 0= if DNONE exit then
   b BLOCK-ORD-CK DSLOT-MAX * s + cells D-OUT + @ ;

: DOUT-AT! ( n n n -- )
   {: v:n b:n s:n :}
   s DIN-WINDOW? 0= if exit then
   v  b BLOCK-ORD-CK DSLOT-MAX * s + cells D-OUT + ! ;

: DCUR<IN ( n -- )
   {: b:n :}
   DSLOT-MAX 0 ?do  b i DIN-AT  i cells D-CUR + !  loop ;

: DOUT<CUR ( n -- )
   {: b:n :}
   DSLOT-MAX 0 ?do  i cells D-CUR + @  b i DOUT-AT!  loop ;

: DXFER-BLOCK ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id b:n :}
   b DCUR<IN
   f b BLOCK-AT {: bk:IR-ID:ir-block-id :}
   bk OP-COUNT 0 ?do  bk i OP-AT DOP-XFER drop  loop
   b DOUT<CUR ;

\ ---- the meet, and the translation that makes it exact -----------------------
here CELL 1- and CELL swap - CELL 1- and allot
create D-MEET DSLOT-MAX cells allot

: DMEET1 ( n n -- n )
   {: a:n b:n :}
   a DANY = if b exit then
   b DANY = if a exit then
   a b = if a exit then
   DNONE ;

\ A NAME may not cross a backward edge unchanged: the value a slot holds on the
\ second turn is not the value it held on the first.
: DXLATE ( IR-ID:ir-op-id IR-ID:ir-block-id bool n -- n )
   {: t:IR-ID:ir-op-id tb:IR-ID:ir-block-id back:bool v:n :}
   v 0 < if v exit then
   tb ARG-COUNT {: k:n :}
   DANY
   t OP-SLOT O-BR = if
      t OPERANDS-OF k = if
         k 0 ?do
            t i OPERAND-AT VSLOT v = if
               drop  tb i ARG-AT VSLOT  leave
            then
         loop
      then
   then
   dup DANY <> if exit then
   drop
   back if DNONE exit then
   v ;

: DMEET-EDGE ( IR-ID:ir-op-id IR-ID:ir-block-id n n -- )
   {: t:IR-ID:ir-op-id tb:IR-ID:ir-block-id p:n b:n :}
   b p <= {: back:bool :}
   DSLOT-MAX 0 ?do
      i cells D-MEET + @
      t tb  back  p i DOUT-AT  DXLATE
      DMEET1
      i cells D-MEET + !
   loop ;

: DEDGE? ( IR-ID:ir-op-id n -- bool )
   {: t:IR-ID:ir-op-id b:n :}
   false
   t SUCCS-OF 0 ?do
      t i SUCC-IDX b = if drop true leave then
   loop ;

: DMEET-FROM ( IR-ID:ir-fun-id n n -- )
   {: f:IR-ID:ir-fun-id p:n b:n :}
   f p BLOCK-AT TERM-OP {: t:IR-ID:ir-op-id :}
   t b DEDGE? 0= if exit then
   t  f b BLOCK-AT  p b DMEET-EDGE ;

: DIN-SET? ( n n n -- bool )
   {: v:n b:n s:n :}
   b s DIN-AT v = if false exit then
   v b s DIN-AT!
   true ;

: DMEET-BLOCK ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id b:n :}
   DSLOT-MAX 0 ?do DANY i cells D-MEET + ! loop
   f BLOCK-COUNT 0 ?do  f i b DMEET-FROM  loop
   DSLOT-MAX 0 ?do
      i cells D-MEET + @  b i DIN-SET? if 1 D-MOVED ! then
   loop ;

\ ---- the entry map, and the fixpoint over the rest ---------------------------
: DIN-ANY ( n -- )
   {: b:n :}
   DSLOT-MAX 0 ?do DANY b i DIN-AT! loop ;

: DENTRY-IN ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   DSLOT-MAX 0 ?do DNONE 0 i DIN-AT! loop
   DSTACK? 0= if exit then
   f 0 BLOCK-AT {: bk:IR-ID:ir-block-id :}
   ARGS SLOT-POSITIONS {: a:n :}
   bk ARG-COUNT a <> if E-A64SEL-PLACE throw then
   a 0 ?do
      bk i ARG-AT VSLOT  0  ARGS i A64EFF:SEQ-SLOT@  DIN-AT!
   loop ;

: DIN-INIT ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   f BLOCK-COUNT 1 ?do i DIN-ANY loop
   f DENTRY-IN ;

: DRES-ROUND ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   f BLOCK-COUNT 1 ?do  f i DMEET-BLOCK  loop
   f BLOCK-COUNT 0 ?do  f i DXFER-BLOCK  loop ;

\ Every cell starts at "nothing said", may name one value, and may then fall to
\ "nothing", so the descent has a bounded number of rounds.
NFROZEN:BMAX DSLOT-MAX * 2 * 2 + constant DRES-ROUNDS

: DRES-FIX ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   f DIN-INIT
   f BLOCK-COUNT 0 ?do f i DXFER-BLOCK loop
   0
   begin
      1 D-MOVED !
      dup DRES-ROUNDS >= if E-A64SEL-CAP throw then
      0 D-MOVED !
      f DRES-ROUND
      1+
      D-MOVED @ 0=
   until
   drop ;

\ ---- which values reach a register -------------------------------------------
: DNEED-CLEAR ( -- )
   VMAX 0 ?do 0 i cells D-NEED + ! loop ;

\ The routine's own interface is not this pass's to change.
: DNEED-ENTRY ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   DSTACK? if exit then
   f 0 BLOCK-AT {: bk:IR-ID:ir-block-id :}
   bk ARG-COUNT 0 ?do bk i ARG-AT DNEED+ loop ;

: DNEED-OPERANDS ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id OPERANDS-OF 0 ?do id i OPERAND-AT DNEED+ loop ;

\ A value the site writes down needs a register to be written out of; one it
\ KEEPS needs one to stay in across the branch.
: DNEED-CALL ( IR-ID:ir-op-id n n n n n -- )
   {: id:IR-ID:ir-op-id mask:n a:n r:n kk:n m:n :}
   id 0 OPERAND-AT DNEED+
   kk a + 0 ?do
      mask i DBIT? 0= if id kk m i DSAVE-VAL DNEED+ then
   loop
   id TAIL-OP? if exit then
   m 0 ?do id kk i + 1+ OPERAND-AT DNEED+ loop ;

: DNEED-EXIT ( IR-ID:ir-op-id n -- )
   {: id:IR-ID:ir-op-id mask:n :}
   DSTACK? 0= if id DNEED-OPERANDS exit then
   OUTS SLOT-POSITIONS 0 ?do
      mask i DBIT? 0= if id i OPERAND-AT DNEED+ then
   loop ;

: DNEED-OP ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id DOP-XFER {: mask:n :}
   id OP-SLOT {: s:n :}
   s O-BR = if exit then
   s O-CALL = if id mask  id SELF-SHAPE  DNEED-CALL exit then
   s O-WORDCALL = if id mask  id SITE-SHAPE  DNEED-CALL exit then
   s O-RETURN = if id mask DNEED-EXIT exit then
   s O-TRAP = if id DNEED-OPERANDS exit then
   id DNEED-OPERANDS ;

: DNEED-BLOCK ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id b:n :}
   b DCUR<IN
   f b BLOCK-AT {: bk:IR-ID:ir-block-id :}
   bk OP-COUNT 0 ?do  bk i OP-AT DNEED-OP  loop ;

: DNEED-EDGE-OF ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id b:n :}
   f b BLOCK-AT TERM-OP {: t:IR-ID:ir-op-id :}
   t OP-SLOT O-BR <> if exit then
   t BR-TARGET {: tb:n :}
   FUN tb BLOCK-AT ARG-COUNT {: k:n :}
   t OPERANDS-OF k <> if exit then
   k 0 ?do
      tb i DDROP? 0= if t i OPERAND-AT DNEED+ then
   loop ;

: DNEED-EDGES ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   f BLOCK-COUNT 0 ?do f i DNEED-EDGE-OF loop ;

: DNEED-FIX ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   DNEED-CLEAR
   f DNEED-ENTRY
   f BLOCK-COUNT 0 ?do f i DNEED-BLOCK loop
   begin
      0 D-MOVED !
      f DNEED-EDGES
      D-MOVED @ 0=
   until ;

: DRESIDENCY ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   f DRES-FIX
   f DNEED-FIX ;

\ ---- choosing where the pointer stands ---------------------------------------
: DREQ+ ( n -- )
   {: at:n :}
   D-REQ-N @ DREQ-MAX >= if 1 D-REQ-OVER ! exit then
   at  D-REQ-N @ cells D-REQ + !
   D-REQ-N @ 1+ D-REQ-N ! ;

: DREQ-AT ( n -- n )
   cells D-REQ + @ ;

\ A call requires two places and they are the CALLEE's, not this routine's.
: DPLACE-CALL ( n n n n -- )
   drop
   {: a:n r:n kk:n :}
   kk a + A64IR:SLOT-WIDTH * DREQ+
   kk r + A64IR:SLOT-WIDTH * DREQ+ ;

\ A trap requires ONE place, the shared routine's.
: DPLACE-TRAP ( -- )
   A64IR:SLOT-WIDTH DREQ+ ;

\ A routine with two returns would publish twice.
: DPLACE-RETURN ( -- )
   D-RETS @ 1+ D-RETS !
   D-RETS @ 1 > if E-A64SEL-PLACE throw then
   OUTS SLOT-POSITIONS A64IR:SLOT-WIDTH * DREQ+ ;

: DPLACE-OP ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id OP-SLOT {: s:n :}
   s O-CALL = if id SELF-SHAPE DPLACE-CALL exit then
   s O-WORDCALL = if id SITE-SHAPE DPLACE-CALL exit then
   s O-TRAP = if DPLACE-TRAP exit then
   s O-RETURN = if DPLACE-RETURN then ;

: DPLACE-BLOCK ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id b:n :}
   f b BLOCK-AT {: bk:IR-ID:ir-block-id :}
   bk OP-COUNT 0 ?do  bk i OP-AT DPLACE-OP  loop ;

\ One instruction for every place the routine requires that is not this one; a
\ place required twice is counted twice.
: DPLACE-COST ( n -- n )
   {: c:n :}
   0
   D-REQ-N @ 0 ?do
      i DREQ-AT c <> if 1+ then
   loop ;

: DPLACE-OK? ( n -- bool )
   {: c:n :}
   c 0 >=  c A64EFF:SLOT-BACK <=  and ;

\ A routine that leaves through a callee has NO choice: the tail branch is the
\ whole of its site.
: DPLACE-BETTER? ( n n -- bool )
   {: c:n k:n :}
   k D-COST @ < if true exit then
   k D-COST @ =  c D-POS @ <  and ;

: DPLACE-TRY ( n -- )
   {: c:n :}
   c DPLACE-OK? 0= if exit then
   c DPLACE-COST {: k:n :}
   c k DPLACE-BETTER? 0= if exit then
   c D-POS !
   k D-COST ! ;

: DPLACE-CHOOSE ( -- )
   0 D-POS !
   0 DPLACE-COST D-COST !
   D-REQ-N @ 0 ?do  i DREQ-AT DPLACE-TRY  loop ;

: DPLACE ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   0 D-POS !
   0 D-REQ-N !
   0 D-REQ-OVER !
   0 D-RETS !
   DSTACK? 0= if exit then
   ARGS SLOT-POSITIONS A64IR:SLOT-WIDTH * DREQ+
   f BLOCK-COUNT 0 ?do  f i DPLACE-BLOCK  loop
   TAIL? if ARGS SLOT-POSITIONS A64IR:SLOT-WIDTH * D-POS ! exit then
   D-REQ-OVER @ 0<> if exit then
   DPLACE-CHOOSE ;

\ ---- opening the selected function -------------------------------------------
\ The two modules number their symbols separately, so the name is copied out of
\ the source module's interner and interned into the new one.
: FUN-NAME ( IR-ID:ir-fun-id -- IR-ID:ir-symbol-id )
   {: f:IR-ID:ir-fun-id :}
   V-SYMP VW V-SYMR VW  V-FUNR VW MKEY f IR-FUN:FSYMBOL@  NAMEBUF NAME-CAP
   IR-SYM:FCOPY {: u:n :}
   CTX BLD NAMEBUF u IR-BUILD:INTERN-SYMBOL ;

: FUN-SIG ( IR-ID:ir-fun-id -- IR-ID:ir-type-id )
   {: f:IR-ID:ir-fun-id :}
   V-TYPR VW  V-FUNR VW MKEY f IR-FUN:FSIGNATURE@  IR-TYPE:FARITY@
   {: in:n out:n :}
   CTX BLD A64IR:GPR-TYPE {: t:IR-ID:ir-type-id :}
   IR-TYPE:FN-BEGIN
   in 0 ?do t IR-TYPE:FN-PARAM loop
   out 0 ?do t IR-TYPE:FN-RESULT loop
   CTX BLD IR-BUILD:INTERN-CODE-REF ;

: OPEN-FUN ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   CTX BLD f FUN-NAME IR-BUILD:BEGIN-FUN
   CTX BLD f FUN-SIG IR-BUILD:SET-SIGNATURE
   CTX BLD  V-FUNR VW f IR-FUN:FLINKAGE@  IR-BUILD:SET-LINKAGE
   CTX BLD  V-FUNR VW f IR-FUN:FVISIBILITY@  IR-BUILD:SET-VISIBILITY
   CTX BLD  V-FUNR VW f IR-FUN:FCONVENTION@  IR-BUILD:SET-CONVENTION
   CTX BLD f FUN-SPAN IR-BUILD:SET-FUN-SPAN ;

\ At the class the source argument's TYPE says.
: OPEN-ARG1 ( IR-ID:ir-value-id -- )
   {: a:IR-ID:ir-value-id :}
   a TOKEN? if
      a  CTX BLD  CTX BLD A64IR:MEM-TYPE  IR-BUILD:ADD-BLOCK-ARG
      dup TOK!  VBIND
      exit
   then
   a REAL? if
      a  CTX BLD  CTX BLD A64IR:FPR-TYPE  IR-BUILD:ADD-BLOCK-ARG  VBIND
      exit
   then
   a  CTX BLD  CTX BLD A64IR:GPR-TYPE  IR-BUILD:ADD-BLOCK-ARG  VBIND ;

\ A block argument of the memory-order type is the ORDER arriving.
: OPEN-ARGS ( IR-ID:ir-block-id n -- )
   {: bk:IR-ID:ir-block-id ord:n :}
   bk ARG-COUNT {: n:n :}
   n 0 ?do
      ord i DDROP? 0= if bk i ARG-AT OPEN-ARG1 then
   loop ;

\ Under the data-stack convention the block takes no argument at all, because
\ nothing arrives in a register.
: OPEN-DARGS ( IR-ID:ir-block-id -- )
   {: bk:IR-ID:ir-block-id :}
   ARGS SLOT-POSITIONS {: a:n :}
   bk ARG-COUNT a <> if E-A64SEL-PLACE throw then
   bk 0 OP-AT {: at:IR-ID:ir-op-id :}
   at PROLOGUE
   at  a A64IR:SLOT-WIDTH *  EMIT-DTAKE
   0 ORDER-EDGE!
   a 0 ?do
      bk i ARG-AT {: v:IR-ID:ir-value-id :}
      v DNEED? if
         v LOAD-PLACE? if v FPLACE! then
         v
         at  ARGS i A64EFF:SEQ-SLOT@ A64IR:SLOT-WIDTH *
         v FPLACED? DLOAD-FORM  EMIT-DLOAD
         VBIND
      then
   loop ;

\ Only the entry block carries the routine's interface.
: OPEN-BLOCK ( IR-ID:ir-block-id n -- )
   {: bk:IR-ID:ir-block-id ord:n :}
   CTX BLD IR-BUILD:BEGIN-BLOCK
   CTX BLD bk BLOCK-SPAN IR-BUILD:SET-BLOCK-SPAN
   DSTACK? ord 0= and if bk OPEN-DARGS exit then
   bk ord OPEN-ARGS
   ord ORDER-ENTER ;


\ ---- emitting a selection as a select ----------------------------------------
: RSEL-SLOT ( n n -- n )
   {: b:n i:n :}
   i 0 < i SEL-WIDTH-MAX >= or if E-A64SEL-CAP throw then
   b BLOCK-ORD-CK SEL-WIDTH-MAX * i + ;

: RSEL@ ( n n -- IR-ID:ir-value-id )
   RSEL-SLOT RSEL @ ;

: RSEL! ( IR-ID:ir-value-id n n -- )
   RSEL-SLOT RSEL ! ;

: SELZ-OPCODE ( bool -- A64IR:opcode )
   if A64IR-OPCODE:SELZD else A64IR-OPCODE:SELZ then ;

: GPR-SEL-OPCODE ( bool -- A64IR:opcode )
   if A64IR-OPCODE:CMPSELD else A64IR-OPCODE:CMPSEL then ;

: FREG-SEL-OPCODE ( bool -- A64IR:opcode )
   if A64IR-OPCODE:FCMPSELD else A64IR-OPCODE:FCMPSEL then ;

: FZERO-SEL-OPCODE ( bool -- A64IR:opcode )
   if A64IR-OPCODE:FCMPSELZD else A64IR-OPCODE:FCMPSELZ then ;

\ Its kind says which instruction writes the flags and how many registers that
\ instruction reads.
: CMPSEL-OPCODE ( A64SEL:cmpkind bool -- A64IR:opcode )
   {: k:A64SEL:cmpkind d:bool :}
   k
   MATCH A64SEL:cmpkind
      none  OF E-A64SEL-OPCODE throw ENDOF
      gpr   OF d GPR-SEL-OPCODE ENDOF
      freg  OF d FREG-SEL-OPCODE ENDOF
      fzero OF d FZERO-SEL-OPCODE ENDOF
   ;MATCH ;

: SEL-RESULT+ ( bool -- )
   if FRESULT+ else RESULT+ then ;

: EMIT-SELZ ( IR-ID:ir-op-id IR-ID:ir-value-id IR-ID:ir-value-id IR-ID:ir-value-id bool -- IR-ID:ir-value-id )
   {: at:IR-ID:ir-op-id v:IR-ID:ir-value-id
      tv:IR-ID:ir-value-id fv:IR-ID:ir-value-id d:bool :}
   at d SELZ-OPCODE OPEN
   CTX BLD v IR-BUILD:ADD-OPERAND
   CTX BLD tv IR-BUILD:ADD-OPERAND
   CTX BLD fv IR-BUILD:ADD-OPERAND
   d SEL-RESULT+
   CLOSE-VALUE
   ACC ;

\ How many of the comparison's operands the select carries comes off its kind,
\ exactly as it does for the fused branch.
: EMIT-CMPSEL ( IR-ID:ir-op-id IR-ID:ir-op-id IR-ID:ir-value-id IR-ID:ir-value-id bool -- IR-ID:ir-value-id )
   {: at:IR-ID:ir-op-id cm:IR-ID:ir-op-id
      tv:IR-ID:ir-value-id fv:IR-ID:ir-value-id d:bool :}
   cm OP-KIND {: k:A64SEL:cmpkind :}
   at k d CMPSEL-OPCODE OPEN
   k KIND-OPERANDS 0 ?do
      CTX BLD  cm i OPERAND  IR-BUILD:ADD-OPERAND
   loop
   CTX BLD tv IR-BUILD:ADD-OPERAND
   CTX BLD fv IR-BUILD:ADD-OPERAND
   d SEL-RESULT+
   CTX BLD  CTX BLD A64IR:KEY-COND
   CTX BLD  cm COMPARE-COND  A64IR:COND-ATTR  IR-BUILD:ADD-ATTR
   CLOSE-VALUE
   ACC ;

: REGION-PICK ( IR-ID:ir-op-id n n -- IR-ID:ir-value-id )
   {: t:IR-ID:ir-op-id fz:n i:n :}
   R-S1 @ i RSEL@ {: tv:IR-ID:ir-value-id :}
   R-S0 @ i RSEL@ {: fv:IR-ID:ir-value-id :}
   tv fv SAME-VALUE? if tv exit then
   0 R-JB @ i ARG-AT TOKEN? if E-A64SEL-SHAPE throw then
   0 R-JB @ i ARG-AT REAL? {: d:bool :}
   fz 0 < if  t  t 0 OPERAND  tv fv  d  EMIT-SELZ exit then
   t  BLK fz OP-AT  tv fv  d  EMIT-CMPSEL ;

\ ---- one block of the region ------------------------------------------------
\ A member's arguments are bound to what its ONE predecessor handed over.
: REGION-BIND-ARGS ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id b:n :}
   f b BLOCK-AT {: bk:IR-ID:ir-block-id :}
   bk ARG-COUNT {: k:n :}
   f b R-FROM@ BLOCK-AT TERM-OP {: t:IR-ID:ir-op-id :}
   t OP-SLOT O-BR <> if
      k 0<> if E-A64SEL-SHAPE throw then
      exit
   then
   t OPERANDS-OF k <> if E-A64SEL-SHAPE throw then
   k 0 ?do
      b i DDROP? 0= if
         bk i ARG-AT  t i OPERAND  VBIND
      then
   loop ;

\ The terminator is what the conversion replaces, and the comparison the select
\ will make is emitted by the select.
: REGION-OPS ( IR-ID:ir-block-id -- )
   {: bk:IR-ID:ir-block-id :}
   bk FUSE-INDEX {: fz:n :}
   bk OP-COUNT 1- {: k:n :}
   k 0 ?do
      i fz <> if bk i OP-AT RULE then
   loop ;

: REGION-BLOCK-OPS ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id b:n :}
   f b BLOCK-AT {: bk:IR-ID:ir-block-id :}
   bk 0 S-BLK !
   bk REGION-OPS ;

: REGION-MEMBER? ( n n -- bool )
   {: h:n b:n :}
   b R-ABSORB? 0= if false exit then
   b R-OWNER@ h = ;

: REGION-MEMBERS-OPS ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id h:n :}
   f BLOCK-COUNT h 1+ ?do
      h i REGION-MEMBER? if
         f i REGION-BIND-ARGS
         f i REGION-BLOCK-OPS
      then
   loop ;

\ ---- what each block of the region hands the exit ---------------------------
: RJOIN-DROP? ( n -- bool )
   R-EXIT-BK @ swap DDROP? ;

: REGION-VALUES-BR ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id b:n :}
   f b BLOCK-AT TERM-OP {: t:IR-ID:ir-op-id :}
   t 0 SUCC-IDX {: sc:n :}
   sc R-EXIT-BK @ = if
      t OPERANDS-OF R-WIDTH @ <> if E-A64SEL-SHAPE throw then
      R-WIDTH @ 0 ?do
         i RJOIN-DROP? 0= if  t i OPERAND  b i RSEL!  then
      loop
      exit
   then
   R-WIDTH @ 0 ?do
      i RJOIN-DROP? 0= if  sc i RSEL@  b i RSEL!  then
   loop ;

: REGION-VALUES-BRZ ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id b:n :}
   f b BLOCK-AT {: bk:IR-ID:ir-block-id :}
   bk TERM-OP {: t:IR-ID:ir-op-id :}
   bk FUSE-INDEX {: fz:n :}
   t 0 SUCC-IDX R-S0 !
   t 1 SUCC-IDX R-S1 !
   R-WIDTH @ 0 ?do
      i RJOIN-DROP? 0= if  t fz i REGION-PICK  b i RSEL!  then
   loop ;

: REGION-BLOCK-VALUES ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id b:n :}
   f b BLOCK-AT {: bk:IR-ID:ir-block-id :}
   bk 0 S-BLK !
   bk BR-TERM? if f b REGION-VALUES-BR exit then
   f b REGION-VALUES-BRZ ;

: REGION-MEMBERS-VALUES ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id h:n :}
   f BLOCK-COUNT {: k:n :}
   k h 1+ ?do
      k h + i - {: b:n :}
      h b REGION-MEMBER? if f b REGION-BLOCK-VALUES then
   loop ;

\ ---- the one branch the region keeps ----------------------------------------
: REGION-BR ( IR-ID:ir-op-id n -- )
   {: t:IR-ID:ir-op-id h:n :}
   0
   R-WIDTH @ 0 ?do
      i RJOIN-DROP? 0= if
         0 R-JB @ i ARG-AT TOKEN? if
            h i RSEL@
         else
            t  h i RSEL@  0 R-JB @ i ARG-AT REAL?  EMIT-COPY
         then
         over EDGE-V !
         1+
      then
   loop
   {: n:n :}
   t A64IR-OPCODE:BR OPEN
   n 0 ?do
      CTX BLD  i EDGE-V @  IR-BUILD:ADD-OPERAND
   loop
   R-EXIT-BK @ SUCCESSOR-ORD+
   CTX BLD IR-BUILD:END-OP drop ;

: REGION-WIDTH! ( -- )
   0 R-JB @ ARG-COUNT {: w:n :}
   w SEL-WIDTH-MAX > if E-A64SEL-CAP throw then
   w R-WIDTH ! ;

: REGION-EMIT ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id h:n :}
   h R-EXIT@ R-EXIT-BK !
   f R-EXIT-BK @ BLOCK-AT 0 R-JB !
   REGION-WIDTH!
   RLIT-OPEN
   f h REGION-BLOCK-OPS
   f h REGION-MEMBERS-OPS
   RLIT-CLOSE
   f h REGION-MEMBERS-VALUES
   f h REGION-BLOCK-VALUES
   f h BLOCK-AT TERM-OP  h  REGION-BR ;

\ The value map is NOT cleared between blocks: a value defined in one block and
\ read in another is ordinary.
: WALK-BLOCK ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id ord:n :}
   ord R-ABSORB? if exit then
   ord DCUR<IN
   f ord BLOCK-AT {: bk:IR-ID:ir-block-id :}
   bk ord OPEN-BLOCK
   bk 0 S-BLK !
   ord R-HEAD? if
      f ord REGION-EMIT
      CTX BLD IR-BUILD:END-BLOCK drop
      exit
   then
   bk FUSE-SCAN
   bk OP-COUNT {: n:n :}
   n 0 ?do
      i FUSE-AT @ <> if bk i OP-AT RULE then
   loop
   CTX BLD IR-BUILD:END-BLOCK drop ;

\ Block by block in the order the module records them, which is the order every
\ later pass reads too.
: WALK-FUN ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id ord:n :}
   f BLOCK-COUNT {: n:n :}
   n 1 < if E-A64SEL-SHAPE throw then
   n NFROZEN:BMAX > if E-A64SEL-CAP throw then
   f ord FUN-PLACES!
   f ord TAIL-SITE!
   f 0 S-FUN !
   f OPEN-FUN
   VCLEAR
   ORDER-CLEAR
   RLIT-CLOSE
   f PLAN-REGIONS
   f DRESIDENCY
   f DPLACE
   n 0 ?do
      f i WALK-BLOCK
   loop
   R-NEWBASE @ R-NEXT @ + R-NEWBASE !
   CTX BLD IR-BUILD:END-FUN drop ;

\ ---- what one selection run is told ------------------------------------------
\ The new module gets the same source the old one has, proved by digest.
: SOURCE! ( IR-CTX:ctx IR-BUILD:builder ptr u8 n -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder p u:n :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   V-SRC VW IR-SOURCE:FSOURCES 1 <> if E-A64SEL-SHAPE throw then
   V-SRC VW  MKEY 0 IR-ID:PACK-SOURCE  IR-SOURCE:FDIGEST@
   p u CDIGEST:COMPUTE
   CDIGEST-DIGEST:EQ 0= if E-A64SEL-SOURCE throw then
   c b p u IR-BUILD:ADD-SOURCE 0 S-SID ! ;

: BND-TAKE ( -- )
   BND-MODE @ {: have:n :}
   BOUND-NO BND-MODE !
   have BOUND-YES <> if E-A64SEL-BIND throw then ;

: BND-MODULE-CK ( IR-BUILD:module -- )
   IR-BUILD:FMODULE  0 BND-MOD @  IR-ID:MODULE-SAME?
   0= if E-A64SEL-SOURCE throw then ;

: BIND1 ( IR-CTX:ctx IR-BUILD:builder HIR:opcode -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder o:HIR:opcode :}
   c b o HIR:OPCODE  o SLOT-OF BND-OP ! ;

: HIR-CK ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b  c b IR-BUILD:DIALECT@  HIR:NAME IR-BUILD:SYMBOL-IS?
   0= if E-A64SEL-SOURCE throw then
   c b IR-BUILD:SCHEMA-MAJOR@ HIR:MAJOR <> if E-A64SEL-SOURCE throw then
   c b IR-BUILD:SCHEMA-MINOR@ HIR:MINOR <> if E-A64SEL-SOURCE throw then ;

public

\ ---- binding the source dialect ----------------------------------------------
\ The only moment a module can be asked its opcode identities, because its
\ symbols are its own ordinals.
: BIND-SOURCE ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   BND-MODE @ BOUND-YES = if E-A64SEL-BIND throw then
   c b HIR-CK
   b IR-BUILD:MODULE@ 0 BND-MOD !
   c b HIR-OPCODE:CONST  BIND1
   c b HIR-OPCODE:ADD    BIND1
   c b HIR-OPCODE:SUB    BIND1
   c b HIR-OPCODE:MUL    BIND1
   c b HIR-OPCODE:DIV    BIND1
   c b HIR-OPCODE:LT     BIND1
   c b HIR-OPCODE:LE     BIND1
   c b HIR-OPCODE:GT     BIND1
   c b HIR-OPCODE:GE     BIND1
   c b HIR-OPCODE:EQUAL  BIND1
   c b HIR-OPCODE:NE     BIND1
   c b HIR-OPCODE:AND    BIND1
   c b HIR-OPCODE:OR     BIND1
   c b HIR-OPCODE:XOR    BIND1
   c b HIR-OPCODE:LSHIFT BIND1
   c b HIR-OPCODE:RSHIFT BIND1
   c b HIR-OPCODE:INVERT BIND1
   c b HIR-OPCODE:BR     BIND1
   c b HIR-OPCODE:BRZ    BIND1
   c b HIR-OPCODE:MEM    BIND1
   c b HIR-OPCODE:LOAD   BIND1
   c b HIR-OPCODE:STORE  BIND1
   c b HIR-OPCODE:BLOAD  BIND1
   c b HIR-OPCODE:BSTORE BIND1
   c b HIR-OPCODE:CALL   BIND1
   c b HIR-OPCODE:WORDCALL BIND1
   c b HIR-OPCODE:QUOT   BIND1
   c b HIR-OPCODE:RETURN BIND1
   c b HIR-OPCODE:FCONST   BIND1
   c b HIR-OPCODE:FADD     BIND1
   c b HIR-OPCODE:FSUB     BIND1
   c b HIR-OPCODE:FMUL     BIND1
   c b HIR-OPCODE:FDIV     BIND1
   c b HIR-OPCODE:FNEG     BIND1
   c b HIR-OPCODE:FABS     BIND1
   c b HIR-OPCODE:FSQRT    BIND1
   c b HIR-OPCODE:INTREAL  BIND1
   c b HIR-OPCODE:REALINT  BIND1
   c b HIR-OPCODE:BITSREAL BIND1
   c b HIR-OPCODE:REALBITS BIND1
   c b HIR-OPCODE:FLT      BIND1
   c b HIR-OPCODE:FGT      BIND1
   c b HIR-OPCODE:FEQ      BIND1
   c b HIR-OPCODE:FLTZ     BIND1
   c b HIR-OPCODE:FEQZ     BIND1
   c b HIR-OPCODE:TRAP     BIND1
   c b HIR:KEY-VALUE 0 BND-VAL !
   c b HIR:KEY-ADDR  0 BND-ADDR !
   c b HIR:KEY-FUN   0 BND-FUN !
   c b HIR:KEY-ENTRY 0 BND-ENTRY !
   c b HIR:KEY-IN    0 BND-IN !
   c b HIR:KEY-OUT   0 BND-OUT !
   c b HIR:MEM-TYPE 0 BND-MEM !
   c b HIR:REAL-TYPE 0 BND-REAL !
   BOUND-YES BND-MODE ! ;

\ Each pass answers for itself, because a caller cleaning up after a refusal
\ cannot know how far the run got.
: BOUND? ( -- bool )
   BND-MODE @ BOUND-YES = ;

: RELEASE ( -- )
   BND-TAKE ;

\ ---- the pass ----------------------------------------------------------------
\ A module with no such loop or no such pair is handed back untouched, because
\ rebuilding renumbers values and would move registers for nothing.
: SELECT ( IR-CTX:ctx IR-BUILD:module IR-BUILD:builder ptr u8 n A64EFF:routine -- IR-BUILD:module )
   A64EFF:VALIDATE A64EFF-ROUTINE:UNMAKE
   {: cv:A64EFF:conv gi:A64EFF:placeseq gr:A64EFF:placeseq gc:A64EFF:gprs
      fi:A64EFF:fprs fr:A64EFF:fprs fc:A64EFF:fprs
      z:A64EFF:nzcv l:A64EFF:link ct:A64EFF:control
      t:A64EFF:traits size:n delta:n :}
   {: c:IR-CTX:ctx m:IR-BUILD:module b:IR-BUILD:builder p u:n :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   BND-TAKE
   m BND-MODULE-CK
   gi 0 S-ARGS !
   gr 0 S-OUTS !
   gi A64EFF:SEQ-LEN S-DECL-IN !
   gr A64EFF:SEQ-LEN S-DECL-OUT !
   cv gi gr gc fi fr fc z l ct t size delta A64EFF-ROUTINE:MAKE
   A64EFF:GPR-WRITABLE 0 S-POOL !
   cv gi gr gc fi fr fc z l ct t size delta A64EFF-ROUTINE:MAKE
   A64EFF:FPR-WRITABLE 0 S-FPOOL !
   t 0 S-TRT !
   size S-FRAME !
   ct A64EFF-CONTROL:TAIL-CALL A64EFF-CONTROL:EQ if 1 else 0 then S-TAIL !
   cv A64EFF-CONV:DSTACK A64EFF-CONV:EQ if 1 else 0 then S-DSTACK !
   t l A64FRAME:LINK-KEPT? if 1 else 0 then S-LSAVE !
   0 N-CALLS !
   0 N-TAILS !
   -1 TAIL-SITE !
   0 N-TRAPS !
   0 R-NEWBASE !
   CONTRACT-CK
   c b A64IR:REGISTER
   c 0 S-CTX !
   b 0 S-BLD !
   m VIEWS!
   c b p u SOURCE!
   FUN-COUNT {: n:n :}
   n 0 ?do
      MKEY i IR-ID:PACK-FUN i WALK-FUN
   loop
   CALLED-CK
   TAILED-CK
   c b IR-BUILD:FREEZE ;

private
get-current prot-wid-add

public
get-current prot-wid-add

;using
;package
