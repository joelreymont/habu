\ regalloc-verify.f - decide whether a finished register assignment is true of the
\ module it claims to be about.
\
\ It re-derives every register fact from the operand and result windows of the
\ module it is handed and knows nothing about how the allocator reached them.
\
\ A register number names a register of ONE file - d0 and x0 are two registers
\ and both are number zero - so every question about a register is asked of the
\ FILE the value's class belongs to.
\
\ Value ids are the MODULE's and slots are a FUNCTION's, so the value tables are
\ cleared once per module and the slot table once per function.

require lib/prelude.f
require lib/errors.f
require src/compiler/a64-effect.f
require src/compiler/ir/id.f
require src/compiler/ir/context.f
require src/compiler/ir/schema.f
require src/compiler/ir/op.f
require src/compiler/ir/fun.f
require src/compiler/ir/build.f
require src/compiler/native/a64ir.f
require src/compiler/native/frame.f
require src/compiler/native/frozen.f
require src/compiler/native/regalloc.f

package A64RAV
using NFROZEN

\ ---- the one door this check opens for a reader ------------------------------
public

defer DKEEP-HOOK ( -- )

private

-1 constant NOPOS

0 constant ST-NONE
1 constant ST-ACCEPTED

0 constant BOUND-NO
1 constant BOUND-YES

0 constant C-GPR
1 constant C-TOKEN
2 constant C-FPR

\ Two files, separately numbered, so a register question is always asked of one.
2 constant FILES-N
0 constant F-GPR
1 constant F-FPR

-1 constant NOFILE

: FILE-OF ( n -- n )
   {: cls:n :}
   cls C-GPR = if F-GPR exit then
   cls C-FPR = if F-FPR exit then
   cls C-TOKEN = if NOFILE exit then
   E-A64RAV-CLASS throw ;

-1 constant NOSLOT

VMAX constant SLOTS-MAX

here CELL 1- and CELL swap - CELL 1- and allot
variable ST
ST-NONE ST !
variable BND-MODE
BOUND-NO BND-MODE !
variable A-GEN
0 A-GEN !
variable N-VALS
0 N-VALS !
variable NB-N                        \ blocks in the function being checked
0 NB-N !
variable VB-BASE                     \ where that function's blocks start in the module
0 VB-BASE !
variable V-DSTACK                    \ whether the contract declares the data-stack convention
0 V-DSTACK !

1 TYPED-BUFFER S-FUN IR-ID:ir-fun-id

: FUN ( -- IR-ID:ir-fun-id )         0 S-FUN @ ;

1 TYPED-BUFFER BND-MOD IR-ID:ir-module-id
1 TYPED-BUFFER BND-GPR IR-ID:ir-type-id
1 TYPED-BUFFER BND-FPR IR-ID:ir-type-id
1 TYPED-BUFFER BND-MEM IR-ID:ir-type-id
1 TYPED-BUFFER BND-SLOT IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-FRAME IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-DSLOT IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-DBYTES IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-DBACK IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-ENTRY IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-TRAP IR-ID:ir-symbol-id
1 TYPED-BUFFER V-POOL A64EFF:gprs
1 TYPED-BUFFER V-FPOOL A64EFF:fprs

create D-AT VMAX cells allot         \ where the module says each value is written
create L-AT VMAX cells allot         \ where the module says each value is last read
create S-AT VMAX cells allot         \ whether the block defines this value at all

\ A value belongs to the function whose window its definition position falls in,
\ which is the only sense in which a value has a function at all.
create F-VB NFROZEN:FMAX 1 + cells allot
create C-AT VMAX cells allot         \ which class the module gives each value
create W-AT SLOTS-MAX cells allot    \ where each slot was written, or -1
create U-AT VMAX cells allot         \ how many operands of the function name each value
create UB VMAX cells allot           \ which blocks name each value, one bit per block
create DB VMAX cells allot           \ which block defines each value, or -1
create RCH BMAX cells allot          \ blocks one reachability question has reached

: DEF-AT ( n -- n )                  cells D-AT + @ ;
: LAST-AT ( n -- n )                 cells L-AT + @ ;
: SEEN-AT ( n -- n )                 cells S-AT + @ ;
: CLS-AT ( n -- n )                  cells C-AT + @ ;
: FILE-AT ( n -- n )                 CLS-AT FILE-OF ;
: USES-AT ( n -- n )                 cells U-AT + @ ;

: DEF! ( n n -- )                    {: v:n k:n :} v k cells D-AT + ! ;
: LAST! ( n n -- )                   {: v:n k:n :} v k cells L-AT + ! ;
: SEEN! ( n n -- )                   {: v:n k:n :} v k cells S-AT + ! ;
: CLS! ( n n -- )                    {: v:n k:n :} v k cells C-AT + ! ;
: USES! ( n n -- )                   {: v:n k:n :} v k cells U-AT + ! ;

\ ---- the tables, and which of them a FUNCTION owns ---------------------------
\ The value tables are the MODULE's; the slot table is a FUNCTION's.
: VALUES-CLEAR ( -- )
   VMAX 0 ?do
      0 i SEEN!
      NOPOS i DEF!
      NOPOS i LAST!
      C-GPR i CLS!
   loop ;

: SLOTS-CLEAR ( -- )
   SLOTS-MAX 0 ?do -1 i cells W-AT + ! loop ;

: TABLES-CLEAR ( -- )
   VALUES-CLEAR
   SLOTS-CLEAR ;

\ ---- reading the frozen module -----------------------------------------------
: SLOT ( IR-ID:ir-value-id -- n )
   IR-ID:VALUE-LOCAL
   dup 0 < over VMAX >= or if E-A64RAV-COVER throw then ;

\ ---- what an operation says about the memory it reaches -----------------------
: ATTR-INT ( IR-ID:ir-op-id IR-ID:ir-symbol-id -- n )
   {: id:IR-ID:ir-op-id want:IR-ID:ir-symbol-id :}
   NOSLOT
   id ATTRS-OF 0 ?do
      id i ATTR-KEY-AT want SAME-SYM? if
         drop
         id i ATTR-INT-AT
         leave
      then
   loop ;

: SLOT-OF ( IR-ID:ir-op-id -- n )    0 BND-SLOT @ ATTR-INT ;
: FRAME-OF ( IR-ID:ir-op-id -- n )   0 BND-FRAME @ ATTR-INT ;
: DSLOT-OF ( IR-ID:ir-op-id -- n )   0 BND-DSLOT @ ATTR-INT ;
: DBYTES-OF ( IR-ID:ir-op-id -- n )  0 BND-DBYTES @ ATTR-INT ;
: DBACK-OF ( IR-ID:ir-op-id -- n )   0 BND-DBACK @ ATTR-INT ;

: DCALL? ( IR-ID:ir-op-id -- bool )
   DBACK-OF NOSLOT <> ;

: TRAP-AT? ( IR-ID:ir-op-id -- bool )
   0 BND-TRAP @ ATTR-INT NOSLOT <> ;

: FRAME-TOUCH? ( IR-ID:ir-op-id -- bool )
   {: id:IR-ID:ir-op-id :}
   id FRAME-OF NOSLOT <>  id SLOT-OF NOSLOT <>  or ;

: DSTACK-TOUCH? ( IR-ID:ir-op-id -- bool )
   {: id:IR-ID:ir-op-id :}
   id DBYTES-OF NOSLOT <>  id DSLOT-OF NOSLOT <>  or
   id DBACK-OF NOSLOT <> or ;

: STORES? ( IR-ID:ir-op-id -- bool )
   {: id:IR-ID:ir-op-id :}
   V-SCHR VW id OPCODE-AT IR-SCHEMA:FEFFECT@
   IR--SCHEMA-EFFECT:WRITE IR--SCHEMA-EFFECT:EQ ;

: FUN-AT ( n -- IR-ID:ir-fun-id )
   {: k:n :}
   k 0 < k FUN-COUNT >= or if E-A64RAV-SHAPE throw then
   MKEY k IR-ID:PACK-FUN ;

: FUNS-CK ( -- n )
   FUN-COUNT {: n:n :}
   n 1 < if E-A64RAV-SHAPE throw then
   n NFROZEN:FMAX > if E-A64RAV-COVER throw then
   n ;

\ The block the routine's RESULTS leave through, and NO-RET when there is none;
\ a trap block is not that block.
-1 constant NO-RET

: RET-ORD ( IR-ID:ir-fun-id -- n )
   {: f:IR-ID:ir-fun-id :}
   NO-RET
   f BLOCK-COUNT 0 ?do
      f i BLOCK-AT TERM-AT {: t:IR-ID:ir-op-id :}
      t SUCCS-OF 0=  t TRAP-AT? 0=  and if
         dup NO-RET <> if E-A64RAV-SHAPE throw then
         drop i
      then
   loop ;

\ ---- where one function's blocks stand in the module --------------------------
\ A block names itself twice over: by its ordinal in the MODULE, which is what a
\ successor carries, and by its ordinal in the FUNCTION.
: VB-BASE! ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   f 0 BLOCK-AT IR-ID:BLOCK-LOCAL VB-BASE !
   f BLOCK-COUNT 0 ?do
      f i BLOCK-AT IR-ID:BLOCK-LOCAL  VB-BASE @ -  i <>
      if E-A64RAV-SHAPE throw then
   loop ;

\ ---- what the module says about each value -----------------------------------
: NOTE-DEF ( IR-ID:ir-value-id n -- )
   {: id:IR-ID:ir-value-id pos:n :}
   id SLOT {: k:n :}
   k SEEN-AT 0<> if E-A64RAV-COVER throw then
   1 k SEEN!
   pos k DEF!
   pos k LAST! ;

: NOTE-USE ( IR-ID:ir-value-id n -- )
   {: id:IR-ID:ir-value-id pos:n :}
   id SLOT {: k:n :}
   k SEEN-AT 0= if E-A64RAV-COVER throw then
   pos k LAST! ;

\ ---- the checks --------------------------------------------------------------
: VALS-N! ( -- )
   V-VALR VW IR-OP:FVALUES {: n:n :}
   n VMAX > if E-A64RAV-COVER throw then
   n N-VALS ! ;

\ Every value of the module was defined exactly once, across all its functions.
: COVER-CK ( -- )
   N-VALS @ A64RA:VALUES <> if E-A64RAV-COVER throw then
   N-VALS @ 0 ?do i SEEN-AT 0= if E-A64RAV-COVER throw then loop ;

: INTERVAL-CK ( -- )
   N-VALS @ 0 ?do
      i DEF-AT i A64RA:DEF@ <> if E-A64RAV-INTERVAL throw then
      i LAST-AT i A64RA:LAST@ <> if E-A64RAV-INTERVAL throw then
   loop ;

: CLASS-CK ( -- )
   N-VALS @ 0 ?do
      MKEY i IR-ID:PACK-VALUE VALUE-TYPE-AT
      {: t:IR-ID:ir-type-id :}
      t 0 BND-GPR @ SAME-TYPE? if C-GPR i CLS! then
      t 0 BND-FPR @ SAME-TYPE? if C-FPR i CLS! then
      t 0 BND-MEM @ SAME-TYPE? if C-TOKEN i CLS! then
      t 0 BND-GPR @ SAME-TYPE?  t 0 BND-FPR @ SAME-TYPE? or
      t 0 BND-MEM @ SAME-TYPE? or
      0= if E-A64RAV-CLASS throw then
   loop ;

: IN-FILE? ( n n -- bool )
   {: v:n fl:n :}
   v FILE-AT fl = ;

: REGGED? ( n -- bool )
   FILE-AT NOFILE <> ;

\ ---- the memory order --------------------------------------------------------
: UB-AT ( n -- n )                   cells UB + @ ;
: UB! ( n n -- )                     {: v:n k:n :} v k cells UB + ! ;
: DB-AT ( n -- n )                   cells DB + @ ;
: DB! ( n n -- )                     {: v:n k:n :} v k cells DB + ! ;

: UB-HAS? ( n n -- bool )
   {: k:n b:n :}
   k UB-AT  1 b lshift  and 0<> ;

: UB-ADD ( n n -- )
   {: k:n b:n :}
   k UB-AT  1 b lshift or  k UB! ;

: UB-BLOCKS ( n -- n )
   {: k:n :}
   0
   NB-N @ 0 ?do k i UB-HAS? if 1+ then loop ;

\ ---- reachability between blocks, with one block held out --------------------
: RCH? ( n -- bool )                 cells RCH + @ 0<> ;
: RCH-MARK ( n -- )                  1 swap cells RCH + ! ;

: RCH-CLEAR ( -- )
   NB-N @ 0 ?do 0 i cells RCH + ! loop ;

: SUCC-ORD ( IR-ID:ir-op-id n -- n )
   SUCC-AT IR-ID:BLOCK-LOCAL  VB-BASE @ -
   dup 0 < over NB-N @ >= or if E-A64RAV-SHAPE throw then ;

: RCH-EXPAND ( n n -- bool )
   {: b:n d:n :}
   FUN b BLOCK-AT TERM-AT {: t:IR-ID:ir-op-id :}
   false
   t SUCCS-OF 0 ?do
      t i SUCC-ORD
      dup d = over RCH? or if drop else RCH-MARK drop true then
   loop ;

: REACH-FILL ( n n -- )
   {: from:n d:n :}
   RCH-CLEAR
   from d RCH-EXPAND drop
   begin
      false
      NB-N @ 0 ?do
         i RCH? if i d RCH-EXPAND or then
      loop
      0=
   until ;

: ORDER-FROM-CK ( n n -- )
   {: k:n u:n :}
   u  k DB-AT  REACH-FILL
   NB-N @ 0 ?do
      i u <>  k i UB-HAS?  and  i RCH?  and if E-A64RAV-ORDER throw then
   loop ;

: ORDER-VALUE-CK ( n -- )
   {: k:n :}
   k USES-AT 1 < if E-A64RAV-ORDER throw then
   k USES-AT  k UB-BLOCKS  <> if E-A64RAV-ORDER throw then
   k DB-AT 0 < if E-A64RAV-ORDER throw then
   NB-N @ 0 ?do
      k i UB-HAS? if k i ORDER-FROM-CK then
   loop ;

\ ---- who reads and who writes each value -------------------------------------
: COUNT-OP ( IR-ID:ir-op-id n -- )
   {: id:IR-ID:ir-op-id b:n :}
   id OPERANDS-OF 0 ?do
      id i OPERAND-AT SLOT {: k:n :}
      k USES-AT 1+ k USES!
      k b UB-ADD
   loop
   id RESULTS-OF 0 ?do
      id i RESULT-AT SLOT {: k:n :}
      b k DB!
   loop ;

: COUNT-BLOCK ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id b:n :}
   f b BLOCK-AT {: bk:IR-ID:ir-block-id :}
   bk ARG-COUNT 0 ?do
      bk i ARG-AT SLOT {: k:n :}
      b k DB!
   loop
   bk OP-COUNT 0 ?do
      bk i OP-AT b COUNT-OP
   loop ;

: IN-WINDOW? ( n n n -- bool )
   {: v:n lo:n hi:n :}
   v DEF-AT {: d:n :}
   d NOPOS = if false exit then
   d lo >= d hi < and ;

\ The token values it orders are this FUNCTION's, and the window says so.
: ORDER-CK ( IR-ID:ir-fun-id n n -- )
   {: f:IR-ID:ir-fun-id lo:n hi:n :}
   f 0 S-FUN !
   f BLOCK-COUNT NB-N !
   NB-N @ BMAX > if E-A64RAV-SHAPE throw then
   f VB-BASE!
   N-VALS @ 0 ?do 0 i USES! 0 i UB! -1 i DB! loop
   NB-N @ 0 ?do f i COUNT-BLOCK loop
   N-VALS @ 0 ?do
      i CLS-AT C-TOKEN =  i lo hi IN-WINDOW?  and if i ORDER-VALUE-CK then
   loop ;

: FILE-POOL ( n n n -- n )
   {: fl:n pool:n fpool:n :}
   fl F-GPR = if pool exit then
   fl F-FPR = if fpool exit then
   E-A64RAV-CLASS throw ;

: REGISTER-CK ( -- )
   A64RA:POOL A64EFF:GPRS-N {: pool:n :}
   A64RA:FPOOL A64EFF:FPRS-N {: fpool:n :}
   N-VALS @ 0 ?do
      i A64RA:CLAIM@ {: r:n :}
      i REGGED? 0= if
         r 0 >= r A64EFF:FILE-SIZE < and if E-A64RAV-CLASS throw then
      else
         r 0 < r A64EFF:FILE-SIZE >= or if E-A64RAV-REGISTER throw then
         i FILE-AT pool fpool FILE-POOL
         1 r lshift and 0= if E-A64RAV-REGISTER throw then
      then
   loop ;

\ ---- what a caller may keep in a register across a call ----------------------
\ A value whose live range spans a call site has to be in a register the callee
\ does not destroy, which is the whole of the caller-save rule re-derived here.
: VCALL-ENTRY ( IR-ID:ir-op-id -- n )
   0 BND-ENTRY @ ATTR-INT ;

\ Read by its ATTRIBUTES and never by its opcode: a tail branch carries an
\ address and no take-back count.
: TAILBR? ( IR-ID:ir-op-id -- bool )
   {: id:IR-ID:ir-op-id :}
   id VCALL-ENTRY NOSLOT = if false exit then
   id DBACK-OF NOSLOT = ;

: VCALL-BITS ( IR-ID:ir-op-id n -- n )
   {: id:IR-ID:ir-op-id fl:n :}
   id VCALL-ENTRY {: e:n :}
   fl F-FPR = if
      e NOSLOT = if 0 V-FPOOL @ A64EFF:FPRS-N exit then
      e 0 V-FPOOL @ NCLOB:FPR-CLOB A64EFF:FPRS-N exit
   then
   fl F-GPR = 0= if E-A64RAV-CLASS throw then
   e NOSLOT = if 0 V-POOL @ A64EFF:GPRS-N exit then
   e 0 V-POOL @ NCLOB:GPR-CLOB A64EFF:GPRS-N ;

: CLOB-AT ( IR-ID:ir-op-id n -- )
   {: id:IR-ID:ir-op-id p:n :}
   id DCALL? 0= if exit then
   N-VALS @ 0 ?do
      i REGGED?  i DEF-AT p <  and  i LAST-AT p >  and if
         id i FILE-AT VCALL-BITS  1 i A64RA:CLAIM@ lshift  and
         0<> if E-A64RAV-CLOBBER throw then
      then
   loop ;

: CLASH? ( n n -- bool )
   {: a:n b:n :}
   a DEF-AT b DEF-AT = if true exit then
   a DEF-AT b DEF-AT < if
      a LAST-AT b DEF-AT > exit
   then
   b LAST-AT a DEF-AT > ;

\ Two values that clash may not share a register, measured over the live ranges
\ this pass computed rather than the ones the allocator claimed.
: OVERLAP-CK ( -- )
   N-VALS @ {: n:n :}
   n 0 ?do
      n i 1+ ?do
         j REGGED?  j FILE-AT i FILE-AT = and  j i CLASH? and if
            j A64RA:CLAIM@ i A64RA:CLAIM@ = if E-A64RAV-OVERLAP throw then
         then
      loop
   loop ;

\ A form that names one register field twice - the move-wide overwrite - has its
\ result tied to its operand, which the schema declares.
: OP-TIE-CK ( IR-ID:ir-op-id n -- )
   {: id:IR-ID:ir-op-id i:n :}
   V-SCHP VW V-SCHR VW  id OPCODE-AT  i IR-SCHEMA:FTIE-RESULT@ {: rs:n :}
   V-SCHP VW V-SCHR VW  id OPCODE-AT  i IR-SCHEMA:FTIE-OPERAND@ {: op:n :}
   id op OPERAND-AT SLOT A64RA:CLAIM@
   id rs RESULT-AT SLOT A64RA:CLAIM@
   <> if E-A64RAV-TIE throw then ;

: TIE-CK ( IR-ID:ir-block-id -- )
   {: bk:IR-ID:ir-block-id :}
   bk OP-COUNT {: n:n :}
   n 0 ?do
      bk i OP-AT {: id:IR-ID:ir-op-id :}
      V-SCHR VW id OPCODE-AT IR-SCHEMA:FTIES 0 ?do
         id i OP-TIE-CK
      loop
   loop ;

\ ---- the routine's declared registers ------------------------------------------
: REG-POSITIONS ( A64EFF:placeseq -- n )
   {: s:A64EFF:placeseq :}
   s A64EFF:SEQ-LEN {: len:n :}
   s A64EFF:SEQ-SLOTS {: sl:n :}
   sl 0= if len exit then
   sl len <> if E-A64RAV-PLACE throw then
   0 ;

: ARG-CK ( IR-ID:ir-block-id A64EFF:placeseq -- )
   {: bk:IR-ID:ir-block-id args:A64EFF:placeseq :}
   V-DSTACK @ 0<> bk ARG-COUNT 0<> and
   if E-A64RAV-PLACE throw then
   args REG-POSITIONS {: n:n :}
   bk ARG-COUNT n < if E-A64RAV-FIXED throw then
   n 0 ?do
      bk i ARG-AT SLOT A64RA:CLAIM@
      args i A64EFF:SEQ-REG@ <> if E-A64RAV-FIXED throw then
   loop ;

: OUT-TAIL-CK ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   V-DSTACK @ 0= if E-A64RAV-PLACE throw then
   id OPERANDS-OF 1 <> if E-A64RAV-PLACE throw then ;

: OUT-CK ( IR-ID:ir-block-id A64EFF:placeseq -- )
   {: bk:IR-ID:ir-block-id outs:A64EFF:placeseq :}
   V-BLKR VW V-OPR VW MKEY bk IR-FUN:FTERMINATOR@ {: id:IR-ID:ir-op-id :}
   id TAILBR? if id OUT-TAIL-CK exit then
   V-DSTACK @ 0<> id OPERANDS-OF 0<> and
   if E-A64RAV-PLACE throw then
   outs REG-POSITIONS {: n:n :}
   id OPERANDS-OF n < if E-A64RAV-FIXED throw then
   n 0 ?do
      id i OPERAND-AT SLOT A64RA:CLAIM@
      outs i A64EFF:SEQ-REG@ <> if E-A64RAV-FIXED throw then
   loop ;

\ ---- the frame -----------------------------------------------------------------
\ A block that touches the frame at all takes it with its first operation and
\ gives it back with its last.
: FRAMES? ( IR-ID:ir-block-id -- bool )
   {: bk:IR-ID:ir-block-id :}
   false
   bk OP-COUNT 0 ?do
      bk i OP-AT FRAME-TOUCH? if drop true leave then
   loop ;

: FRAME-AT? ( IR-ID:ir-block-id n n -- )
   {: bk:IR-ID:ir-block-id at:n want:n :}
   bk at OP-AT {: id:IR-ID:ir-op-id :}
   id SLOT-OF NOSLOT <> if E-A64RAV-FRAME throw then
   id FRAME-OF want <> if E-A64RAV-FRAME throw then ;

\ ---- the data stack ----------------------------------------------------------
\ A routine whose convention names data-stack slots reaches the caller's stack
\ and one whose convention names registers never does.
: DMOVE-AT? ( IR-ID:ir-block-id n n -- )
   {: bk:IR-ID:ir-block-id at:n want:n :}
   bk at OP-AT {: id:IR-ID:ir-op-id :}
   id DSLOT-OF NOSLOT <> if E-A64RAV-DSTACK throw then
   id DBYTES-OF want <> if E-A64RAV-DSTACK throw then ;

: FLOW-CK ( IR-ID:ir-block-id -- )
   {: bk:IR-ID:ir-block-id :}
   bk OP-COUNT 0 ?do
      bk i OP-AT {: id:IR-ID:ir-op-id :}
      id SLOT-OF {: off:n :}
      off NOSLOT <> if
         off A64IR:SLOT-WIDTH / {: s:n :}
         s 0 < s SLOTS-MAX >= or if E-A64RAV-SLOT throw then
         id STORES? if
            s cells W-AT + @ 0 >= if E-A64RAV-SHARE throw then
            i s cells W-AT + !
         else
            s cells W-AT + @ 0 < if E-A64RAV-RELOAD throw then
         then
      then
   loop ;

: SLOT-CK ( IR-ID:ir-fun-id A64EFF:routine -- )
   A64EFF:VALIDATE A64EFF-ROUTINE:UNMAKE
   {: cv:A64EFF:conv gi:A64EFF:placeseq gr:A64EFF:placeseq gc:A64EFF:gprs
      fi:A64EFF:fprs fr:A64EFF:fprs fc:A64EFF:fprs
      z:A64EFF:nzcv l:A64EFF:link ct:A64EFF:control t:A64EFF:traits
      size:n delta:n :}
   {: f:IR-ID:ir-fun-id :}
   f BLOCK-COUNT 0 ?do
      f i BLOCK-AT {: bk:IR-ID:ir-block-id :}
      bk OP-COUNT 0 ?do
         bk i OP-AT SLOT-OF {: off:n :}
         off NOSLOT <> if
            off A64IR:SLOT-WIDTH
            cv gi gr gc fi fr fc z l ct t size delta A64EFF-ROUTINE:MAKE
            A64EFF:CHECK-SLOT
         then
      loop
   loop ;

\ ---- re-deriving the allocation ----------------------------------------------
\ Live ranges are held as bit sets over positions of one number line.
64 constant SET-BITS
VMAX SET-BITS / constant SETC
0 constant P-IN
1 constant P-OUT
2 constant P-USE
3 constant P-DEF
4 constant PLANES

here CELL 1- and CELL swap - CELL 1- and allot
variable V-BLKS
0 V-BLKS !
variable V-AT
0 V-AT !
variable V-CHANGED
0 V-CHANGED !
variable V-LSAVE                     \ whether the contract's prologue keeps the caller's return address
0 V-LSAVE !
variable V-TAIL                      \ whether the contract says control leaves through a callee
0 V-TAIL !
variable V-NORET                     \ whether the contract says control never comes back
0 V-NORET !
variable V-FRAME                     \ whether the module reaches a frame at all
0 V-FRAME !
variable V-BASE                      \ the first frame byte the allocator's slots may use
0 V-BASE !
variable VD-AT                       \ the position the data-stack scan stands on
0 VD-AT !

create VB-ST BMAX cells allot
create VB-EN BMAX cells allot
create V-SETS PLANES BMAX * SETC * cells allot
create V-TMP SETC cells allot

: VBIT-CELL ( n -- n )   SET-BITS / ;
: VBIT-MASK ( n -- n )   SET-BITS mod 1 swap lshift ;

: VS-IX ( n n n -- n )
   {: pl:n b:n w:n :}
   pl BMAX * b + SETC * w + ;

: VS@ ( n n n -- n )     VS-IX cells V-SETS + @ ;

: VS! ( n n n n -- )
   {: val:n pl:n b:n w:n :}
   val  pl b w VS-IX cells V-SETS + ! ;

: VS-HAS? ( n n n -- bool )
   {: pl:n b:n v:n :}
   pl b v VBIT-CELL VS@  v VBIT-MASK and 0<> ;

: VS-SET ( n n n -- )
   {: pl:n b:n v:n :}
   pl b v VBIT-CELL VS@  v VBIT-MASK or  pl b v VBIT-CELL VS! ;

: VTMP-CLEAR ( -- )
   SETC 0 ?do 0 i cells V-TMP + ! loop ;

: VTMP-HAS? ( n -- bool )
   {: v:n :}
   v VBIT-CELL cells V-TMP + @  v VBIT-MASK and 0<> ;

: VTMP-SET ( n -- )
   {: v:n :}
   v VBIT-CELL cells V-TMP + @  v VBIT-MASK or
   v VBIT-CELL cells V-TMP + ! ;

: VSETS-CLEAR ( -- )
   PLANES BMAX * SETC * 0 ?do 0 i cells V-SETS + ! loop ;

\ ---- the linear order --------------------------------------------------------
: VLAY1 ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id b:n :}
   V-AT @ b cells VB-ST + !
   V-AT @  f b BLOCK-AT OP-COUNT  + {: e:n :}
   e b cells VB-EN + !
   e 1+ V-AT ! ;

\ The positions run across the whole MODULE and not from each function's own
\ zero, so two functions' values can never be measured against one line twice.
: VLAYOUT ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id base:n :}
   f BLOCK-COUNT {: n:n :}
   n BMAX > if E-A64RAV-COVER throw then
   n V-BLKS !
   f VB-BASE!
   base V-AT !
   n 0 ?do f i VLAY1 loop ;

: VOP-POS ( n n -- n )
   {: b:n i:n :}
   b cells VB-ST + @ 1+ i + ;

\ ---- liveness ----------------------------------------------------------------
: VUSE1 ( n IR-ID:ir-value-id -- )
   {: b:n id:IR-ID:ir-value-id :}
   id SLOT {: v:n :}
   v VTMP-HAS? if exit then
   P-USE b v VS-SET ;

: VDEF1 ( n IR-ID:ir-value-id -- )
   {: b:n id:IR-ID:ir-value-id :}
   id SLOT {: v:n :}
   P-DEF b v VS-SET
   v VTMP-SET ;

: VOP-UD ( n IR-ID:ir-op-id -- )
   {: b:n id:IR-ID:ir-op-id :}
   id OPERANDS-OF 0 ?do b  id i OPERAND-AT  VUSE1 loop
   id RESULTS-OF 0 ?do  b  id i RESULT-AT   VDEF1 loop ;

: VBLOCK-UD ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id b:n :}
   f b BLOCK-AT {: bk:IR-ID:ir-block-id :}
   VTMP-CLEAR
   bk ARG-COUNT 0 ?do b  bk i ARG-AT  VDEF1 loop
   bk OP-COUNT 0 ?do  b  bk i OP-AT   VOP-UD loop ;

: VSUCC-ORD ( IR-ID:ir-op-id n -- n )
   SUCC-AT IR-ID:BLOCK-LOCAL  VB-BASE @ -
   dup 0 < over V-BLKS @ >= or if E-A64RAV-SHAPE throw then ;

: VOUT-ADD ( n n -- )
   {: b:n s:n :}
   SETC 0 ?do
      P-OUT b i VS@  P-IN s i VS@ or  P-OUT b i VS!
   loop ;

: VOUT ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id b:n :}
   SETC 0 ?do 0 P-OUT b i VS! loop
   f b BLOCK-AT TERM-AT {: t:IR-ID:ir-op-id :}
   t SUCCS-OF 0 ?do
      b  t i VSUCC-ORD  VOUT-ADD
   loop ;

: VIN1 ( n n -- bool )
   {: b:n w:n :}
   P-USE b w VS@   P-OUT b w VS@  P-DEF b w VS@ invert and   or {: nv:n :}
   nv  P-IN b w VS@ = if false exit then
   nv P-IN b w VS!
   true ;

: VIN ( n -- bool )
   {: b:n :}
   false
   SETC 0 ?do b i VIN1 or loop ;

: VPASS1 ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id b:n :}
   f b VOUT
   b VIN if 1 V-CHANGED ! then ;

: VLIVENESS ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   VSETS-CLEAR
   V-BLKS @ 0 ?do f i VBLOCK-UD loop
   begin
      0 V-CHANGED !
      V-BLKS @ 0 ?do
         f  V-BLKS @ 1- i -  VPASS1
      loop
      V-CHANGED @ 0=
   until ;

\ ---- the hull ranges ---------------------------------------------------------
: VOP-RANGE ( IR-ID:ir-op-id n -- )
   {: id:IR-ID:ir-op-id pos:n :}
   id OPERANDS-OF 0 ?do id i OPERAND-AT pos NOTE-USE loop
   id RESULTS-OF 0 ?do  id i RESULT-AT  pos NOTE-DEF loop ;

: VBLOCK-RANGE ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id b:n :}
   f b BLOCK-AT {: bk:IR-ID:ir-block-id :}
   bk ARG-COUNT 0 ?do
      bk i ARG-AT  b cells VB-ST + @  NOTE-DEF
   loop
   bk OP-COUNT 0 ?do
      bk i OP-AT  b i VOP-POS  VOP-RANGE
   loop ;

: VEXTEND1 ( n n -- )
   {: b:n k:n :}
   P-IN b k VS-HAS? if
      b cells VB-ST + @  k DEF-AT min  k DEF!
   then
   P-OUT b k VS-HAS? if
      b cells VB-EN + @  k LAST-AT max  k LAST!
   then ;

: VEXTEND-V ( n -- )
   {: k:n :}
   V-BLKS @ 0 ?do i k VEXTEND1 loop ;

: VMEASURE1 ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id base:n :}
   f base VLAYOUT
   f VLIVENESS
   V-BLKS @ 0 ?do f i VBLOCK-RANGE loop
   N-VALS @ 0 ?do
      i DEF-AT NOPOS <>  i DEF-AT base >=  and if i VEXTEND-V then
   loop ;

\ ---- the edge clause ---------------------------------------------------------
\ A branch moves nothing, so the register holding the value a terminator hands
\ over is the register holding the destination's argument at that index.
: VEDGE1 ( IR-ID:ir-op-id IR-ID:ir-block-id n -- )
   {: t:IR-ID:ir-op-id sb:IR-ID:ir-block-id i:n :}
   t i OPERAND-AT SLOT A64RA:CLAIM@
   sb i ARG-AT SLOT A64RA:CLAIM@
   <> if E-A64RAV-EDGE throw then ;

\ A terminator with more than one successor hands nothing over - its operands
\ are what it tests, not block arguments.
: VMULTI-CK ( IR-ID:ir-fun-id IR-ID:ir-op-id -- )
   {: f:IR-ID:ir-fun-id t:IR-ID:ir-op-id :}
   t SUCCS-OF 0 ?do
      f  t i VSUCC-ORD  BLOCK-AT ARG-COUNT 0<> if E-A64RAV-EDGE throw then
   loop ;

: VEDGE-OF ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id b:n :}
   f b BLOCK-AT TERM-AT {: t:IR-ID:ir-op-id :}
   t SUCCS-OF 1 <> if f t VMULTI-CK exit then
   f  t 0 VSUCC-ORD  BLOCK-AT {: sb:IR-ID:ir-block-id :}
   t OPERANDS-OF sb ARG-COUNT <> if E-A64RAV-EDGE throw then
   t OPERANDS-OF 0 ?do t sb i VEDGE1 loop ;

: VEDGE-CK ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   V-BLKS @ 0 ?do f i VEDGE-OF loop ;

\ ---- the frame of a routine with control flow --------------------------------
\ A routine of more than one block reaches its frame in exactly TWO blocks: the
\ one that takes it and the one that gives it back.
: VNO-FRAME ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   V-BLKS @ 0 ?do
      f i BLOCK-AT FRAMES? if E-A64RAV-FRAME throw then
   loop ;

: VANY-FRAME ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   0 V-FRAME !
   V-BLKS @ 0 ?do
      f i BLOCK-AT FRAMES? if 1 V-FRAME ! then
   loop ;

: VLINK-AT? ( IR-ID:ir-block-id n bool -- )
   {: bk:IR-ID:ir-block-id at:n store:bool :}
   bk at OP-AT {: id:IR-ID:ir-op-id :}
   id FRAME-OF NOSLOT <> if E-A64RAV-CALL throw then
   id SLOT-OF A64FRAME:LINK-SLOT <> if E-A64RAV-CALL throw then
   store if
      id STORES? 0= if E-A64RAV-CALL throw then exit
   then
   id STORES? if E-A64RAV-CALL throw then ;

: VFRAME-BLOCKS-CK ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id rb:n :}
   V-BLKS @ 0 ?do
      i 0 <> i rb <> and if
         f i BLOCK-AT FRAMES? if E-A64RAV-FRAME throw then
      then
   loop ;


: VNO-SIZE ( IR-ID:ir-block-id n n -- )
   {: bk:IR-ID:ir-block-id keep:n also:n :}
   bk OP-COUNT 0 ?do
      i keep <> i also <> and if
         bk i OP-AT FRAME-OF NOSLOT <> if E-A64RAV-FRAME throw then
      then
   loop ;

: VLINK-HERE? ( IR-ID:ir-block-id n n n -- bool )
   {: bk:IR-ID:ir-block-id b:n rb:n at:n :}
   V-LSAVE @ 0= if false exit then
   b 0 = at 1 = and if true exit then
   b rb = at bk OP-COUNT 3 - = and if true exit then
   false ;

: VOWNER1 ( IR-ID:ir-block-id n n n -- )
   {: bk:IR-ID:ir-block-id b:n rb:n at:n :}
   bk at OP-AT SLOT-OF {: off:n :}
   off NOSLOT = if exit then
   bk b rb at VLINK-HERE? if
      off A64FRAME:LINK-SLOT <> if E-A64RAV-OWNER throw then
      exit
   then
   off V-BASE @ < if E-A64RAV-OWNER throw then ;

: VOWNER-BLOCK ( IR-ID:ir-block-id n n -- )
   {: bk:IR-ID:ir-block-id b:n rb:n :}
   bk OP-COUNT 0 ?do bk b rb i VOWNER1 loop ;

: VOWNER-CK ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id rb:n :}
   V-BLKS @ 0 ?do
      f i BLOCK-AT {: bk:IR-ID:ir-block-id :}
      bk i rb VOWNER-BLOCK
   loop ;

: VBRACKET-CK ( IR-ID:ir-fun-id n n n -- )
   {: f:IR-ID:ir-fun-id rb:n want:n bad:n :}
   f 0 BLOCK-AT {: eb:IR-ID:ir-block-id :}
   f rb BLOCK-AT {: xb:IR-ID:ir-block-id :}
   xb OP-COUNT {: n:n :}
   n 2 < if bad throw then
   rb 0 = n 3 < and if bad throw then
   eb 0 want FRAME-AT?
   xb n 2 - want FRAME-AT?
   rb 0 = if
      eb 0 n 2 - VNO-SIZE
      f rb VFRAME-BLOCKS-CK
      exit
   then
   eb 0 NOPOS VNO-SIZE
   xb n 2 - NOPOS VNO-SIZE
   f rb VFRAME-BLOCKS-CK ;

\ A routine that does not call has nothing in its frame but what the register
\ allocator put there.
: VSPILL-CK ( IR-ID:ir-fun-id n n -- )
   {: f:IR-ID:ir-fun-id rb:n want:n :}
   V-FRAME @ 0= if f VNO-FRAME exit then
   f rb want E-A64RAV-FRAME VBRACKET-CK ;

\ A routine that DOES call keeps its caller's return address in the bottom slot,
\ and the allocator's own slots start above it.
: VLINK-CK ( IR-ID:ir-fun-id n n -- )
   {: f:IR-ID:ir-fun-id rb:n want:n :}
   f 0 BLOCK-AT {: eb:IR-ID:ir-block-id :}
   f rb BLOCK-AT {: xb:IR-ID:ir-block-id :}
   eb OP-COUNT 2 < if E-A64RAV-CALL throw then
   xb OP-COUNT {: n:n :}
   n 3 < if E-A64RAV-CALL throw then
   rb 0 = n 5 < and if E-A64RAV-CALL throw then
   f rb want E-A64RAV-CALL VBRACKET-CK
   eb 1 true VLINK-AT?
   xb n 3 - false VLINK-AT? ;

\ A routine that never returns takes its frame and does not give it back, so its
\ declared delta is the whole of it.
: VNO-RET-BRACKET-CK ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id want:n :}
   f 0 BLOCK-AT {: eb:IR-ID:ir-block-id :}
   eb 0 want FRAME-AT?
   eb 0 NOPOS VNO-SIZE
   f NO-RET VFRAME-BLOCKS-CK ;

: VNO-RET-SPILL-CK ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id want:n :}
   V-FRAME @ 0= if f VNO-FRAME exit then
   f want VNO-RET-BRACKET-CK ;

: VNO-RET-LINK-CK ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id want:n :}
   f 0 BLOCK-AT {: eb:IR-ID:ir-block-id :}
   eb OP-COUNT 2 < if E-A64RAV-CALL throw then
   f want VNO-RET-BRACKET-CK
   eb 1 true VLINK-AT? ;

: VNO-RET-FRAME-CK ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id want:n :}
   V-LSAVE @ 0= if f want VNO-RET-SPILL-CK else f want VNO-RET-LINK-CK then
   f NO-RET VOWNER-CK ;

: VFRAME-CK ( IR-ID:ir-fun-id n n -- )
   {: f:IR-ID:ir-fun-id rb:n want:n :}
   rb NO-RET = if f want VNO-RET-FRAME-CK exit then
   V-LSAVE @ 0= if f rb want VSPILL-CK else f rb want VLINK-CK then
   f rb VOWNER-CK ;

\ ---- the data stack, across blocks -------------------------------------------
: VNO-DSTACK ( IR-ID:ir-block-id -- )
   {: bk:IR-ID:ir-block-id :}
   bk OP-COUNT 0 ?do
      bk i OP-AT DSTACK-TOUCH? if E-A64RAV-DSTACK throw then
   loop ;

: VB-OPS ( n -- n )
   {: b:n :}
   b cells VB-EN + @  b cells VB-ST + @ - ;

: PRO-N ( -- n )
   V-LSAVE @ 0<> if 2 exit then
   V-FRAME @ 0<> if 1 exit then
   0 ;

\ ---- what each slot of the caller's stack holds ------------------------------
\ The residency is a SECOND derivation: the selector decides a value already
\ lies in the slot it would be stored to, and this re-derives that from the
\ module rather than reading the selector's claim.
64 constant VDSLOTS                  \ slots the residency is tracked over
-1 constant VD-BOT                   \ nothing is known about this slot
-2 constant VD-TOP                   \ nothing has been said about it yet
0 constant VD-UNDEF
1 constant VD-DEF

here CELL 1- and CELL swap - CELL 1- and allot
create VD-VIN BMAX VDSLOTS * cells allot    \ the value each slot holds at a block's head
create VD-VOUT BMAX VDSLOTS * cells allot   \ and at its end
create VD-DIN BMAX VDSLOTS * cells allot    \ whether each slot is defined there
create VD-DOUT BMAX VDSLOTS * cells allot
create VD-VCUR VDSLOTS cells allot
create VD-DCUR VDSLOTS cells allot
create VD-VMEET VDSLOTS cells allot
create VD-DMEET VDSLOTS cells allot
variable VD-EL                       \ argument loads the entry sequence really built
variable VD-ES                       \ result stores the exit sequence really built
variable VD-P                        \ the position one of the scans below stands at
variable VD-J                        \ the declared place one of them stands at
variable VD-S
variable VD-PREV
variable VD-MOVED

\ ---- where the pointer stands, re-derived ------------------------------------
\ Every number the four data-stack forms carry is re-derived, including where
\ the body's pointer stands, because a place that is merely read proves nothing.
variable VD-STAND                    \ where the body's data-stack pointer stands
0 VD-STAND !
variable VD-ENTRY                    \ where the caller leaves it: 8*in
0 VD-ENTRY !
variable VD-LEAVE                    \ where the caller expects it back: 8*out
0 VD-LEAVE !

256 constant VDREQ-MAX               \ places one routine's collection holds

here CELL 1- and CELL swap - CELL 1- and allot
create VD-REQ VDREQ-MAX cells allot
variable VD-REQ-N
variable VD-REQ-OVER
variable VD-BEST
variable VD-BCOST

: VD-WINDOW? ( n -- bool )
   dup 0 < if drop false exit then
   VDSLOTS < ;

: VDV-IN ( n n -- n )
   {: b:n s:n :}
   s VD-WINDOW? 0= if VD-BOT exit then
   b VDSLOTS * s + cells VD-VIN + @ ;

: VDV-IN! ( n n n -- )
   {: v:n b:n s:n :}
   s VD-WINDOW? 0= if exit then
   v  b VDSLOTS * s + cells VD-VIN + ! ;

: VDD-IN ( n n -- n )
   {: b:n s:n :}
   s VD-WINDOW? 0= if VD-DEF exit then
   b VDSLOTS * s + cells VD-DIN + @ ;

: VDD-IN! ( n n n -- )
   {: v:n b:n s:n :}
   s VD-WINDOW? 0= if exit then
   v  b VDSLOTS * s + cells VD-DIN + ! ;

: VDV-OUT ( n n -- n )
   {: b:n s:n :}
   s VD-WINDOW? 0= if VD-BOT exit then
   b VDSLOTS * s + cells VD-VOUT + @ ;

: VDV-OUT! ( n n n -- )
   {: v:n b:n s:n :}
   s VD-WINDOW? 0= if exit then
   v  b VDSLOTS * s + cells VD-VOUT + ! ;

: VDD-OUT ( n n -- n )
   {: b:n s:n :}
   s VD-WINDOW? 0= if VD-DEF exit then
   b VDSLOTS * s + cells VD-DOUT + @ ;

: VDD-OUT! ( n n n -- )
   {: v:n b:n s:n :}
   s VD-WINDOW? 0= if exit then
   v  b VDSLOTS * s + cells VD-DOUT + ! ;

\ ---- the running map, and what one operation does to it ----------------------
: VDCUR<IN ( n -- )
   {: b:n :}
   VDSLOTS 0 ?do
      b i VDV-IN  i cells VD-VCUR + !
      b i VDD-IN  i cells VD-DCUR + !
   loop ;

: VDOUT<CUR ( n -- )
   {: b:n :}
   VDSLOTS 0 ?do
      i cells VD-VCUR + @  b i VDV-OUT!
      i cells VD-DCUR + @  b i VDD-OUT!
   loop ;

: VDV@ ( n -- n )
   dup VD-WINDOW? 0= if drop VD-BOT exit then
   cells VD-VCUR + @ ;

: VDD@ ( n -- n )
   dup VD-WINDOW? 0= if drop VD-DEF exit then
   cells VD-DCUR + @ ;

: VDPUT ( n n n -- )
   {: v:n d:n s:n :}
   s VD-WINDOW? 0= if exit then
   v s cells VD-VCUR + !
   d s cells VD-DCUR + ! ;

: VDCELL ( n -- n )
   VD-STAND @ + {: off:n :}
   off 0 < if E-A64RAV-DSTACK throw then
   off A64IR:SLOT-WIDTH mod 0<> if E-A64RAV-DSTACK throw then
   off A64IR:SLOT-WIDTH / ;

: VDREACH-CK ( n -- )
   {: off:n :}
   off A64EFF:SLOT-BACK negate < if E-A64RAV-DSTACK throw then
   off A64IR:SLOT-WIDTH A64EFF:SLOT-REACH > if E-A64RAV-DSTACK throw then ;

: VDSLOT-CELL ( IR-ID:ir-op-id -- n )
   {: id:IR-ID:ir-op-id :}
   id DSLOT-OF {: off:n :}
   off VDREACH-CK
   off VDCELL ;

: VD-NAMED? ( n -- bool )
   dup 0 >= swap N-VALS @ < and ;

: VDARG ( n -- n )
   VMAX + ;

: VDCALLRES ( IR-ID:ir-op-id n -- n )
   {: id:IR-ID:ir-op-id j:n :}
   VMAX VDSLOTS +  id IR-ID:OP-LOCAL VDSLOTS * +  j + ;

: VDCALL-XFER ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id DBACK-OF VDCELL {: back:n :}
   VDSLOTS 0 ?do VD-BOT VD-UNDEF i VDPUT loop
   back 0 ?do
      id i VDCALLRES  VD-DEF  i VDPUT
   loop ;

\ A store through an address the program computed may have reached a data-stack
\ slot, so it clobbers what this pass believed about the slot's contents.
: VDCLOBBER? ( IR-ID:ir-op-id -- bool )
   {: id:IR-ID:ir-op-id :}
   id STORES? 0= if false exit then
   id DSTACK-TOUCH? 0= ;

: VDCLOBBER-XFER ( -- )
   VDSLOTS 0 ?do
      i VDD@  VD-BOT swap  i VDPUT
   loop ;

\ ---- what a data-stack refusal was about, kept for a reader -------------------
\ One code, three reasons, and a caller who sees neither the operation nor the
\ module: the reason is published beside the refusal so a reader can say which.
1 constant DKEEP-NAMED       \ a load into a slot already holding a named value
2 constant DKEEP-DEAD        \ a load whose result nothing reads
3 constant DKEEP-SAME        \ a store of the value the slot already holds

1 TYPED-BUFFER LAST-FUN IR-ID:ir-fun-id
1 TYPED-BUFFER LAST-OP IR-ID:ir-op-id
variable LAST-CLAUSE
variable LAST-HELD           \ has a residency run ever filled the three above

: DKEEP-HOOK-NONE ( -- ) ;

\ PUBLIC BECAUSE A CAPTURED CHAIN HAS TO RE-RUN IT. DKEEP-HOOK is the one
\ declared address cell in the compiler chain's capture window, and a captured
\ window's declared cells arrive trapped: what they held was a code address in
\ the process that compiled them, so the seed re-traps each one and the boot-run
\ list is what installs the real vector (src/habu/aot-decl.f says why). The
\ capture tool puts this name on that list, and LFIND has to find it. There is no
\ wrapper: a second name for a one-cell install is the forwarder that grows a
\ second body.
public
: DKEEP-HOOK-DEFAULT ( -- )
   [: DKEEP-HOOK-NONE ;] is A64RAV:DKEEP-HOOK ;
private
DKEEP-HOOK-DEFAULT

: DKEEP! ( IR-ID:ir-op-id n -- )
   {: id:IR-ID:ir-op-id clause:n :}
   id 0 LAST-OP !
   clause LAST-CLAUSE !
   DKEEP-HOOK
   E-A64RAV-DKEEP throw ;

\ ---- one operation, measured -------------------------------------------------
: VDLOAD-CK ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id VDSLOT-CELL {: s:n :}
   s VDD@ VD-DEF <> if E-A64RAV-DRES throw then
   s VDV@ VD-NAMED? if id DKEEP-NAMED DKEEP! then
   id 0 RESULT-AT SLOT {: k:n :}
   k USES-AT 0= if id DKEEP-DEAD DKEEP! then
   k VD-DEF s VDPUT ;

: VDSTORE-CK ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id VDSLOT-CELL {: s:n :}
   id 0 OPERAND-AT SLOT {: k:n :}
   s VDV@ k = if id DKEEP-SAME DKEEP! then
   k VD-DEF s VDPUT ;

: VDPUBLISH-CK ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id DBYTES-OF VDCELL 0 ?do
      i VDD@ VD-DEF <> if E-A64RAV-DRES throw then
   loop ;

: VDOUTS-CK ( A64EFF:placeseq n -- )
   {: outs:A64EFF:placeseq r:n :}
   r 0 ?do
      outs i A64EFF:SEQ-SLOT@ VDD@ VD-DEF <> if E-A64RAV-DRES throw then
   loop ;

: VDOP-XFER ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id DCALL? if id VDCALL-XFER exit then
   id DSLOT-OF NOSLOT <> if
      id STORES? if id VDSTORE-CK exit then
      id VDLOAD-CK exit
   then
   id VDCLOBBER? if VDCLOBBER-XFER then ;

\ ---- the fixpoint ------------------------------------------------------------
: VDMEET-V ( n n -- n )
   {: a:n b:n :}
   a VD-TOP = if b exit then
   b VD-TOP = if a exit then
   a b = if a exit then
   VD-BOT ;

: VDMEET-D ( n n -- n )
   {: a:n b:n :}
   a VD-TOP = if b exit then
   b VD-TOP = if a exit then
   a b = if a exit then
   VD-UNDEF ;

\ A value a branch hands to argument `i` is read as that argument on the way in,
\ so the two are one register by the edge rule.
: VDXLATE ( IR-ID:ir-op-id IR-ID:ir-block-id n -- n )
   {: t:IR-ID:ir-op-id tb:IR-ID:ir-block-id v:n :}
   v VD-NAMED? 0= if v exit then
   tb ARG-COUNT {: k:n :}
   t OPERANDS-OF k <> if v exit then
   v
   k 0 ?do
      t i OPERAND-AT SLOT v = if
         drop  tb i ARG-AT SLOT  leave
      then
   loop ;

: VDMEET-EDGE ( IR-ID:ir-op-id IR-ID:ir-block-id n -- )
   {: t:IR-ID:ir-op-id tb:IR-ID:ir-block-id p:n :}
   VDSLOTS 0 ?do
      i cells VD-VMEET + @   t tb  p i VDV-OUT  VDXLATE   VDMEET-V
      i cells VD-VMEET + !
      i cells VD-DMEET + @   p i VDD-OUT   VDMEET-D
      i cells VD-DMEET + !
   loop ;

: VDEDGE? ( IR-ID:ir-op-id n -- bool )
   {: t:IR-ID:ir-op-id b:n :}
   false
   t SUCCS-OF 0 ?do
      t i VSUCC-ORD b = if drop true leave then
   loop ;

: VDMEET-FROM ( IR-ID:ir-fun-id n n -- )
   {: f:IR-ID:ir-fun-id p:n b:n :}
   f p BLOCK-AT TERM-AT {: t:IR-ID:ir-op-id :}
   t b VDEDGE? 0= if exit then
   t  f b BLOCK-AT  p VDMEET-EDGE ;

: VDIN-SET? ( n n n n -- bool )
   {: v:n d:n b:n s:n :}
   false
   b s VDV-IN v <> if v b s VDV-IN! drop true then
   b s VDD-IN d <> if d b s VDD-IN! drop true then ;

: VDMEET-BLOCK ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id b:n :}
   VDSLOTS 0 ?do
      VD-TOP i cells VD-VMEET + !
      VD-TOP i cells VD-DMEET + !
   loop
   V-BLKS @ 0 ?do  f i b VDMEET-FROM  loop
   VDSLOTS 0 ?do
      i cells VD-VMEET + @  i cells VD-DMEET + @  b i VDIN-SET?
      if 1 VD-MOVED ! then
   loop ;

: VDXFER-BLOCK ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id b:n :}
   b VDCUR<IN
   f b BLOCK-AT {: bk:IR-ID:ir-block-id :}
   bk OP-COUNT 0 ?do  bk i OP-AT VDOP-XFER  loop
   b VDOUT<CUR ;

: VDENTRY-IN ( n A64EFF:placeseq -- )
   {: a:n args:A64EFF:placeseq :}
   VDSLOTS 0 ?do
      VD-BOT 0 i VDV-IN!
      VD-UNDEF 0 i VDD-IN!
   loop
   a 0 ?do
      i VDARG  0  args i A64EFF:SEQ-SLOT@  VDV-IN!
      VD-DEF   0  args i A64EFF:SEQ-SLOT@  VDD-IN!
   loop ;

: VDIN-ANY ( n -- )
   {: b:n :}
   VDSLOTS 0 ?do
      VD-TOP b i VDV-IN!
      VD-TOP b i VDD-IN!
   loop ;

BMAX VDSLOTS * 4 * 2 + constant VD-ROUNDS

: VDRES-FIX ( IR-ID:ir-fun-id n A64EFF:placeseq -- )
   {: f:IR-ID:ir-fun-id a:n args:A64EFF:placeseq :}
   V-BLKS @ 1 ?do i VDIN-ANY loop
   a args VDENTRY-IN
   V-BLKS @ 0 ?do f i VDXFER-BLOCK loop
   0
   begin
      dup VD-ROUNDS >= if E-A64RAV-DRES throw then
      0 VD-MOVED !
      V-BLKS @ 1 ?do  f i VDMEET-BLOCK  loop
      V-BLKS @ 0 ?do  f i VDXFER-BLOCK  loop
      1+
      VD-MOVED @ 0=
   until
   drop ;

\ ---- the checked pass --------------------------------------------------------
: VDCK-OP ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id DCALL? if id VDPUBLISH-CK  id VDCALL-XFER exit then
   id DSLOT-OF NOSLOT <> if
      id STORES? if id VDSTORE-CK exit then
      id VDLOAD-CK exit
   then
   id VDCLOBBER? if VDCLOBBER-XFER then ;

: VDCK-BLOCK ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id b:n :}
   b VDCUR<IN
   f b BLOCK-AT {: bk:IR-ID:ir-block-id :}
   bk OP-COUNT 0 ?do  bk i OP-AT VDCK-OP  loop ;

: VDPUB-BACK ( -- n )
   V-TAIL @ 0<> if 1 exit then
   2 ;

: VDCK-EXIT ( IR-ID:ir-fun-id n n A64EFF:placeseq -- )
   {: f:IR-ID:ir-fun-id rb:n r:n outs:A64EFF:placeseq :}
   rb VDCUR<IN
   f rb BLOCK-AT {: xb:IR-ID:ir-block-id :}
   xb OP-COUNT PRO-N - VDPUB-BACK - {: pub:n :}
   pub 0 ?do  xb i OP-AT VDOP-XFER  loop
   outs r VDOUTS-CK ;

: VDRES-CK ( IR-ID:ir-fun-id n n n A64EFF:placeseq A64EFF:placeseq -- )
   {: f:IR-ID:ir-fun-id rb:n a:n r:n args:A64EFF:placeseq outs:A64EFF:placeseq :}
   f 0 LAST-FUN !
   0 LAST-CLAUSE !
   1 LAST-HELD !
   f a args VDRES-FIX
   V-BLKS @ 0 ?do f i VDCK-BLOCK loop
   rb NO-RET <> if f rb r outs VDCK-EXIT then
   DKEEP-HOOK ;

\ ---- the entry and exit sequences, re-derived --------------------------------
: VDLOAD? ( IR-ID:ir-block-id n -- bool )
   {: bk:IR-ID:ir-block-id at:n :}
   bk at OP-AT {: id:IR-ID:ir-op-id :}
   id DSLOT-OF NOSLOT = if false exit then
   id STORES? 0= ;

: VDSTORE? ( IR-ID:ir-block-id n -- bool )
   {: bk:IR-ID:ir-block-id at:n :}
   bk at OP-AT {: id:IR-ID:ir-op-id :}
   id DSLOT-OF NOSLOT = if false exit then
   id STORES? ;

: VDSLOT-AT ( IR-ID:ir-block-id n -- n )
   OP-AT VDSLOT-CELL ;

: VDSEQ-FIND ( A64EFF:placeseq n n -- )
   {: seq:A64EFF:placeseq len:n s:n :}
   begin
      VD-J @ len >= if E-A64RAV-DSTACK throw then
      seq VD-J @ A64EFF:SEQ-SLOT@ s =
      VD-J @ 1+ VD-J !
   until ;

: VDSTAND-AT ( IR-ID:ir-block-id n n -- )
   {: bk:IR-ID:ir-block-id at:n entry:n :}
   bk at OP-AT {: id:IR-ID:ir-op-id :}
   id DSLOT-OF NOSLOT <> if E-A64RAV-DSTACK throw then
   id DBYTES-OF NOSLOT = if E-A64RAV-DSTACK throw then
   entry id DBYTES-OF - {: stand:n :}
   stand 0 < if E-A64RAV-DSTACK throw then
   stand A64EFF:SLOT-BACK > if E-A64RAV-DSTACK throw then
   stand A64IR:SLOT-WIDTH mod 0<> if E-A64RAV-DSTACK throw then
   stand VD-STAND ! ;

: VDENTRY-CK ( IR-ID:ir-fun-id n A64EFF:placeseq -- )
   {: f:IR-ID:ir-fun-id a:n args:A64EFF:placeseq :}
   f 0 BLOCK-AT {: eb:IR-ID:ir-block-id :}
   eb OP-COUNT PRO-N 1+ < if E-A64RAV-DSTACK throw then
   eb PRO-N  a A64IR:SLOT-WIDTH *  VDSTAND-AT
   PRO-N 1+ VD-P !
   0 VD-J !
   0 VD-EL !
   begin
      VD-P @ eb OP-COUNT < if eb VD-P @ VDLOAD? else false then
   while
      args a  eb VD-P @ VDSLOT-AT  VDSEQ-FIND
      VD-P @ 1+ VD-P !
      VD-EL @ 1+ VD-EL !
   repeat ;

: VDEXIT-CK ( IR-ID:ir-fun-id n n A64EFF:placeseq -- )
   {: f:IR-ID:ir-fun-id rb:n r:n outs:A64EFF:placeseq :}
   f rb BLOCK-AT {: xb:IR-ID:ir-block-id :}
   xb OP-COUNT PRO-N - {: n:n :}
   n 2 < if E-A64RAV-DSTACK throw then
   xb n 2 -  r A64IR:SLOT-WIDTH * VD-STAND @ -  DMOVE-AT?
   n 2 - VD-P !
   0 VD-ES !
   begin
      VD-P @ 0 > if xb VD-P @ 1- VDSTORE? else false then
   while
      VD-P @ 1- VD-P !
      VD-ES @ 1+ VD-ES !
   repeat
   0 VD-J !
   VD-ES @ 0 ?do
      outs r  xb  VD-P @ i +  VDSLOT-AT  VDSEQ-FIND
   loop ;

\ The same window for a routine that leaves through a CALLEE, whose results the
\ callee publishes into the very cells this routine's caller reads.
: VDTAIL-CK ( IR-ID:ir-fun-id n n A64EFF:placeseq -- )
   {: f:IR-ID:ir-fun-id rb:n r:n outs:A64EFF:placeseq :}
   f rb BLOCK-AT {: xb:IR-ID:ir-block-id :}
   xb OP-COUNT PRO-N - {: n:n :}
   n 1 < if E-A64RAV-DSTACK throw then
   xb  xb OP-COUNT 1-  OP-AT TAILBR? 0= if E-A64RAV-DSTACK throw then
   VD-STAND @  r A64IR:SLOT-WIDTH *  <> if E-A64RAV-DSTACK throw then
   n 1 - VD-P !
   0 VD-ES !
   begin
      VD-P @ 0 > if xb VD-P @ 1- VDSTORE? else false then
   while
      VD-P @ 1- VD-P !
      VD-ES @ 1+ VD-ES !
   repeat
   0 VD-J !
   VD-ES @ 0 ?do
      outs r  xb  VD-P @ i +  VDSLOT-AT  VDSEQ-FIND
   loop ;

\ Which of the two exit runs this routine has - and the third answer, NEITHER,
\ for a routine every path of which traps.
: VDLEAVE-CK ( IR-ID:ir-fun-id n n A64EFF:placeseq -- )
   {: f:IR-ID:ir-fun-id rb:n r:n outs:A64EFF:placeseq :}
   rb NO-RET = if exit then
   V-TAIL @ 0<> if f rb r outs VDTAIL-CK exit then
   f rb r outs VDEXIT-CK ;

: VDTAIL-POS? ( n n -- bool )
   {: n:n at:n :}
   n 1 - PRO-N - {: q:n :}
   at q VD-ES @ - >= at q < and ;

: VDEXIT-POS? ( n n -- bool )
   {: n:n at:n :}
   V-TAIL @ 0<> if n at VDTAIL-POS? exit then
   n 2 - PRO-N - {: p:n :}
   at p = if true exit then
   at p VD-ES @ - >= at p < and ;

: VDPOS? ( n n n n -- bool )
   {: b:n at:n eb:n rb:n :}
   b eb = at VD-EL @ PRO-N + <= and if true exit then
   b rb = if rb VB-OPS at VDEXIT-POS? exit then
   false ;

\ ---- a call site, re-derived -------------------------------------------------
: DSTORE-RUN ( IR-ID:ir-block-id n -- n )
   {: bk:IR-ID:ir-block-id at:n :}
   bk OP-COUNT {: n:n :}
   -1 VD-PREV !
   0
   n at - 0 ?do
      bk at i + VDSTORE? 0= if leave then
      bk at i + VDSLOT-AT VD-S !
      VD-S @ VD-PREV @ <= if E-A64RAV-CALL throw then
      VD-S @ VD-PREV !
      drop i 1+
   loop ;

: DLOAD-RUN ( IR-ID:ir-block-id n -- n )
   {: bk:IR-ID:ir-block-id at:n :}
   bk OP-COUNT {: n:n :}
   -1 VD-PREV !
   0
   n at - 0 ?do
      bk at i + VDLOAD? 0= if leave then
      bk at i + VDSLOT-AT VD-S !
      VD-S @ VD-PREV @ <= if E-A64RAV-CALL throw then
      VD-S @ VD-PREV !
      drop i 1+
   loop ;

: VDRUN-BOUND ( IR-ID:ir-block-id n n n -- )
   {: bk:IR-ID:ir-block-id at:n k:n limit:n :}
   k 0 ?do
      bk at i + VDSLOT-AT limit >= if E-A64RAV-CALL throw then
   loop ;

: VDNET-CK ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id VCALL-ENTRY NOSLOT <> if exit then
   id DBACK-OF  id DBYTES-OF -  {: net:n :}
   net  VD-LEAVE @ VD-ENTRY @ -  <> if E-A64RAV-CALL throw then ;

: VDREQ+ ( n -- )
   {: at:n :}
   VD-REQ-N @ VDREQ-MAX >= if 1 VD-REQ-OVER ! exit then
   at  VD-REQ-N @ cells VD-REQ + !
   VD-REQ-N @ 1+ VD-REQ-N ! ;

\ A trap site is a call site with nothing to take back, and it is measured as one.
: VTRAP-SITE ( IR-ID:ir-block-id n n -- n )
   {: bk:IR-ID:ir-block-id at:n cp:n :}
   cp bk OP-COUNT 1- <> if E-A64RAV-CALL throw then
   bk cp OP-AT {: id:IR-ID:ir-op-id :}
   id DBACK-OF NOSLOT <> if E-A64RAV-CALL throw then
   id DBYTES-OF VD-STAND @ + VDREQ+
   bk at  cp at -  id DBYTES-OF VDCELL  VDRUN-BOUND
   cp 1+ ;

: VCALL-SITE ( IR-ID:ir-block-id n -- n )
   {: bk:IR-ID:ir-block-id at:n :}
   bk at DSTORE-RUN {: g:n :}
   at g + {: cp:n :}
   cp bk OP-COUNT >= if E-A64RAV-CALL throw then
   bk cp OP-AT TRAP-AT? if bk at cp VTRAP-SITE exit then
   bk cp OP-AT {: id:IR-ID:ir-op-id :}
   id DCALL? 0= if E-A64RAV-CALL throw then
   id VDNET-CK
   id DBYTES-OF VD-STAND @ + VDREQ+
   id DBACK-OF VD-STAND @ + VDREQ+
   bk at g  id DBYTES-OF VDCELL  VDRUN-BOUND
   bk cp 1+ DLOAD-RUN {: b:n :}
   bk cp 1+ b  id DBACK-OF VDCELL  VDRUN-BOUND
   cp 1+ b + ;

: VDCLEAN1 ( IR-ID:ir-fun-id n n n -- )
   {: f:IR-ID:ir-fun-id b:n eb:n rb:n :}
   f b BLOCK-AT {: bk:IR-ID:ir-block-id :}
   bk OP-COUNT {: n:n :}
   0 VD-AT !
   begin VD-AT @ n < while
      b VD-AT @ eb rb VDPOS? if
         VD-AT @ 1+ VD-AT !
      else
         bk VD-AT @ OP-AT DSTACK-TOUCH? if
            bk VD-AT @ VCALL-SITE VD-AT !
         else
            VD-AT @ 1+ VD-AT !
         then
      then
   repeat ;

\ ---- the placement, re-derived -----------------------------------------------
: VDPLACE-COST ( n -- n )
   {: c:n :}
   0
   VD-REQ-N @ 0 ?do
      i cells VD-REQ + @ c <> if 1+ then
   loop ;

: VDPLACE-OK? ( n -- bool )
   {: c:n :}
   c 0 >=  c A64EFF:SLOT-BACK <=  and ;

: VDPLACE-BETTER? ( n n -- bool )
   {: c:n k:n :}
   k VD-BCOST @ < if true exit then
   k VD-BCOST @ =  c VD-BEST @ <  and ;

: VDPLACE-TRY ( n -- )
   {: c:n :}
   c VDPLACE-OK? 0= if exit then
   c VDPLACE-COST {: k:n :}
   c k VDPLACE-BETTER? 0= if exit then
   c VD-BEST !
   k VD-BCOST ! ;

: VDPLACE-CK ( -- )
   VD-REQ-OVER @ 0<> if exit then
   0 VD-BEST !
   0 VDPLACE-COST VD-BCOST !
   VD-REQ-N @ 0 ?do  i cells VD-REQ + @ VDPLACE-TRY  loop
   VD-BEST @ VD-STAND @ <> if E-A64RAV-DSTACK throw then ;

\ Which of the two shapes this module has to have is the CONTRACT's declaration:
\ a register-convention routine touches the caller's stack nowhere.
: VDSTACK-CK ( IR-ID:ir-fun-id n A64EFF:placeseq A64EFF:placeseq -- )
   {: f:IR-ID:ir-fun-id rb:n args:A64EFF:placeseq outs:A64EFF:placeseq :}
   args A64EFF:SEQ-SLOTS {: a:n :}
   outs A64EFF:SEQ-SLOTS {: r:n :}
   V-DSTACK @ 0= if
      V-BLKS @ 0 ?do f i BLOCK-AT VNO-DSTACK loop
      exit
   then
   a A64IR:SLOT-WIDTH * VD-ENTRY !
   r A64IR:SLOT-WIDTH * VD-LEAVE !
   0 VD-REQ-N !
   0 VD-REQ-OVER !
   VD-ENTRY @ VDREQ+
   rb NO-RET <> if VD-LEAVE @ VDREQ+ then
   f a args VDENTRY-CK
   f rb r outs VDLEAVE-CK
   V-BLKS @ 0 ?do f i 0 rb VDCLEAN1 loop
   VDPLACE-CK
   f rb a r args outs VDRES-CK ;

\ ---- the contract's control and the module's terminator ----------------------
: VTAIL1 ( IR-ID:ir-fun-id n n -- )
   {: f:IR-ID:ir-fun-id b:n rb:n :}
   f b BLOCK-AT {: bk:IR-ID:ir-block-id :}
   bk OP-COUNT 0 ?do
      bk i OP-AT TAILBR? if
         V-TAIL @ 0= if E-A64RAV-SHAPE throw then
         b rb <> if E-A64RAV-SHAPE throw then
         i bk OP-COUNT 1- <> if E-A64RAV-SHAPE throw then
      then
   loop ;

\ A routine that never returns leaves through no callee either.
: VNO-TAIL-CK ( -- )
   V-TAIL @ 0<> if E-A64RAV-SHAPE throw then ;

: VTAIL-CK ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id rb:n :}
   V-BLKS @ 0 ?do f i rb VTAIL1 loop
   rb NO-RET = if VNO-TAIL-CK exit then
   f rb BLOCK-AT TERM-AT TAILBR? 0= if VNO-TAIL-CK exit then
   V-TAIL @ 0= if E-A64RAV-SHAPE throw then ;

\ ---- the whole re-derivation -------------------------------------------------
: VBLOCK-CKS ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   V-BLKS @ 0 ?do
      f i BLOCK-AT TIE-CK
      f i BLOCK-AT FLOW-CK
   loop ;

: VCLOB-BLOCK ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id b:n :}
   f b BLOCK-AT {: bk:IR-ID:ir-block-id :}
   bk OP-COUNT 0 ?do
      bk i OP-AT  b i VOP-POS  CLOB-AT
   loop ;

: VCLOB-CK ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   V-BLKS @ 0 ?do f i VCLOB-BLOCK loop ;

: VALUE-CKS ( -- )
   COVER-CK
   INTERVAL-CK
   CLASS-CK
   REGISTER-CK
   OVERLAP-CK ;

: VERIFY ( IR-ID:ir-fun-id n A64EFF:placeseq A64EFF:placeseq n n n -- )
   {: f:IR-ID:ir-fun-id rb:n args:A64EFF:placeseq outs:A64EFF:placeseq frame:n
      lo:n hi:n :}
   SLOTS-CLEAR
   f VANY-FRAME
   f lo hi ORDER-CK
   f VEDGE-CK
   f rb VTAIL-CK
   f rb frame VFRAME-CK
   f VBLOCK-CKS
   f 0 BLOCK-AT args ARG-CK
   rb NO-RET <> if f rb BLOCK-AT outs OUT-CK then
   f rb args outs VDSTACK-CK
   f VCLOB-CK ;

\ ---- what the acceptance is bound to -----------------------------------------
: STATE-CK ( -- )
   A64RA:SEALED? 0= if E-A64RAV-STATE throw then ;

: MODULE-CK ( IR-BUILD:module -- )
   IR-BUILD:FMODULE A64RA:MODULE@ IR-ID:MODULE-SAME?
   0= if E-A64RAV-MODULE throw then ;

: CONTRACT-CK ( A64EFF:gprs A64EFF:fprs -- )
   {: pool:A64EFF:gprs fpool:A64EFF:fprs :}
   pool A64RA:POOL A64EFF-GPRS:EQ 0= if E-A64RAV-CONTRACT throw then
   fpool A64RA:FPOOL A64EFF-FPRS:EQ 0= if E-A64RAV-CONTRACT throw then ;

: FRESH-CK ( -- )
   ST @ ST-ACCEPTED <> if E-A64RAV-STATE throw then
   STATE-CK
   A64RA:GEN A-GEN @ <> if E-A64RAV-STATE throw then ;

\ This binding is NOT spent, because the allocator and the lowering pass take
\ their own over the same module.
: BND-TAKE ( -- )
   BND-MODE @ BOUND-YES <> if E-A64RAV-STATE throw then ;

: BND-MODULE-CK ( IR-BUILD:module -- )
   IR-BUILD:FMODULE  0 BND-MOD @  IR-ID:MODULE-SAME?
   0= if E-A64RAV-MODULE throw then ;

: DIALECT-CK ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b  c b IR-BUILD:DIALECT@  A64IR:NAME IR-BUILD:SYMBOL-IS?
   0= if E-A64RAV-MODULE throw then
   c b IR-BUILD:SCHEMA-MAJOR@ A64IR:MAJOR <> if E-A64RAV-MODULE throw then
   c b IR-BUILD:SCHEMA-MINOR@ A64IR:MINOR <> if E-A64RAV-MODULE throw then ;

\ ---- which places one FUNCTION's boundary uses -------------------------------
: FUN-SLOTS ( n -- A64EFF:placeseq )
   {: k:n :}
   A64EFF:SEQ-NONE
   k 0 ?do i A64EFF:SEQ-WITH-SLOT loop ;

: DECL-CK ( n n n n -- )
   {: in:n out:n din:n dout:n :}
   in din <> if E-A64RAV-CONTRACT throw then
   out dout <> if E-A64RAV-CONTRACT throw then ;

: FUN-PLACES ( IR-ID:ir-fun-id n A64EFF:placeseq A64EFF:placeseq -- A64EFF:placeseq A64EFF:placeseq )
   {: f:IR-ID:ir-fun-id ord:n args:A64EFF:placeseq outs:A64EFF:placeseq :}
   V-DSTACK @ 0= if args outs exit then
   f NFROZEN:FUN-ARITY {: in:n out:n :}
   ord 0= if in out  args A64EFF:SEQ-LEN outs A64EFF:SEQ-LEN  DECL-CK then
   in FUN-SLOTS  out FUN-SLOTS ;

\ ---- the contract's control statement, against the module's own answer --------
\ The contract's `no-return` is a PERMISSION the module has to have earned: it
\ is what lets a routine that calls keep no return address at all.
: VNORET-CK ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   V-NORET @ 0= if exit then
   f RET-ORD NO-RET <> if E-A64RAV-SHAPE throw then ;

: WALK ( IR-BUILD:module A64EFF:gprs A64EFF:fprs A64EFF:placeseq A64EFF:placeseq n -- )
   {: m:IR-BUILD:module pool:A64EFF:gprs fpool:A64EFF:fprs
      args:A64EFF:placeseq outs:A64EFF:placeseq frame:n :}
   ST-NONE ST !
   BND-TAKE
   m BND-MODULE-CK
   STATE-CK
   m MODULE-CK
   pool 0 V-POOL !
   fpool 0 V-FPOOL !
   pool fpool CONTRACT-CK
   m VIEWS!
   VALS-N!
   TABLES-CLEAR
   FUNS-CK {: nf:n :}
   0
   nf 0 ?do
      dup i cells F-VB + !
      i FUN-AT over VMEASURE1
      drop V-AT @
   loop
   nf cells F-VB + !
   VALUE-CKS
   nf 0 ?do
      i FUN-AT {: f:IR-ID:ir-fun-id :}
      f  i cells F-VB + @  VLAYOUT
      f VLIVENESS
      i 0= if f VNORET-CK then
      f  f RET-ORD  f i args outs FUN-PLACES  frame
      i cells F-VB + @  i 1 + cells F-VB + @  VERIFY
   loop ;

public

\ ---- binding the dialect -----------------------------------------------------
\ The only moment a module can be asked its opcode and key identities, because
\ its symbols are its own ordinals.
: BIND-DIALECT ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b DIALECT-CK
   b IR-BUILD:MODULE@ 0 BND-MOD !
   c b A64IR:GPR-TYPE  0 BND-GPR !
   c b A64IR:FPR-TYPE  0 BND-FPR !
   c b A64IR:MEM-TYPE  0 BND-MEM !
   c b A64IR:KEY-SLOT   0 BND-SLOT !
   c b A64IR:KEY-FRAME  0 BND-FRAME !
   c b A64IR:KEY-DSLOT  0 BND-DSLOT !
   c b A64IR:KEY-DBYTES 0 BND-DBYTES !
   c b A64IR:KEY-DBACK  0 BND-DBACK !
   c b A64IR:KEY-ENTRY  0 BND-ENTRY !
   c b A64IR:KEY-TRAP-ENTRY 0 BND-TRAP !
   BOUND-YES BND-MODE ! ;

\ ---- the check ---------------------------------------------------------------
: ACCEPT ( IR-BUILD:module A64EFF:routine -- )
   A64EFF:VALIDATE A64EFF-ROUTINE:UNMAKE
   {: cv:A64EFF:conv gi:A64EFF:placeseq gr:A64EFF:placeseq gc:A64EFF:gprs
      fi:A64EFF:fprs fr:A64EFF:fprs fc:A64EFF:fprs
      z:A64EFF:nzcv l:A64EFF:link ct:A64EFF:control
      t:A64EFF:traits size:n delta:n :}
   cv gi gr gc fi fr fc z l ct t size delta A64EFF-ROUTINE:MAKE
   A64EFF:GPR-WRITABLE {: pool:A64EFF:gprs :}
   cv gi gr gc fi fr fc z l ct t size delta A64EFF-ROUTINE:MAKE
   A64EFF:FPR-WRITABLE {: fpool:A64EFF:fprs :}
   t l A64FRAME:LINK-KEPT? if 1 else 0 then V-LSAVE !
   ct A64EFF-CONTROL:TAIL-CALL A64EFF-CONTROL:EQ if 1 else 0 then V-TAIL !
   ct A64EFF-CONTROL:NO-RETURN A64EFF-CONTROL:EQ if 1 else 0 then V-NORET !
   cv A64EFF-CONV:DSTACK A64EFF-CONV:EQ if 1 else 0 then V-DSTACK !
   t l A64FRAME:SPILL-BASE V-BASE !
   pool fpool gi gr size WALK
   FUNS-CK 0 ?do
      i FUN-AT
      cv gi gr gc fi fr fc z l ct t size delta A64EFF-ROUTINE:MAKE
      SLOT-CK
   loop
   A64RA:GEN A-GEN !
   ST-ACCEPTED ST ! ;

: ACCEPTED? ( -- bool )
   ST @ ST-ACCEPTED = ;

\ The one checked answer in the chain. It stops answering the moment a later
\ allocation replaces the one that was accepted.
: REG@ ( n -- n )
   FRESH-CK
   dup 0 < over N-VALS @ >= or if E-A64RAV-COVER throw then
   dup REGGED? 0= if E-A64RAV-CLASS throw then
   A64RA:CLAIM@ ;

: FLOATING? ( n -- bool )
   FRESH-CK
   dup 0 < over N-VALS @ >= or if E-A64RAV-COVER throw then
   dup REGGED? 0= if E-A64RAV-CLASS throw then
   F-FPR IN-FILE? ;

\ ---- what the accepted allocation says this routine destroys ------------------
\ What the allocation assigned, which is what may be published: a register the
\ emission does not happen to write today is still one it could name tomorrow.
: GPR-WRITTEN ( -- A64EFF:gprs )
   FRESH-CK
   0
   N-VALS @ 0 ?do
      i F-GPR IN-FILE? if
         1  i A64RA:CLAIM@  lshift or
      then
   loop
   A64EFF:GPR-SET ;

: FPR-WRITTEN ( -- A64EFF:fprs )
   FRESH-CK
   0
   N-VALS @ 0 ?do
      i F-FPR IN-FILE? if
         1  i A64RA:CLAIM@  lshift or
      then
   loop
   A64EFF:FPR-SET ;

: REGISTERED? ( n -- bool )
   FRESH-CK
   dup 0 < over N-VALS @ >= or if E-A64RAV-COVER throw then
   REGGED? ;

\ ---- reading back what the last residency run was about ----------------------
: DKEEP-HELD? ( -- bool )            LAST-HELD @ 0<> ;
: DKEEP-FUN ( -- IR-ID:ir-fun-id )   0 LAST-FUN @ ;
: DKEEP-OP ( -- IR-ID:ir-op-id )     0 LAST-OP @ ;
: DKEEP-CLAUSE ( -- n )              LAST-CLAUSE @ ;

: DKEEP-CALL? ( IR-ID:ir-op-id -- bool )    DCALL? ;
: DKEEP-STORES? ( IR-ID:ir-op-id -- bool )  STORES? ;
: DKEEP-DSLOT ( IR-ID:ir-op-id -- n )       DSLOT-OF ;
: DKEEP-USES ( IR-ID:ir-value-id -- n )     SLOT USES-AT ;

: DKEEP-CLAUSE$ ( n -- ptr u8 n )
   {: c:n :}
   c DKEEP-NAMED = if s" load into a slot already holding a named value" exit then
   c DKEEP-DEAD = if s" load whose result nothing reads" exit then
   c DKEEP-SAME = if s" store of the value the slot already holds" exit then
   s" none" ;

private
get-current prot-wid-add

public
get-current prot-wid-add

;using
;package
