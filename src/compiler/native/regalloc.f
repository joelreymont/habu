\ regalloc.f - give every virtual register of one machine module a real ARM64
\ general register, by linear scan over the blocks of every function it holds.
\
\ A register number names a register of ONE file - d0 and x0 are two registers
\ and both are number zero - so every register question is asked of the FILE the
\ value's class belongs to.
\
\ Positions run END TO END across the module's functions on one number line, so
\ a class belongs to the function whose window its definition falls in.
\
\ Functions share the frame-size contract, with a separate frame per invocation.

require lib/prelude.f
require lib/errors.f
require src/compiler/target.f
require src/compiler/binding.f
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

package A64RA
using NFROZEN
private

\ Each pass retains its own dimensions while later passes read its results.
variable SCRATCH-VALUES
variable SCRATCH-BLOCKS
variable SCRATCH-FUNS
variable SCRATCH-OPS
: VMAX ( -- n ) SCRATCH-VALUES @ ;
: BMAX ( -- n ) SCRATCH-BLOCKS @ ;
: FMAX ( -- n ) SCRATCH-FUNS @ ;
: OMAX ( -- n ) SCRATCH-OPS @ ;
: SCRATCH-SIZES! ( -- )
   NFROZEN:VALUE-COUNT 1 max SCRATCH-VALUES !
   NFROZEN:TOTAL-BLOCKS 1 max SCRATCH-BLOCKS !
   NFROZEN:TOTAL-FUNS 1 max SCRATCH-FUNS !
   NFROZEN:TOTAL-OPS 1 max SCRATCH-OPS ! ;

\ ---- the bound dialect -------------------------------------------------------
0 constant BOUND-NO
1 constant BOUND-YES

\ ---- how much of one routine this pass holds ---------------------------------

0 constant P-STORE
1 constant P-RELOAD
2 constant P-MOVE                    \ a returned value put where it has to leave
3 constant P-REMAT                   \ a value written again where it is read, instead of reloaded

0 constant C-GPR
1 constant C-TOKEN
2 constant C-FPR

\ Two files, separately numbered, so a register question is asked of one file.
2 constant FILES-N
0 constant F-GPR
1 constant F-FPR

\ An answer and not a refusal: "which file" is asked of every value, including
\ the memory token, which is held in no file at all.
-1 constant NOFILE

: FILE-OF ( n -- n )
   {: cls:n :}
   cls C-GPR = if F-GPR exit then
   cls C-FPR = if F-FPR exit then
   cls C-TOKEN = if NOFILE exit then
   E-A64RA-CLASS throw ;

-1 constant NOSLOT

A64EFF:FILE-SIZE constant REGS-N

-1 constant NOPOS

\ The three keys that say an operation reaches the CALLER's data stack.
3 constant DKEYS-N
0 constant DK-SLOT
1 constant DK-BYTES
2 constant DK-BACK

-1 constant NOBODY

-1 constant NOATTR

A64EFF:SEQ-LIMIT constant FIXED-MAX

\ One table of two planes, because everything that reads one reads the other.
2 constant DECLS-N
0 constant D-FIX
1 constant D-WANT

\ ---- allocation state --------------------------------------------------------
0 constant ST-EMPTY
1 constant ST-SEALED

here CELL 1- and CELL swap - CELL 1- and allot
variable BND-MODE
BOUND-NO BND-MODE !
variable ST
ST-EMPTY ST !
variable GEN-N
0 GEN-N !
variable ALLOC-NS-ACC                \ mono-ns spent inside ALLOCATE since the last reset
0 ALLOC-NS-ACC !
variable N-VALS
0 N-VALS !
variable N-PLAN
0 N-PLAN !
variable N-SLOTS
0 N-SLOTS !
variable BASE-N                      \ the first frame byte this walk may use
0 BASE-N !
variable SEEN-FRAME                  \ the one frame size the module already owns
NOATTR SEEN-FRAME !
variable ARGS-N
0 ARGS-N !
variable OUTS-N
0 OUTS-N !

1 TYPED-BUFFER BND-MOD IR-ID:ir-module-id
1 TYPED-BUFFER BND-TYP IR-ID:ir-type-id
1 TYPED-BUFFER BND-MEM IR-ID:ir-type-id
1 TYPED-BUFFER BND-FPR IR-ID:ir-type-id
1 TYPED-BUFFER BND-SLOT IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-FRAME IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-MOV IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-MOVZ IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-ADDR IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-SHIFT IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-ENTRY IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-TRAP IR-ID:ir-symbol-id
DKEYS-N TYPED-BUFFER BND-DKEY IR-ID:ir-symbol-id

1 TYPED-BUFFER S-MOD IR-ID:ir-module-id
1 TYPED-BUFFER S-POOL A64EFF:gprs
1 TYPED-BUFFER S-FPOOL A64EFF:fprs

DYNAMIC-BUFFER V-DEF-BUF n
: V-DEF ( -- ptr n ) 0 V-DEF-BUF ;
DYNAMIC-BUFFER V-LAST-BUF n
: V-LAST ( -- ptr n ) 0 V-LAST-BUF ;
DYNAMIC-BUFFER V-REG-BUF n
: V-REG ( -- ptr n ) 0 V-REG-BUF ;
DYNAMIC-BUFFER V-SET-BUF n
: V-SET ( -- ptr n ) 0 V-SET-BUF ;
DYNAMIC-BUFFER V-CLS-BUF n
: V-CLS ( -- ptr n ) 0 V-CLS-BUF ;
DYNAMIC-BUFFER V-SLOT-BUF n
: V-SLOT ( -- ptr n ) 0 V-SLOT-BUF ;
DYNAMIC-BUFFER V-REMAT-BUF n
: V-REMAT ( -- ptr n ) 0 V-REMAT-BUF ;
DYNAMIC-BUFFER V-DECL-BUF n
: V-DECL ( -- ptr n ) 0 V-DECL-BUF ;
create A-REG FIXED-MAX cells allot
create O-REG FIXED-MAX cells allot
create R-HOLD FILES-N REGS-N * cells allot
\ How many registers each file's pool holds, and how many of those are free.
\ Maintained where a holder changes, because the pressure questions are asked at
\ every instruction and the answer moves only when a register changes hands.
create R-POOL-N FILES-N cells allot
create R-FREE-N FILES-N cells allot
DYNAMIC-BUFFER PL-BLK-BUF n
: PL-BLK ( -- ptr n ) 0 PL-BLK-BUF ;
DYNAMIC-BUFFER PL-POS-BUF n
: PL-POS ( -- ptr n ) 0 PL-POS-BUF ;
DYNAMIC-BUFFER PL-KIND-BUF n
: PL-KIND ( -- ptr n ) 0 PL-KIND-BUF ;
DYNAMIC-BUFFER PL-VAL-BUF n
: PL-VAL ( -- ptr n ) 0 PL-VAL-BUF ;

: PLAN-ROOM ( n -- ) {: n:n :}
   n PL-BLK-BUF-RESERVE
   n PL-POS-BUF-RESERVE
   n PL-KIND-BUF-RESERVE
   n PL-VAL-BUF-RESERVE
   ;

\ ---- where each function sits on the module's one number line ----------------
\ Function ordinal to its first position; positions run end to end.
DYNAMIC-BUFFER F-BASE-BUF n
: F-BASE ( -- ptr n ) 0 F-BASE-BUF ;
DYNAMIC-BUFFER F-RET-BUF n
: F-RET ( -- ptr n ) 0 F-RET-BUF ;
variable F-LO                                \ the base of the function laid out now
variable N-FUNS                              \ how many functions the module holds
variable SHORT-FUN                           \ the function whose scan ran short

\ ---- the slots, read back ----------------------------------------------------
: POOL-BITS ( n -- n )
   {: fl:n :}
   fl F-FPR = if 0 S-FPOOL @ A64EFF:FPRS-N exit then
   fl F-GPR = if 0 S-POOL @ A64EFF:GPRS-N exit then
   E-A64RA-CLASS throw ;

: POOL-HAS? ( n n -- bool )
   {: fl:n r:n :}
   fl POOL-BITS 1 r lshift and 0<> ;

\ ---- the per-value tables ----------------------------------------------------
: SLOT ( IR-ID:ir-value-id -- n )
   IR-ID:VALUE-LOCAL
   dup 0 < over VMAX >= or if E-A64RA-CAP throw then ;

: DEF-AT ( n -- n )                  cells V-DEF + @ ;
: LAST-AT ( n -- n )                 cells V-LAST + @ ;
: REG-AT ( n -- n )                  cells V-REG + @ ;
: SET-AT ( n -- n )                  cells V-SET + @ ;
: CLS-AT ( n -- n )                  cells V-CLS + @ ;
: FILE-AT ( n -- n )                 CLS-AT FILE-OF ;
: SLOT-AT ( n -- n )                 cells V-SLOT + @ ;
: REMAT-AT ( n -- bool )             cells V-REMAT + @ 0<> ;

: DECL-IX ( n n -- n )               {: d:n k:n :} d VMAX * k + ;
: DECL-AT ( n n -- n )               DECL-IX cells V-DECL + @ ;
: DECL! ( n n n -- )                 {: v:n d:n k:n :} v d k DECL-IX cells V-DECL + ! ;

: DEF! ( n n -- )                    {: v:n k:n :} v k cells V-DEF + ! ;
: LAST! ( n n -- )                   {: v:n k:n :} v k cells V-LAST + ! ;
: REG! ( n n -- )                    {: v:n k:n :} v k cells V-REG + ! ;
: SET! ( n n -- )                    {: v:n k:n :} v k cells V-SET + ! ;
: CLS! ( n n -- )                    {: v:n k:n :} v k cells V-CLS + ! ;
: SLOT! ( n n -- )                   {: v:n k:n :} v k cells V-SLOT + ! ;
: REMAT! ( n n -- )                  {: v:n k:n :} v k cells V-REMAT + ! ;

\ A register is a file and a number, and the file is checked against the table's
\ own shape rather than trusted from the caller.
: RIX ( n n -- n )
   {: fl:n r:n :}
   fl 0 < fl FILES-N >= or if E-A64RA-CLASS throw then
   r 0 < r REGS-N >= or if E-A64RA-CLASS throw then
   fl REGS-N * r + ;

: HOLD-AT ( n n -- n )               RIX cells R-HOLD + @ ;

: POOL-N-AT ( n -- n )               cells R-POOL-N + @ ;

\ Per file, because a class that wants a floating register is not served by a
\ free general one.
: FREE-N-AT ( n -- n )               cells R-FREE-N + @ ;

: FREE-N+ ( n n -- )                 {: fl:n d:n :}
   fl FREE-N-AT d +  fl cells R-FREE-N + ! ;

\ Only a pool register is ever handed out, so only a pool register can move the
\ count, and it moves it by one in the direction the holder went.
: HOLD! ( n n n -- )
   {: v:n fl:n r:n :}
   fl r RIX {: ix:n :}
   fl r POOL-HAS? if
      ix cells R-HOLD + @ NOBODY = {: was-free:bool :}
      v NOBODY = if
         was-free 0= if fl 1 FREE-N+ then
      else
         was-free if fl -1 FREE-N+ then
      then
   then
   v ix cells R-HOLD + ! ;

: POOL-SIZE ( n -- n )
   {: fl:n :}
   0
   REGS-N 0 ?do fl i POOL-HAS? if 1+ then loop ;

\ The counts are restated after the table is cleared rather than carried through
\ it: what HOLD! reads on the way past is the turn before's holder.
: HOLDERS-CLEAR ( -- )
   FILES-N 0 ?do
      REGS-N 0 ?do NOBODY j i HOLD! loop
   loop
   FILES-N 0 ?do
      i POOL-SIZE {: n:n :}
      n i cells R-POOL-N + !
      n i cells R-FREE-N + !
   loop ;

: TABLES-CLEAR ( -- )
   VMAX 0 ?do
      0 i SET!
      NOPOS i DEF!
      NOPOS i LAST!
      NOBODY i REG!
      C-GPR i CLS!
      NOSLOT i SLOT!
      0 i REMAT!
      DECLS-N 0 ?do NOBODY i j DECL! loop
   loop
   HOLDERS-CLEAR
   0 N-PLAN !
   0 N-SLOTS ! ;

\ ---- the spill plan ----------------------------------------------------------
\ One row per insertion, in walk order. Blocks use module ordinals so plans
\ for different functions cannot name the same insertion site.
: PLAN+ ( n n n n -- )
   {: blk:n kind:n pos:n k:n :}
   N-PLAN @ {: j:n :}
   j 1+ PLAN-ROOM
   blk j cells PL-BLK + !
   pos j cells PL-POS + !
   kind j cells PL-KIND + !
   k j cells PL-VAL + !
   j 1+ N-PLAN ! ;

\ One row serves every read of one value by one operation.
: RELOADED? ( n n n -- bool )
   {: blk:n k:n pos:n :}
   false
   N-PLAN @ 0 ?do
      i cells PL-KIND + @ P-RELOAD =  i cells PL-KIND + @ P-REMAT = or
      i cells PL-BLK + @ blk = and
      i cells PL-POS + @ pos = and
      i cells PL-VAL + @ k = and
      if drop true leave then
   loop ;

\ ---- the register constraints this operation's form declares -----------------
\ The schema table of the module being allocated is the authority on which
\ register fields a form ties together.
: TIES-AT ( IR-ID:ir-op-id -- n )
   {: id:IR-ID:ir-op-id :}
   V-SCHR VW id OPCODE-AT IR-SCHEMA:FTIES ;

: TIE-RESULT-AT ( IR-ID:ir-op-id n -- n )
   {: id:IR-ID:ir-op-id i:n :}
   V-SCHP VW V-SCHR VW id OPCODE-AT i IR-SCHEMA:FTIE-RESULT@ ;

: TIE-OPERAND-AT ( IR-ID:ir-op-id n -- n )
   {: id:IR-ID:ir-op-id i:n :}
   V-SCHP VW V-SCHR VW id OPCODE-AT i IR-SCHEMA:FTIE-OPERAND@ ;

\ ---- what a call site destroys -----------------------------------------------
: ATTR-INT-OF ( IR-ID:ir-op-id IR-ID:ir-symbol-id -- n )
   {: id:IR-ID:ir-op-id want:IR-ID:ir-symbol-id :}
   NOATTR
   id ATTRS-OF 0 ?do
      id i ATTR-KEY-AT want SAME-SYM? if
         drop
         id i ATTR-INT-AT
         leave
      then
   loop ;


\ Address carriers are indivisible through spill insertion: only their final
\ lane holds the pointer that a frame slot may retain.
: ADDRESS-HALF ( IR-ID:ir-op-id -- n )
   {: id:IR-ID:ir-op-id :}
   id 0 BND-ADDR @ ATTR-INT-OF A64IR:ADDR-NONE <= if -1 exit then
   id 0 BND-SHIFT @ ATTR-INT-OF A64IR:HALF-BITS / ;


: DSTORE? ( IR-ID:ir-op-id -- bool )
   {: id:IR-ID:ir-op-id :}
   id DK-SLOT BND-DKEY @ ATTR-INT-OF NOATTR = if false exit then
   V-SCHR VW id OPCODE-AT IR-SCHEMA:FEFFECT@
   IR--SCHEMA-EFFECT:WRITE IR--SCHEMA-EFFECT:EQ ;

: DLOAD? ( IR-ID:ir-op-id -- bool )
   {: id:IR-ID:ir-op-id :}
   id DK-SLOT BND-DKEY @ ATTR-INT-OF NOATTR = if false exit then
   V-SCHR VW id OPCODE-AT IR-SCHEMA:FEFFECT@
   IR--SCHEMA-EFFECT:READ IR--SCHEMA-EFFECT:EQ ;

: FRAME-ATTR ( IR-ID:ir-op-id -- n )
   0 BND-FRAME @ ATTR-INT-OF ;

: FRAME-TOUCH? ( IR-ID:ir-op-id -- bool )
   {: id:IR-ID:ir-op-id :}
   id 0 BND-FRAME @ ATTR-INT-OF NOATTR <>
   id 0 BND-SLOT @ ATTR-INT-OF NOATTR <> or ;

: FRAME-SEEN+ ( n -- )
   {: frame:n :}
   frame NOATTR = if exit then
   SEEN-FRAME @ NOATTR = if frame SEEN-FRAME ! exit then
   frame SEEN-FRAME @ <> if E-A64RA-FRAME throw then ;

: BLOCK-FRAME ( IR-ID:ir-block-id -- )
   {: bk:IR-ID:ir-block-id :}
   bk OP-COUNT 0 ?do
      bk i OP-AT FRAME-ATTR FRAME-SEEN+
   loop ;

: FUN-FRAME ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   f BLOCK-COUNT 0 ?do
      f i BLOCK-AT BLOCK-FRAME
   loop ;

: MODULE-FRAME ( -- n )
   NOATTR SEEN-FRAME !
   FUN-COUNT 0 ?do
      MKEY i IR-ID:PACK-FUN FUN-FRAME
   loop
   SEEN-FRAME @ ;

: BASE! ( A64EFF:traits A64EFF:link n -- )
   {: traits:A64EFF:traits link:A64EFF:link size:n :}
   MODULE-FRAME {: frame:n :}
   frame NOATTR = if traits link A64FRAME:SPILL-BASE BASE-N ! exit then
   frame size <> if E-A64RA-FRAME throw then
   frame BASE-N ! ;

\ Asked by the attribute a call carries and no other operation does.
: CALL-AT? ( IR-ID:ir-op-id -- bool )
   DK-BACK BND-DKEY @ ATTR-INT-OF NOATTR <> ;

\ It names an address as a call does and carries no take-back count, because
\ control does not come back.
: TAILBR-AT? ( IR-ID:ir-op-id -- bool )
   {: id:IR-ID:ir-op-id :}
   id 0 BND-ENTRY @ ATTR-INT-OF NOATTR = if false exit then
   id CALL-AT? 0= ;

\ The trap form carries its target under a key of its own.
: TRAP-AT? ( IR-ID:ir-op-id -- bool )
   0 BND-TRAP @ ATTR-INT-OF NOATTR <> ;

: FORBIDDEN? ( n n -- bool )
   {: forbid:n r:n :}
   1 r lshift forbid and 0<> ;

\ ---- the two value classes this dialect has ----------------------------------
: CLASS-OF ( IR-ID:ir-value-id -- n )
   {: id:IR-ID:ir-value-id :}
   id VALUE-TYPE-AT {: t:IR-ID:ir-type-id :}
   t 0 BND-TYP @ SAME-TYPE? if C-GPR exit then
   t 0 BND-FPR @ SAME-TYPE? if C-FPR exit then
   t 0 BND-MEM @ SAME-TYPE? if C-TOKEN exit then
   E-A64RA-CLASS throw ;

\ The MODULE's count and not one function's, because every sweep is over them.
: VALS-N! ( -- )
   V-VALR VW IR-OP:FVALUES {: n:n :}
   n VMAX > if E-A64RA-CAP throw then
   n N-VALS ! ;

\ Every value the module holds has to be one the walk measured, or it has read
\ only part of the program it is allocating for.
: COVER-CK ( -- )
   N-VALS @ 0 ?do i SET-AT 0= if E-A64RA-SHAPE throw then loop ;

: FREE-REG ( n n -- n )
   {: fl:n forbid:n :}
   -1
   REGS-N 0 ?do
      fl i POOL-HAS?
      forbid i FORBIDDEN? 0= and
      fl i HOLD-AT NOBODY = and if drop i leave then
   loop ;

\ FREE-REG only answers one of the pool, so nothing hands out a register that is
\ not the routine's.
: TAKE ( n n -- )
   {: k:n r:n :}
   k FILE-AT {: fl:n :}
   fl r POOL-HAS? 0= if E-A64RA-POOL throw then
   r k REG!
   k fl r HOLD! ;

\ ---- the routine's own fixed registers ---------------------------------------

: REG-POSITIONS ( A64EFF:placeseq -- n )
   {: s:A64EFF:placeseq :}
   s A64EFF:SEQ-LEN {: len:n :}
   s A64EFF:SEQ-SLOTS {: sl:n :}
   sl 0= if len exit then
   sl len <> if E-A64RA-PLACE throw then
   0 ;

: FIXED! ( A64EFF:placeseq A64EFF:placeseq -- )
   {: args:A64EFF:placeseq outs:A64EFF:placeseq :}
   args REG-POSITIONS ARGS-N !
   outs REG-POSITIONS OUTS-N !
   ARGS-N @ 0 ?do args i A64EFF:SEQ-REG@  i cells A-REG + ! loop
   OUTS-N @ 0 ?do outs i A64EFF:SEQ-REG@  i cells O-REG + ! loop ;

\ A declared register the routine may not write is a contract that contradicts
\ itself for this allocation.
: FIXED-POOL-CK ( -- )
   ARGS-N @ 0 ?do
      F-GPR i cells A-REG + @ POOL-HAS? 0= if E-A64RA-FIXED throw then
   loop
   OUTS-N @ 0 ?do
      F-GPR i cells O-REG + @ POOL-HAS? 0= if E-A64RA-FIXED throw then
   loop ;

: FIXED-ARITY-CK ( IR-ID:ir-block-id IR-ID:ir-block-id -- )
   {: bk:IR-ID:ir-block-id rb:IR-ID:ir-block-id :}
   bk ARG-COUNT ARGS-N @ < if E-A64RA-FIXED throw then
   rb TERM-AT OPERANDS-OF OUTS-N @ < if E-A64RA-FIXED throw then ;

\ A side declared in data-stack slots is one the module no longer carries in
\ registers at all: the selector turned each place into a load or a store.
: LOWERED-CK ( IR-ID:ir-block-id IR-ID:ir-block-id A64EFF:conv -- )
   {: bk:IR-ID:ir-block-id rb:IR-ID:ir-block-id cv:A64EFF:conv :}
   cv A64EFF-CONV:DSTACK A64EFF-CONV:EQ 0= if exit then
   bk ARG-COUNT 0<> if E-A64RA-PLACE throw then
   rb TERM-AT TAILBR-AT? if exit then
   rb TERM-AT OPERANDS-OF 0<> if E-A64RA-PLACE throw then ;

\ ---- taking a register away --------------------------------------------------
\ Slots are handed out in order and never given back.
: FRAME-CEIL ( -- n )
   VMAX A64IR:SLOT-WIDTH *  A64EFF:FRAME-MAX min ;

: NEW-SLOT ( -- n )
   BASE-N @  N-SLOTS @ A64IR:SLOT-WIDTH *  + {: off:n :}
   off A64IR:SLOT-WIDTH + FRAME-CEIL > if E-A64RA-PRESSURE throw then
   N-SLOTS @ 1+ N-SLOTS !
   off ;

: DEPTH-WANT ( -- n )
   BASE-N @  N-SLOTS @ A64IR:SLOT-WIDTH *  + ;

: FRAME-WANT ( -- n )
   DEPTH-WANT A64EFF:FRAME-ROUND ;

\ ---- the linear order, and everything decided over it ------------------------

\ ---- the sets ----------------------------------------------------------------
\ Live ranges are bit sets over positions of one number line.
64 constant SET-BITS
: SETC ( -- n ) VMAX SET-BITS 1- + SET-BITS / ;
0 constant P-IN
1 constant P-OUT
2 constant P-USE
3 constant P-DEF
4 constant PLANES

$3FFFFFFF constant POS-INF

here CELL 1- and CELL swap - CELL 1- and allot
variable N-BLKS
0 N-BLKS !
variable B-BASE                      \ where this function's blocks start in the module
0 B-BASE !
variable MB-AT
0 MB-AT !
variable CHANGED
0 CHANGED !
variable SHORT-AT                    \ the position the scan ran short at, or -1
-1 SHORT-AT !
variable SHORT-FILE                  \ and which register file it ran short of
0 SHORT-FILE !
variable SHORT-ROOT                  \ the unplaced class, or NOBODY for pressure
NOBODY SHORT-ROOT !
variable RET-B                       \ the block control leaves the routine through
0 RET-B !

DYNAMIC-BUFFER B-ST-BUF n
: B-ST ( -- ptr n ) 0 B-ST-BUF ;
DYNAMIC-BUFFER B-EN-BUF n
: B-EN ( -- ptr n ) 0 B-EN-BUF ;
DYNAMIC-BUFFER L-SETS-BUF n
: L-SETS ( -- ptr n ) 0 L-SETS-BUF ;
DYNAMIC-BUFFER TMPSET-BUF n
: TMPSET ( -- ptr n ) 0 TMPSET-BUF ;
DYNAMIC-BUFFER UF-BUF n
: UF ( -- ptr n ) 0 UF-BUF ;
\ Each class owns a member list headed by its root. Union splices two lists;
\ queries then visit that class instead of searching every value in the module.
DYNAMIC-BUFFER UF-NEXT-BUF n
DYNAMIC-BUFFER UF-LAST-BUF n
: UF-NEXT@ ( n -- n ) UF-NEXT-BUF @ ;
DYNAMIC-BUFFER CL-LO-BUF n
: CL-LO ( -- ptr n ) 0 CL-LO-BUF ;
DYNAMIC-BUFFER CL-HI-BUF n
: CL-HI ( -- ptr n ) 0 CL-HI-BUF ;
DYNAMIC-BUFFER CL-SLOT-BUF n
: CL-SLOT ( -- ptr n ) 0 CL-SLOT-BUF ;
DYNAMIC-BUFFER CL-REMAT-BUF n
\ Only evicted roots need temporary registers for their stores and reloads.
\ The classes stay fixed throughout fitting; this list grows once per eviction.
DYNAMIC-BUFFER EVICTED-ROOTS n
variable N-EVICTED
: CL-REMAT ( -- ptr n ) 0 CL-REMAT-BUF ;
DYNAMIC-BUFFER CL-DEF-BUF n
: CL-DEF ( -- ptr n ) 0 CL-DEF-BUF ;
DYNAMIC-BUFFER CL-ANCH-BUF n
: CL-ANCH ( -- ptr n ) 0 CL-ANCH-BUF ;
DYNAMIC-BUFFER CL-SIZE-BUF n
: CL-SIZE ( -- ptr n ) 0 CL-SIZE-BUF ;
DYNAMIC-BUFFER CL-KEEP-BUF n
: CL-KEEP ( -- ptr n ) 0 CL-KEEP-BUF ;
DYNAMIC-BUFFER CL-FRAME-BUF n
: CL-FRAME ( -- ptr n ) 0 CL-FRAME-BUF ;
DYNAMIC-BUFFER CL-FIX-BUF n
: CL-FIX ( -- ptr n ) 0 CL-FIX-BUF ;
DYNAMIC-BUFFER CL-WANT-BUF n
: CL-WANT ( -- ptr n ) 0 CL-WANT-BUF ;
DYNAMIC-BUFFER ANCH-HEAD-BUF n
: ANCH-HEAD ( -- ptr n ) 0 ANCH-HEAD-BUF ;
DYNAMIC-BUFFER ANCH-NEXT-BUF n
: ANCH-NEXT ( -- ptr n ) 0 ANCH-NEXT-BUF ;
DYNAMIC-BUFFER CL-USE-START-BUF n
: CL-USE-START ( -- ptr n ) 0 CL-USE-START-BUF ;
DYNAMIC-BUFFER CL-USE-NEXT-BUF n
: CL-USE-NEXT ( -- ptr n ) 0 CL-USE-NEXT-BUF ;
DYNAMIC-BUFFER USE-POS-BUF n
: USE-POS ( -- ptr n ) 0 USE-POS-BUF ;
DYNAMIC-BUFFER READ-END-BUF n
: READ-END ( -- ptr n ) 0 READ-END-BUF ;
\ Class roots bucketed by the position their hull opens at: one list per
\ position, so the sweep asks a position which classes begin there instead of
\ asking every value in the module whether it is one of them.
DYNAMIC-BUFFER DUE-HEAD-BUF n
: DUE-HEAD ( -- ptr n ) 0 DUE-HEAD-BUF ;
DYNAMIC-BUFFER DUE-NEXT-BUF n
: DUE-NEXT ( -- ptr n ) 0 DUE-NEXT-BUF ;

: RESERVE-SCRATCH ( -- )
   SCRATCH-SIZES!
   VMAX V-DEF-BUF-RESERVE
   VMAX V-LAST-BUF-RESERVE
   VMAX V-REG-BUF-RESERVE
   VMAX V-SET-BUF-RESERVE
   VMAX V-CLS-BUF-RESERVE
   VMAX V-SLOT-BUF-RESERVE
   VMAX V-REMAT-BUF-RESERVE
   DECLS-N VMAX * V-DECL-BUF-RESERVE
   FMAX 1 + F-BASE-BUF-RESERVE
   FMAX F-RET-BUF-RESERVE
   BMAX B-ST-BUF-RESERVE
   BMAX B-EN-BUF-RESERVE
   PLANES BMAX * SETC * L-SETS-BUF-RESERVE
   SETC TMPSET-BUF-RESERVE
   VMAX UF-BUF-RESERVE
   VMAX UF-NEXT-BUF-RESERVE
   VMAX UF-LAST-BUF-RESERVE
   VMAX CL-LO-BUF-RESERVE
   VMAX CL-HI-BUF-RESERVE
   VMAX CL-SLOT-BUF-RESERVE
   VMAX CL-REMAT-BUF-RESERVE
   VMAX EVICTED-ROOTS-RESERVE
   VMAX CL-DEF-BUF-RESERVE
   VMAX CL-ANCH-BUF-RESERVE
   VMAX CL-SIZE-BUF-RESERVE
   VMAX CL-KEEP-BUF-RESERVE
   VMAX CL-FRAME-BUF-RESERVE
   VMAX CL-FIX-BUF-RESERVE
   VMAX CL-WANT-BUF-RESERVE
   OMAX ANCH-HEAD-BUF-RESERVE
   OMAX ANCH-NEXT-BUF-RESERVE
   VMAX 1+ CL-USE-START-BUF-RESERVE
   VMAX CL-USE-NEXT-BUF-RESERVE
   OMAX BMAX + READ-END-BUF-RESERVE
   OMAX BMAX + DUE-HEAD-BUF-RESERVE
   VMAX DUE-NEXT-BUF-RESERVE
   ;

: BIT-CELL ( n -- n )    SET-BITS / ;
: BIT-MASK ( n -- n )    SET-BITS mod 1 swap lshift ;

: LS-IX ( n n n -- n )
   {: pl:n b:n w:n :}
   pl BMAX * b + SETC * w + ;

: LS@ ( n n n -- n )     LS-IX cells L-SETS + @ ;

: LS! ( n n n n -- )
   {: val:n pl:n b:n w:n :}
   val  pl b w LS-IX cells L-SETS + ! ;

: LS-HAS? ( n n n -- bool )
   {: pl:n b:n v:n :}
   pl b v BIT-CELL LS@  v BIT-MASK and 0<> ;

: LS-SET ( n n n -- )
   {: pl:n b:n v:n :}
   pl b v BIT-CELL LS@  v BIT-MASK or  pl b v BIT-CELL LS! ;

: TMP-CLEAR ( -- )
   SETC 0 ?do 0 i cells TMPSET + ! loop ;

: TMP-HAS? ( n -- bool )
   {: v:n :}
   v BIT-CELL cells TMPSET + @  v BIT-MASK and 0<> ;

: TMP-SET ( n -- )
   {: v:n :}
   v BIT-CELL cells TMPSET + @  v BIT-MASK or
   v BIT-CELL cells TMPSET + ! ;

: SETS-CLEAR ( -- )
   PLANES BMAX * SETC * 0 ?do 0 i cells L-SETS + ! loop ;

\ ---- step one: the linear order ----------------------------------------------
\ A function's blocks run on from the base it is given, which is what makes one
\ number line out of several functions.
: B-BASE! ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   f 0 BLOCK-AT IR-ID:BLOCK-LOCAL B-BASE !
   f BLOCK-COUNT 0 ?do
      f i BLOCK-AT IR-ID:BLOCK-LOCAL  B-BASE @ -  i <>
      if E-A64RA-SHAPE throw then
   loop ;

: MB-LAY1 ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id b:n :}
   MB-AT @ b cells B-ST + !
   MB-AT @  f b BLOCK-AT OP-COUNT  + {: e:n :}
   e b cells B-EN + !
   e 1+ MB-AT ! ;

\ The base is passed in because this runs twice over each function.
: MB-LAYOUT ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id base:n :}
   f BLOCK-COUNT {: n:n :}
   n BMAX > if E-A64RA-CAP throw then
   n N-BLKS !
   f B-BASE!
   base MB-AT !
   base F-LO !
   n 0 ?do f i MB-LAY1 loop ;

: OP-POS ( n n -- n )
   {: b:n i:n :}
   b cells B-ST + @ 1+ i + ;

\ ---- step two: liveness ------------------------------------------------------
: MB-USE1 ( n IR-ID:ir-value-id -- )
   {: b:n id:IR-ID:ir-value-id :}
   id SLOT {: v:n :}
   v TMP-HAS? if exit then
   P-USE b v LS-SET ;

: MB-DEF1 ( n IR-ID:ir-value-id -- )
   {: b:n id:IR-ID:ir-value-id :}
   id SLOT {: v:n :}
   P-DEF b v LS-SET
   v TMP-SET ;

: MB-OP-UD ( n IR-ID:ir-op-id -- )
   {: b:n id:IR-ID:ir-op-id :}
   id OPERANDS-OF 0 ?do b  id i OPERAND-AT  MB-USE1 loop
   id RESULTS-OF 0 ?do  b  id i RESULT-AT   MB-DEF1 loop ;

: MB-BLOCK-UD ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id b:n :}
   f b BLOCK-AT {: bk:IR-ID:ir-block-id :}
   TMP-CLEAR
   bk ARG-COUNT 0 ?do b  bk i ARG-AT  MB-DEF1 loop
   bk OP-COUNT 0 ?do  b  bk i OP-AT   MB-OP-UD loop ;

\ A successor names a block of the MODULE and this pass indexes by a block of
\ the FUNCTION, so the function's base comes off it here.
: SUCC-ORD ( IR-ID:ir-op-id n -- n )
   SUCC-AT IR-ID:BLOCK-LOCAL  B-BASE @ -
   dup 0 < over N-BLKS @ >= or if E-A64RA-SHAPE throw then ;

: MB-OUT-ADD ( n n -- )
   {: b:n s:n :}
   SETC 0 ?do
      P-OUT b i LS@  P-IN s i LS@ or  P-OUT b i LS!
   loop ;

: MB-OUT ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id b:n :}
   SETC 0 ?do 0 P-OUT b i LS! loop
   f b BLOCK-AT TERM-AT {: t:IR-ID:ir-op-id :}
   t SUCCS-OF 0 ?do
      b  t i SUCC-ORD  MB-OUT-ADD
   loop ;

: MB-IN1 ( n n -- bool )
   {: b:n w:n :}
   P-USE b w LS@   P-OUT b w LS@  P-DEF b w LS@ invert and   or {: nv:n :}
   nv  P-IN b w LS@ = if false exit then
   nv P-IN b w LS!
   true ;

: MB-IN ( n -- bool )
   {: b:n :}
   false
   SETC 0 ?do b i MB-IN1 or loop ;

: MB-PASS1 ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id b:n :}
   f b MB-OUT
   b MB-IN if 1 CHANGED ! then ;

\ The sets only grow and there are finitely many values and blocks, so the
\ iteration terminates. Blocks are visited backwards because that is the order
\ information flows in.
: MB-LIVENESS ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   SETS-CLEAR
   N-BLKS @ 0 ?do f i MB-BLOCK-UD loop
   begin
      0 CHANGED !
      N-BLKS @ 0 ?do
         f  N-BLKS @ 1- i -  MB-PASS1
      loop
      CHANGED @ 0=
   until ;

\ ---- step three: the hull intervals ------------------------------------------
: MB-DEFINE ( IR-ID:ir-value-id n -- )
   {: id:IR-ID:ir-value-id pos:n :}
   id CLASS-OF {: cls:n :}
   id SLOT {: k:n :}
   k SET-AT 0<> if E-A64RA-SHAPE throw then
   1 k SET!
   cls k CLS!
   pos k DEF!
   pos k LAST! ;

: MB-USE ( IR-ID:ir-value-id n -- )
   {: id:IR-ID:ir-value-id pos:n :}
   id SLOT {: k:n :}
   k SET-AT 0= if E-A64RA-SHAPE throw then
   pos k LAST-AT max k LAST! ;

: MB-OP-RANGE ( IR-ID:ir-op-id n -- )
   {: id:IR-ID:ir-op-id pos:n :}
   id OPERANDS-OF 0 ?do id i OPERAND-AT pos MB-USE loop
   id RESULTS-OF 0 ?do  id i RESULT-AT  pos MB-DEFINE loop ;

: MB-BLOCK-RANGE ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id b:n :}
   f b BLOCK-AT {: bk:IR-ID:ir-block-id :}
   bk ARG-COUNT 0 ?do
      bk i ARG-AT  b cells B-ST + @  MB-DEFINE
   loop
   bk OP-COUNT 0 ?do
      bk i OP-AT  b i OP-POS  MB-OP-RANGE
   loop ;

\ Live at a block's ENTRY means the range reaches back to that entry; live-IN
\ alone does not reach its last operation.
: MB-EXTEND1 ( n n -- )
   {: b:n k:n :}
   P-IN b k LS-HAS? if
      b cells B-ST + @  k DEF-AT min  k DEF!
   then
   P-OUT b k LS-HAS? if
      b cells B-EN + @  k LAST-AT max  k LAST!
   then ;

: MB-EXTEND-V ( n -- )
   {: k:n :}
   N-BLKS @ 0 ?do i k MB-EXTEND1 loop ;

\ Asked only about the values THIS function defined, which the window says.
: MB-RANGES ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   N-BLKS @ 0 ?do f i MB-BLOCK-RANGE loop
   N-VALS @ 0 ?do
      i DEF-AT NOPOS <>  i DEF-AT F-LO @ >=  and if i MB-EXTEND-V then
   loop ;

\ ---- step four: the block-argument classes -----------------------------------
: UF-INIT ( -- )
   VMAX 0 ?do
      i i cells UF + !
      NOBODY i UF-NEXT-BUF !
      i i UF-LAST-BUF !
      POS-INF i cells CL-LO + !
      -1 i cells CL-HI + !
   loop ;

: UF-FIND ( n -- n )
   begin dup cells UF + @ over <> while
      cells UF + @
   repeat ;

\ Two values joined into one class have to be able to share one register, so
\ this asks their CLASSES and not their files.
: UF-UNION ( n n -- )
   {: a:n b:n :}
   a CLS-AT b CLS-AT <> if E-A64RA-FILE throw then
   a UF-FIND {: ra:n :}
   b UF-FIND {: rb:n :}
   ra rb = if exit then
   ra rb min {: head:n :}
   ra rb max {: tail:n :}
   tail head UF-LAST-BUF @ UF-NEXT-BUF !
   tail UF-LAST-BUF @ head UF-LAST-BUF !
   head tail cells UF + ! ;

: MB-EDGES-OF ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id b:n :}
   f b BLOCK-AT TERM-AT {: t:IR-ID:ir-op-id :}
   t SUCCS-OF 1 <> if exit then
   f  t 0 SUCC-ORD  BLOCK-AT {: sb:IR-ID:ir-block-id :}
   t OPERANDS-OF sb ARG-COUNT <> if E-A64RA-EDGE throw then
   t OPERANDS-OF 0 ?do
      t i OPERAND-AT SLOT  sb i ARG-AT SLOT  UF-UNION
   loop ;

\ The rule is the read-then-write boundary, stated once for the whole pass.
: OVERLAP? ( n n -- bool )
   {: a:n b:n :}
   a DEF-AT b DEF-AT = if true exit then
   a DEF-AT b DEF-AT < if
      a LAST-AT b DEF-AT > exit
   then
   b LAST-AT a DEF-AT > ;

\ ---- the registers the routine's own contract names --------------------------
\ A declared place is a fact about one VALUE - the argument the caller puts in a
\ register, or the result it takes back.
: MB-FIX! ( IR-ID:ir-block-id -- )
   {: bk:IR-ID:ir-block-id :}
   ARGS-N @ 0 ?do
      bk i ARG-AT SLOT {: k:n :}
      k CLS-AT C-TOKEN = if E-A64RA-FIXED throw then
      i cells A-REG + @  D-FIX k DECL!
   loop ;

: MB-WANT! ( IR-ID:ir-block-id -- )
   {: rb:IR-ID:ir-block-id :}
   rb TERM-AT {: id:IR-ID:ir-op-id :}
   OUTS-N @ 0 ?do
      id i OPERAND-AT SLOT {: k:n :}
      k CLS-AT C-TOKEN = if E-A64RA-FIXED throw then
      i cells O-REG + @  D-WANT k DECL!
   loop ;

\ Two members declared into two different registers cannot be one class.
: MB-ONE-DECL ( n n -- n )
   {: so-far:n x:n :}
   x NOBODY = if so-far exit then
   so-far NOBODY <> so-far x <> and if E-A64RA-FIXED throw then
   x ;

: MB-DECL-KIND ( n n -- n )
   {: r:n d:n :}
   NOBODY
   r begin dup 0 >= while
      {: member:n :}
      d member DECL-AT MB-ONE-DECL
      member UF-NEXT@
   repeat drop ;

: MB-DECL-OF ( n -- n )
   {: r:n :}
   r D-FIX MB-DECL-KIND {: f:n :}
   f NOBODY <> if f exit then
   r D-WANT MB-DECL-KIND ;

: MB-DECLS! ( -- )
   N-VALS @ 0 ?do
      i UF-FIND i = if
         i D-FIX MB-DECL-KIND   i cells CL-FIX + !
         i D-WANT MB-DECL-KIND  i cells CL-WANT + !
      then
   loop ;

\ ---- the same question, asked of two whole classes ---------------------------
: MB-MEETS? ( n n -- bool )
   {: a:n r:n :}
   r begin dup 0 >= while
      dup a swap OVERLAP? if drop true exit then
      UF-NEXT@
   repeat drop false ;

\ Asked member against member, which is the same question the class invariant is.
: MB-CLASH? ( n n -- bool )
   {: ra:n rb:n :}
   ra begin dup 0 >= while
      dup rb MB-MEETS? if drop true exit then
      UF-NEXT@
   repeat drop false ;

\ ---- the ties, which are must-share constraints too --------------------------
\ A form that names one register field for a result and an operand is a
\ must-share constraint the schema declares.
: MB-TIE1 ( IR-ID:ir-op-id n -- )
   {: id:IR-ID:ir-op-id i:n :}
   id  id i TIE-OPERAND-AT  OPERAND-AT SLOT {: s:n :}
   id  id i TIE-RESULT-AT   RESULT-AT  SLOT {: d:n :}
   s UF-FIND {: ra:n :}
   d UF-FIND {: rb:n :}
   ra rb = if exit then
   ra rb MB-CLASH? if E-A64RA-TIE throw then
   s d UF-UNION ;

: MB-TIES-OP ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id TIES-AT 0 ?do id i MB-TIE1 loop ;

: MB-TIES-BLOCK ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id b:n :}
   f b BLOCK-AT {: bk:IR-ID:ir-block-id :}
   bk OP-COUNT 0 ?do bk i OP-AT MB-TIES-OP loop ;

: MB-TIES ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   N-BLKS @ 0 ?do f i MB-TIES-BLOCK loop ;

\ ---- step five: coalescing the copies ----------------------------------------
: MB-COPY? ( IR-ID:ir-op-id -- bool )
   OPCODE-AT 0 BND-MOV @ SAME-SYM? ;

\ Ends already in one class need nothing; ends whose classes hold an interfering
\ pair keep their copy; ends the contract declares into two registers keep it too.
: MB-COALESCE1 ( n n -- )
   {: s:n d:n :}
   s UF-FIND {: ra:n :}
   d UF-FIND {: rb:n :}
   ra rb = if exit then
   ra MB-DECL-OF {: da:n :}
   rb MB-DECL-OF {: db:n :}
   da NOBODY <> db NOBODY <> and  da db <> and if exit then
   ra rb MB-CLASH? if exit then
   s d UF-UNION ;

: MB-COALESCE-OP ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id MB-COPY? 0= if exit then
   id 0 OPERAND-AT SLOT  id 0 RESULT-AT SLOT  MB-COALESCE1 ;

: MB-COALESCE-BLOCK ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id b:n :}
   f b BLOCK-AT {: bk:IR-ID:ir-block-id :}
   bk OP-COUNT 0 ?do bk i OP-AT MB-COALESCE-OP loop ;

: MB-COALESCE ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   N-BLKS @ 0 ?do f i MB-COALESCE-BLOCK loop ;

: MB-CLASS1 ( n -- )
   {: k:n :}
   k UF-FIND {: r:n :}
   k DEF-AT   r cells CL-LO + @ min  r cells CL-LO + !
   k LAST-AT  r cells CL-HI + @ max  r cells CL-HI + ! ;

\ Register-bearing members of one class cannot overlap. Memory orders have no
\ register: mutually exclusive paths can overlap in block-layout hulls while
\ joining the same order class. A64RAV checks their uses along actual CFG paths.
: MB-MEMBER-CK ( n n -- )
   {: a:n b:n :}
   a CLS-AT C-TOKEN = if exit then
   a UF-FIND b UF-FIND <> if exit then
   a b OVERLAP? if E-A64RA-EDGE throw then ;

: MB-CLASSES ( -- )
   N-VALS @ 0 ?do i MB-CLASS1 loop
   N-VALS @ 0 ?do
      i UF-NEXT@ begin dup 0 >= while
         i over MB-MEMBER-CK
         UF-NEXT@
      repeat drop
   loop ;

\ ---- which class may be put in the frame --------------------------------------
\ Which class goes in the frame when the pool runs short.
: KEEP! ( n -- )
   UF-FIND {: r:n :}
   1 r cells CL-KEEP + ! ;

: KEEP? ( n -- bool )
   cells CL-KEEP + @ 0<> ;

: FRAME-KEEP! ( n -- )
   UF-FIND cells CL-FRAME + 1 swap ! ;

: FRAME-KEPT? ( n -- bool )
   UF-FIND cells CL-FRAME + @ 0<> ;

\ ---- what "this class lost its register" means -------------------------------
\ A class the fit evicted and a class in a slot are two answers, not one.
: CL-EVICTED? ( n -- bool )
   {: r:n :}
   r cells CL-SLOT + @ NOSLOT <> if true exit then
   r cells CL-REMAT + @ 0<> ;

: MB-KIND-CLEAR ( -- )
   0 N-EVICTED !
   VMAX 0 ?do
      NOSLOT i cells CL-SLOT + !
      0 i cells CL-REMAT + !
      NOPOS i cells CL-DEF + !
      NOPOS i cells CL-ANCH + !
      0 i cells CL-SIZE + !
      0 i cells CL-KEEP + !
      0 i cells CL-FRAME + !
      NOBODY i cells CL-FIX + !
      NOBODY i cells CL-WANT + !
   loop ;

: MB-SIZES ( -- )
   N-VALS @ 0 ?do
      i UF-FIND {: r:n :}
      r cells CL-SIZE + @ 1+  r cells CL-SIZE + !
   loop ;

: MB-KEEP-OP ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id OPERANDS-OF 0 ?do
      id i OPERAND-AT SLOT dup KEEP! FRAME-KEEP!
   loop
   id RESULTS-OF 0 ?do
      id i RESULT-AT SLOT dup KEEP! FRAME-KEEP!
   loop ;

: MB-KEEP-BLOCK ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id b:n :}
   f b BLOCK-AT {: bk:IR-ID:ir-block-id :}
   b 0= if bk ARG-COUNT 0 ?do bk i ARG-AT SLOT KEEP! loop then
   bk OP-COUNT 0 ?do
      bk i OP-AT {: id:IR-ID:ir-op-id :}
      id FRAME-TOUCH? if id MB-KEEP-OP then
   loop ;

\ Every function's positions lie end to end, so the line ends where the last
\ function's window does.
: LINE-N ( -- n )                    N-FUNS @ cells F-BASE + @ ;

\ ---- reading the linear order backwards --------------------------------------
: POS-BLOCK ( n -- n )
   {: p:n :}
   -1
   N-BLKS @ 0 ?do
      p i cells B-ST + @ >=  p i cells B-EN + @ <=  and if drop i leave then
   loop
   dup 0 < if E-A64RA-SHAPE throw then ;

: POS-OP? ( n -- bool )
   {: p:n :}
   p POS-BLOCK cells B-ST + @ p <> ;

: POS-OP ( IR-ID:ir-fun-id n -- IR-ID:ir-op-id )
   {: f:IR-ID:ir-fun-id p:n :}
   p POS-BLOCK {: b:n :}
   f b BLOCK-AT  p  b cells B-ST + @ -  1-  OP-AT ;

: MB-SPILLABLE? ( IR-ID:ir-fun-id n -- bool )
   nip {: r:n :}
   r CL-EVICTED? if false exit then
   r KEEP? if false exit then
   r CLS-AT C-TOKEN = if false exit then
   true ;

: MB-INCOMING-SPILLABLE? ( IR-ID:ir-fun-id n -- bool )
   nip {: r:n :}
   r CL-EVICTED? if false exit then
   r FRAME-KEPT? if false exit then
   r CLS-AT C-TOKEN = if false exit then
   true ;

\ Each class has a sorted slice of operand positions. A missing use answers
\ this function's end, as did the forward walk through its operations.
: MB-USE-FROM ( n n -- n )
   {: r:n from:n :}
   r 1+ cells CL-USE-START + @ {: end:n :}
   r cells CL-USE-START + @ end
   begin 2dup < while
      {: lo:n hi:n :}
      lo hi + 2 / {: mid:n :}
      mid cells USE-POS + @ from < if mid 1+ hi else lo mid then
   repeat
   drop
   dup end = if drop MB-AT @ exit then
   cells USE-POS + @ MB-AT @ min ;

: MB-READS? ( IR-ID:ir-fun-id n n -- bool )
   {: f:IR-ID:ir-fun-id r:n p:n :}
   p POS-OP? 0= if false exit then
   r p MB-USE-FROM p = ;

: MB-DEFS? ( IR-ID:ir-fun-id n n -- bool )
   {: f:IR-ID:ir-fun-id r:n p:n :}
   p POS-OP? 0= if false exit then
   f p POS-OP {: id:IR-ID:ir-op-id :}
   false
   id RESULTS-OF 0 ?do
      id i RESULT-AT SLOT UF-FIND r = if drop true leave then
   loop ;

: MB-TOUCHES? ( IR-ID:ir-fun-id n n -- bool )
   {: f:IR-ID:ir-fun-id r:n p:n :}
   f r p MB-READS? if true exit then
   f r p MB-DEFS? ;

\ A class nothing reads again answers the position past the last one.
: MB-NEXT-USE ( IR-ID:ir-fun-id n n -- n )
   {: f:IR-ID:ir-fun-id r:n from:n :}
   r from 0 max MB-USE-FROM ;

\ Address carriers and data-stack load runs stay contiguous. Store their
\ results after the run; other definitions can be stored immediately.
: MB-ANCHOR ( IR-ID:ir-block-id n -- n )
   {: bk:IR-ID:ir-block-id at:n :}
   bk OP-COUNT {: n:n :}
   bk at OP-AT ADDRESS-HALF {: half:n :}
   half 0 >= if at A64IR:HALVES + half - n min exit then
   bk at OP-AT DLOAD? 0= if at 1+ n min exit then
   n
   n at 1+ ?do
      bk i OP-AT DLOAD? 0= if drop i leave then
   loop ;

\ A call group begins with the stores that publish its arguments. Reloads may
\ stand before that run, but not inside it; adjacent data-stack groups are not
\ one indivisible run.
: MB-DSTORE-END ( IR-ID:ir-block-id n -- n )
   {: bk:IR-ID:ir-block-id at:n :}
   bk OP-COUNT {: n:n :}
   n
   n at 1+ ?do
      bk i OP-AT DSTORE? 0= if drop i leave then
   loop ;

: MB-DSTORE-HEAD? ( IR-ID:ir-block-id n -- bool )
   {: bk:IR-ID:ir-block-id at:n :}
   bk at OP-AT DSTORE? 0= if false exit then
   at 0= if true exit then
   bk at 1- OP-AT DSTORE? 0= ;

: MB-DEF-POS ( IR-ID:ir-fun-id n -- n )
   {: f:IR-ID:ir-fun-id r:n :}
   -1
   MB-AT @ F-LO @ ?do
      f r i MB-DEFS? if drop i leave then
   loop ;

: MB-ANCH-POS ( IR-ID:ir-fun-id n -- n )
   {: f:IR-ID:ir-fun-id p:n :}
   p POS-BLOCK {: b:n :}
   f b BLOCK-AT  p  b cells B-ST + @ -  1-  MB-ANCHOR {: k:n :}
   b k OP-POS ;

\ Reloads stand before one call's store run, so their temporary registers are
\ live only until their last consuming store in that group.
: MB-RUN-READS? ( IR-ID:ir-fun-id n n -- bool )
   {: f:IR-ID:ir-fun-id r:n p:n :}
   p POS-OP? 0= if false exit then
   r p MB-USE-FROM p cells READ-END + @ < ;

\ ---- the scan ----------------------------------------------------------------
: MB-EXPIRE1 ( n n n -- )
   {: fl:n r:n limit:n :}
   fl r HOLD-AT {: v:n :}
   v NOBODY = if exit then
   v cells CL-HI + @ limit < if NOBODY fl r HOLD! then ;

: MB-EXPIRE ( n -- )
   {: limit:n :}
   FILES-N 0 ?do
      i FREE-N-AT  i POOL-N-AT <> if
         REGS-N 0 ?do j i limit MB-EXPIRE1 loop
      then
   loop ;

\ ---- what a class already in the frame still costs in registers ---------------
\ A class in a frame slot has left the holder table and still needs a register
\ where it is read and where it is written.
: MB-ACROSS? ( n n -- bool )
   {: r:n p:n :}
   p  r cells CL-DEF + @  >   p  r cells CL-ANCH + @  <  and ;

: MB-WRITTEN? ( n n -- bool )
   {: r:n p:n :}
   p  r cells CL-DEF + @  = ;

: MB-FRAMED? ( n n -- bool )
   {: r:n fl:n :}
   r CL-EVICTED? 0= if false exit then
   r UF-FIND r =  r FILE-AT fl =  and ;

: MB-LOAD-N ( IR-ID:ir-fun-id n n -- n )
   {: f:IR-ID:ir-fun-id p:n fl:n :}
   0
   N-EVICTED @ 0 ?do
      i EVICTED-ROOTS @ {: r:n :}
      r fl MB-FRAMED? if
         r p MB-ACROSS?  f r p MB-RUN-READS? or if 1+ then
      then
   loop ;

: MB-STORE-N ( n n -- n )
   {: p:n fl:n :}
   0
   N-EVICTED @ 0 ?do
      i EVICTED-ROOTS @ {: r:n :}
      r fl MB-FRAMED? if
         r p MB-ACROSS?  r p MB-WRITTEN? or if 1+ then
      then
   loop ;

: MB-SHORT! ( n n -- )
   {: p:n fl:n :}
   SHORT-AT @ 0 < if
      p SHORT-AT !  fl SHORT-FILE !  NOBODY SHORT-ROOT !
   then ;

: MB-SHORT-ROOT! ( n n n -- )
   {: p:n fl:n r:n :}
   SHORT-AT @ 0 < if p SHORT-AT !  fl SHORT-FILE !  r SHORT-ROOT ! then ;

\ ---- which registers one class may not have ----------------------------------
: MB-CROSSES? ( n n -- bool )
   {: r:n p:n :}
   r begin dup 0 >= while
      dup DEF-AT p <  over LAST-AT p > and if drop true exit then
      UF-NEXT@
   repeat drop false ;

\ The positions it walks are THIS function's: a class belongs to one function,
\ and so do the calls that can destroy its register.
: MB-FORBID ( IR-ID:ir-fun-id n -- n )
   {: f:IR-ID:ir-fun-id r:n :}
   r FILE-AT {: fl:n :}
   \ No member can cross outside this class's hull. Gaps inside it still need
   \ MB-CROSSES?, because coalesced members need not cover every position.
   F-LO @ r cells CL-LO + @ 1+ max {: first:n :}
   MB-AT @ r cells CL-HI + @ min {: limit:n :}
   first limit >= if 0 exit then
   0
   limit first ?do
      i POS-OP? if
         f i POS-OP CALL-AT? if
            r i MB-CROSSES? if
               \ One crossing call forbids the whole file's writable pool.
               fl POOL-BITS or unloop exit
            then
         then
      then
   loop ;

: MB-DUE? ( n n -- bool )
   {: r:n pos:n :}
   r cells CL-LO + @ pos <> if false exit then
   r CLS-AT C-TOKEN = if false exit then
   r CL-EVICTED? 0= ;

\ A class the contract pins arrives in exactly that register, and three things
\ make that impossible rather than merely awkward.
: MB-PIN ( IR-ID:ir-fun-id n n -- )
   {: f:IR-ID:ir-fun-id r:n want:n :}
   r FILE-AT {: fl:n :}
   fl want POOL-HAS? 0= if E-A64RA-FIXED throw then
   f r MB-FORBID want FORBIDDEN? if E-A64RA-FIXED throw then
   fl want HOLD-AT NOBODY <> if E-A64RA-FIXED throw then
   r want TAKE ;

\ Given for nothing only when the register is free where the class is written.
: MB-WANTED ( IR-ID:ir-fun-id n n -- n )
   {: f:IR-ID:ir-fun-id r:n forbid:n :}
   r cells CL-WANT + @ {: want:n :}
   want NOBODY = if -1 exit then
   forbid want FORBIDDEN? if -1 exit then
   r FILE-AT want HOLD-AT {: held:n :}
   held NOBODY = if want exit then
   held cells CL-WANT + @ NOBODY <> if E-A64RA-FIXED throw then
   -1 ;

: MB-PLACE1 ( IR-ID:ir-fun-id n n -- )
   {: f:IR-ID:ir-fun-id r:n pos:n :}
   r pos MB-DUE? 0= if exit then
   r cells CL-FIX + @ {: fix:n :}
   fix NOBODY <> if f r fix MB-PIN exit then
   f r MB-FORBID {: forbid:n :}
   f r forbid MB-WANTED {: w:n :}
   w 0 >= if r w TAKE exit then
   r FILE-AT forbid FREE-REG {: g:n :}
   g 0 < if pos r FILE-AT r MB-SHORT-ROOT! exit then
   r g TAKE ;

: MB-READ-PRESSURE ( IR-ID:ir-fun-id n n -- )
   {: f:IR-ID:ir-fun-id pos:n fl:n :}
   f pos fl MB-LOAD-N  fl FREE-N-AT > if pos fl MB-SHORT! then ;

: MB-WRITE-PRESSURE ( n n -- )
   {: pos:n fl:n :}
   pos fl MB-STORE-N  fl FREE-N-AT > if pos fl MB-SHORT! then ;

: MB-READ-PRESSURE-ALL ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id pos:n :}
   FILES-N 0 ?do
      SHORT-AT @ 0 >= if leave then
      f pos i MB-READ-PRESSURE
   loop ;

: MB-WRITE-PRESSURE-ALL ( n -- )
   {: pos:n :}
   FILES-N 0 ?do
      SHORT-AT @ 0 >= if leave then
      pos i MB-WRITE-PRESSURE
   loop ;

\ Built once the classes are final and read by every turn of the fit: nothing
\ after coalescing changes which value is a root or where its hull opens.
\ Roots are prepended from the highest value number down, so each bucket is read
\ in ascending value order - the order the module-wide sweep this replaces
\ visited them in, and so the same tie-break.
: MB-DUE-INDEX ( -- )
   LINE-N 0 ?do NOBODY i cells DUE-HEAD + ! loop
   N-VALS @ 0 ?do
      N-VALS @ i - 1- {: k:n :}
      NOBODY k cells DUE-NEXT + !
      k UF-FIND k = if
         k cells CL-LO + @ {: p:n :}
         p cells DUE-HEAD + @  k cells DUE-NEXT + !
         k p cells DUE-HEAD + !
      then
   loop ;

\ Pinned classes first, because the entry block's arguments are all pinned.
: MB-PLACE-PINNED ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id pos:n :}
   pos cells DUE-HEAD + @
   begin dup 0 >= while
      {: r:n :}
      r cells CL-FIX + @ NOBODY <> if f r pos MB-PLACE1 then
      r cells DUE-NEXT + @
   repeat drop ;

: MB-PLACE-REST ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id pos:n :}
   pos cells DUE-HEAD + @
   begin dup 0 >= while
      {: r:n :}
      r cells CL-FIX + @ NOBODY = if f r pos MB-PLACE1 then
      r cells DUE-NEXT + @
   repeat drop ;

\ A class whose last read is HERE still holds its register while this operation
\ reads, so the reading instant is measured before the writing one.
: MB-STEP ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id pos:n :}
   pos MB-EXPIRE
   f pos MB-READ-PRESSURE-ALL
   SHORT-AT @ 0 >= if exit then
   pos 1+ MB-EXPIRE
   f pos MB-PLACE-PINNED
   f pos MB-PLACE-REST
   SHORT-AT @ 0 >= if exit then
   pos MB-WRITE-PRESSURE-ALL ;

\ The shortage cells and the holder table are NOT reset here: they belong to the
\ whole module's fit.
: MB-SCAN ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   MB-AT @ F-LO @ ?do
      SHORT-AT @ 0 < if f i MB-STEP then
   loop ;

\ ---- taking a class out of the registers -------------------------------------
: MB-HELD? ( n -- bool )
   {: r:n :}
   r FILE-AT {: fl:n :}
   false
   REGS-N 0 ?do fl i HOLD-AT r = if drop true leave then loop ;

\ ---- the class that can be WRITTEN AGAIN instead of put away -----------------
\ The one form of this dialect that costs the same to re-emit as to reload.
: MB-MOVZ? ( IR-ID:ir-op-id -- bool )
   OPCODE-AT 0 BND-MOVZ @ SAME-SYM? ;

: MB-DEF-OP? ( IR-ID:ir-fun-id n -- bool )
   {: f:IR-ID:ir-fun-id r:n :}
   r cells CL-LO + @ {: p:n :}
   p POS-OP? 0= if false exit then
   f p POS-OP MB-MOVZ? ;

\ Everything MB-SPILLABLE? asks except the one clause remat does not need.
: MB-REMATABLE? ( IR-ID:ir-fun-id n -- bool )
   {: f:IR-ID:ir-fun-id r:n :}
   r CL-EVICTED? if false exit then
   r cells CL-SIZE + @ 1 <> if false exit then
   r CLS-AT C-TOKEN = if false exit then
   f r MB-DEF-OP? ;

\ A class the operation here reads would need its register back immediately.
: MB-CANDIDATE? ( IR-ID:ir-fun-id n n -- bool )
   {: f:IR-ID:ir-fun-id r:n p:n :}
   r MB-HELD? 0= if false exit then
   f r p MB-TOUCHES? if false exit then
   f r MB-SPILLABLE? if true exit then
   f r MB-REMATABLE? ;

\ A position with no spare class is register pressure no spill can serve.
: MB-SPARE-N ( IR-ID:ir-fun-id n n -- n )
   {: f:IR-ID:ir-fun-id p:n fl:n :}
   0
   REGS-N 0 ?do
      fl i HOLD-AT {: r:n :}
      r NOBODY <> if
         f r p MB-TOUCHES? 0= if 1+ then
      then
   loop ;

: MB-FURTHEST ( IR-ID:ir-fun-id n n -- n )
   {: f:IR-ID:ir-fun-id p:n fl:n :}
   -1
   REGS-N 0 ?do
      fl i HOLD-AT {: r:n :}
      r NOBODY <> if
         f r p MB-CANDIDATE? if
            f r p 1+ MB-NEXT-USE {: c:n :}
            c over > if drop c then
         then
      then
   loop ;

\ Furthest next read, lowest register number breaking a tie, so one program
\ always spills the same way.
: MB-VICTIM ( IR-ID:ir-fun-id n n -- n )
   {: f:IR-ID:ir-fun-id p:n fl:n :}
   f p fl MB-FURTHEST {: want:n :}
   want 0 < if
      f p fl MB-SPARE-N 0= if E-A64RA-POOL throw then
      E-A64RA-SPILL throw
   then
   -1
   REGS-N 0 ?do
      fl i HOLD-AT {: r:n :}
      r NOBODY <> if
         f r p MB-CANDIDATE? if
            f r p 1+ MB-NEXT-USE want = if drop r leave then
         then
      then
   loop
   dup 0 < if E-A64RA-SPILL throw then ;

\ A class that can be written again is marked and given no slot.
: MB-EVICT1 ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id r:n :}
   f r MB-REMATABLE? if 1 r cells CL-REMAT + ! else NEW-SLOT r cells CL-SLOT + ! then
   f r MB-DEF-POS {: d:n :}
   d r cells CL-DEF + !
   f d MB-ANCH-POS  r cells CL-ANCH + !
   r N-EVICTED @ EVICTED-ROOTS !
   N-EVICTED @ 1+ N-EVICTED ! ;

: MB-EVICT ( IR-ID:ir-fun-id n n -- )
   {: f:IR-ID:ir-fun-id p:n fl:n :}
   SHORT-ROOT @ {: incoming:n :}
   incoming NOBODY <> if
      f incoming MB-INCOMING-SPILLABLE?  f incoming MB-REMATABLE?  or if
         f incoming MB-EVICT1 exit
      then
   then
   f  f p fl MB-VICTIM  MB-EVICT1 ;


: MB-FINISH ( -- )
   N-VALS @ 0 ?do
      i CLS-AT C-TOKEN = if
         NOBODY i REG!
      else
         i UF-FIND {: r:n :}
         r cells CL-SLOT + @ {: s:n :}
         s NOSLOT <> if
            NOBODY i REG!
            s i SLOT!
         else
            r cells CL-REMAT + @ 0<> if
               NOBODY i REG!
               1 i REMAT!
            else
               r REG-AT i REG!
            then
         then
      then
   loop ;

\ ---- the decisions, anchored to their blocks ---------------------------------
\ In front of the first operation after the one that defines the value.
: MB-PLAN-STORES ( IR-ID:ir-block-id n n n -- )
   {: bk:IR-ID:ir-block-id b:n at:n d:n :}
   bk d OP-AT {: id:IR-ID:ir-op-id :}
   id ADDRESS-HALF {: half:n :}
   half 0 >= half A64IR:HALVES 1- < and if exit then
   id RESULTS-OF 0 ?do
      id i RESULT-AT SLOT {: k:n :}
      k SLOT-AT NOSLOT <> if b P-STORE at k PLAN+ then
   loop ;

\ HOW it comes back is the eviction's own answer: out of its slot, or written again.
: MB-PLAN-LOADS1 ( IR-ID:ir-block-id n n n -- )
   {: bk:IR-ID:ir-block-id b:n at:n use:n :}
   bk use OP-AT {: id:IR-ID:ir-op-id :}
   id ADDRESS-HALF 0 > if exit then
   id SUCCS-OF 1 = if exit then
   id OPERANDS-OF 0 ?do
      id i OPERAND-AT SLOT {: k:n :}
      b k at RELOADED? 0= if
         k SLOT-AT NOSLOT <> if b P-RELOAD at k PLAN+ then
         k REMAT-AT if b P-REMAT at k PLAN+ then
      then
   loop ;

: MB-PLAN-LOADS ( IR-ID:ir-block-id n n -- )
   {: bk:IR-ID:ir-block-id b:n at:n :}
   bk at OP-AT DSTORE? 0= if bk b at at MB-PLAN-LOADS1 exit then
   bk at MB-DSTORE-HEAD? 0= if exit then
   bk at MB-DSTORE-END at ?do bk b at i MB-PLAN-LOADS1 loop ;

: MB-PLAN-TAIL-CK ( IR-ID:ir-block-id -- )
   {: bk:IR-ID:ir-block-id :}
   bk OP-COUNT {: n:n :}
   n 0 ?do
      bk i MB-ANCHOR n = if
         bk i OP-AT {: id:IR-ID:ir-op-id :}
         id RESULTS-OF 0 ?do
            id i RESULT-AT SLOT SLOT-AT NOSLOT <> if E-A64RA-SPILL throw then
         loop
      then
   loop ;

\ Decided after the fit, because the register a returned class is in is only
\ known then.
: MB-PLAN-MOVES ( IR-ID:ir-block-id -- )
   {: rb:IR-ID:ir-block-id :}
   rb TERM-AT {: id:IR-ID:ir-op-id :}
   rb OP-COUNT 1- {: at:n :}
   OUTS-N @ 0 ?do
      id i OPERAND-AT SLOT {: k:n :}
      k REG-AT  i cells O-REG + @  <> if
         rb IR-ID:BLOCK-LOCAL P-MOVE at k PLAN+
      then
   loop ;

\ Bucket producers once by the same anchors as MB-ANCHOR. Walking backwards
\ finds each data-load run's end once and prepends producers in their original
\ order; anchors themselves need not be monotonic.
: MB-PLAN-ANCHOR1 ( n IR-ID:ir-block-id n -- n )
   {: load-end:n bk:IR-ID:ir-block-id d:n :}
   bk OP-COUNT {: n:n :}
   bk d OP-AT {: id:IR-ID:ir-op-id :}
   id DLOAD? {: dload:bool :}
   id ADDRESS-HALF {: half:n :}
   half 0 >= if
      d A64IR:HALVES + half - n min
   else
      dload if load-end else d 1+ then
   then {: at:n :}
   \ Only later in-block anchors can receive stores; the tail is checked below.
   at d > at n < and if
      at cells ANCH-HEAD + @ d cells ANCH-NEXT + !
      d at cells ANCH-HEAD + !
   then
   dload if load-end else d then ;

: MB-PLAN-ANCHORS ( IR-ID:ir-block-id -- )
   {: bk:IR-ID:ir-block-id :}
   bk OP-COUNT {: n:n :}
   n 0 ?do NOPOS i cells ANCH-HEAD + ! loop
   n
   n 0 ?do bk n i - 1- MB-PLAN-ANCHOR1 loop
   drop ;

: MB-PLAN-BLOCK ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id b:n :}
   f b BLOCK-AT {: bk:IR-ID:ir-block-id :}
   bk MB-PLAN-ANCHORS
   bk OP-COUNT 0 ?do
      i cells ANCH-HEAD + @
      begin dup NOPOS <> while
         {: d:n :}
         bk bk IR-ID:BLOCK-LOCAL i d MB-PLAN-STORES
         d cells ANCH-NEXT + @
      repeat drop
      bk bk IR-ID:BLOCK-LOCAL i MB-PLAN-LOADS
   loop
   bk MB-PLAN-TAIL-CK
   b RET-B @ = if bk MB-PLAN-MOVES then ;

\ Blocks in the module's own order, operations in the block's.
: MB-PLAN ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   N-BLKS @ 0 ?do f i MB-PLAN-BLOCK loop ;

\ The block the routine's RESULTS leave through; a trap block is not that block.
-1 constant NO-RET

: MB-RET-ORD ( IR-ID:ir-fun-id -- n )
   {: f:IR-ID:ir-fun-id :}
   NO-RET
   f BLOCK-COUNT 0 ?do
      f i BLOCK-AT TERM-AT {: t:IR-ID:ir-op-id :}
      t SUCCS-OF 0=  t TRAP-AT? 0=  and if
         dup NO-RET <> if E-A64RA-SHAPE throw then
         drop i
      then
   loop ;

: MB-CONV-CK ( IR-ID:ir-fun-id n A64EFF:conv -- )
   {: f:IR-ID:ir-fun-id k:n cv:A64EFF:conv :}
   k cells F-RET + @ {: rb-ord:n :}
   rb-ord NO-RET = if exit then
   f 0 BLOCK-AT  f rb-ord BLOCK-AT  {: bk:IR-ID:ir-block-id rb:IR-ID:ir-block-id :}
   bk rb FIXED-ARITY-CK
   bk rb cv LOWERED-CK ;

: MB-MEASURE ( IR-ID:ir-fun-id n A64EFF:conv -- )
   {: f:IR-ID:ir-fun-id k:n cv:A64EFF:conv :}
   f k cv MB-CONV-CK
   f  k cells F-BASE + @  MB-LAYOUT
   f MB-LIVENESS
   f MB-RANGES
   f 0 BLOCK-AT MB-FIX!
   k cells F-RET + @ NO-RET <> if
      f  k cells F-RET + @  BLOCK-AT MB-WANT!
   then
   N-BLKS @ 0 ?do f i MB-EDGES-OF loop
   f MB-TIES
   f MB-COALESCE ;

\ ---- what one allocation run is told -----------------------------------------
: FUN-AT ( n -- IR-ID:ir-fun-id )
   {: k:n :}
   k 0 < k FUN-COUNT >= or if E-A64RA-SHAPE throw then
   MKEY k IR-ID:PACK-FUN ;

: FUNS-CK ( -- n )
   FUN-COUNT {: n:n :}
   n 1 < if E-A64RA-SHAPE throw then
   n FMAX > if E-A64RA-CAP throw then
   n ;

\ The block tables are rewritten for every function measured, so they are put
\ back before any per-function question is asked again.
: MB-RELAY ( n -- )
   {: k:n :}
   k FUN-AT  k cells F-BASE + @  MB-LAYOUT
   k cells F-RET + @ RET-B ! ;

\ Coalescing is complete before these slices are built. Count every operand,
\ including duplicates, then fill in module position order. Allocation retries
\ change spills and registers but never the classes or these read positions.
: MB-USE-COUNT-OP ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id OPERANDS-OF 0 ?do
      id i OPERAND-AT SLOT UF-FIND cells CL-USE-NEXT +
      dup @ 1+ swap !
   loop ;

: MB-USE-FILL-OP ( IR-ID:ir-op-id n -- )
   {: id:IR-ID:ir-op-id p:n :}
   id OPERANDS-OF 0 ?do
      id i OPERAND-AT SLOT UF-FIND {: r:n :}
      p r cells CL-USE-NEXT + @ cells USE-POS + !
      r cells CL-USE-NEXT + dup @ 1+ swap !
   loop ;

\ A normal operation reads only itself; a store reads through the end of its
\ contiguous store group. One-successor branches do not require reloads here.
: MB-READ-ENDS ( IR-ID:ir-block-id n -- )
   {: bk:IR-ID:ir-block-id b:n :}
   b bk OP-COUNT OP-POS
   bk OP-COUNT 0 ?do
      bk OP-COUNT i - 1- {: at:n :}
      b at OP-POS {: p:n :}
      bk at OP-AT {: id:IR-ID:ir-op-id :}
      id DSTORE? if
         id SUCCS-OF 1 = if p else dup then
         p cells READ-END + !
      else
         p id SUCCS-OF 1 <> if 1+ then p cells READ-END + !
         drop p
      then
   loop
   drop ;

: MB-USES ( -- )
   N-VALS @ 0 ?do 0 i cells CL-USE-NEXT + ! loop
   N-FUNS @ 0 ?do
      i FUN-AT {: f:IR-ID:ir-fun-id :}
      f BLOCK-COUNT 0 ?do
         f i BLOCK-AT {: bk:IR-ID:ir-block-id :}
         bk OP-COUNT 0 ?do bk i OP-AT MB-USE-COUNT-OP loop
      loop
   loop
   0
   N-VALS @ 0 ?do
      dup i cells CL-USE-START + !
      i cells CL-USE-NEXT + @ over i cells CL-USE-NEXT + ! +
   loop
   dup N-VALS @ cells CL-USE-START + !
   1 max USE-POS-BUF-RESERVE
   N-FUNS @ 0 ?do
      i MB-RELAY
      i FUN-AT {: f:IR-ID:ir-fun-id :}
      N-BLKS @ 0 ?do
         f i BLOCK-AT {: bk:IR-ID:ir-block-id :}
         bk i MB-READ-ENDS
         bk OP-COUNT 0 ?do bk i OP-AT j i OP-POS MB-USE-FILL-OP loop
      loop
   loop ;

\ ---- the three walks over the module's functions ------------------------------
\ Every function onto the line, in the module's own order.
: MEASURE-ALL ( A64EFF:conv -- )
   {: cv:A64EFF:conv :}
   0
   N-FUNS @ 0 ?do
      dup i cells F-BASE + !
      i FUN-AT {: f:IR-ID:ir-fun-id :}
      f MB-RET-ORD  i cells F-RET + !
      f i cv MB-MEASURE
      drop MB-AT @
   loop
   N-FUNS @ cells F-BASE + ! ;

: KEEP-ALL ( -- )
   N-FUNS @ 0 ?do
      i MB-RELAY
      i FUN-AT {: f:IR-ID:ir-fun-id :}
      N-BLKS @ 0 ?do f i MB-KEEP-BLOCK loop
   loop ;

: PLAN-ALL ( -- )
   N-FUNS @ 0 ?do
      i MB-RELAY
      i FUN-AT MB-PLAN
   loop ;

\ Putting a class away only ever frees registers, so a later turn runs with at
\ least as many free as the one before it.
: MB-FIT ( -- )
   begin
      -1 SHORT-AT !
      F-GPR SHORT-FILE !
      NOBODY SHORT-ROOT !
      HOLDERS-CLEAR
      N-FUNS @ 0 ?do
         SHORT-AT @ 0 < if
            i MB-RELAY
            i SHORT-FUN !
            i FUN-AT MB-SCAN
         then
      loop
      SHORT-AT @ 0 <
      dup 0= if
         drop
         SHORT-FUN @ FUN-AT  SHORT-AT @  SHORT-FILE @  MB-EVICT
         false
      then
   until ;

\ ---- the contract, read once -------------------------------------------------
\ A contract is a twelve-field value and a value of more than one cell cannot be
\ bound to a local, so it is unmade at entry.
: SLOTS-CK ( A64EFF:routine -- )
   A64EFF:VALIDATE A64EFF-ROUTINE:UNMAKE
   {: cv:A64EFF:conv gi:A64EFF:placeseq gr:A64EFF:placeseq gc:A64EFF:gprs
      fi:A64EFF:fprs fr:A64EFF:fprs fc:A64EFF:fprs
      z:A64EFF:nzcv l:A64EFF:link ct:A64EFF:control t:A64EFF:traits
      size:n delta:n :}
   FRAME-WANT {: want:n :}
   N-SLOTS @ 0 ?do
      BASE-N @  i A64IR:SLOT-WIDTH *  +  A64IR:SLOT-WIDTH
      cv gi gr gc fi fr fc z l ct t want delta A64EFF-ROUTINE:MAKE
      A64EFF:CHECK-SLOT
   loop ;

: TARGET-CK ( IR-CTX:ctx -- )
   IR-CTX:BINDING@ CBIND:VALIDATE CBIND:TARGET@ CTARGET:ARCH@
   CTARGET-ARCH:AARCH64 CTARGET-ARCH:EQ
   0= if E-A64RA-TARGET throw then ;

\ Taken whatever the outcome, so a refused allocation leaves none behind.
: BND-TAKE ( -- )
   BND-MODE @ {: have:n :}
   BOUND-NO BND-MODE !
   have BOUND-YES <> if E-A64RA-BIND throw then ;

: BND-MODULE-CK ( IR-BUILD:module -- )
   IR-BUILD:FMODULE  0 BND-MOD @  IR-ID:MODULE-SAME?
   0= if E-A64RA-MODULE throw then ;

: DIALECT-CK ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b  c b IR-BUILD:DIALECT@  A64IR:NAME IR-BUILD:SYMBOL-IS?
   0= if E-A64RA-MODULE throw then
   c b IR-BUILD:SCHEMA-MAJOR@ A64IR:MAJOR <> if E-A64RA-MODULE throw then
   c b IR-BUILD:SCHEMA-MINOR@ A64IR:MINOR <> if E-A64RA-MODULE throw then ;

: SEAL-CK ( -- )
   ST @ ST-SEALED <> if E-A64RA-STATE throw then ;

: ORD-CK ( n -- n )
   dup 0 < over N-VALS @ >= or if E-A64RA-CAP throw then ;

public

\ ---- binding the dialect -----------------------------------------------------
\ The only moment a module can be asked its identities, because its symbols and
\ types are its own ordinals.
: BIND-DIALECT ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   BND-MODE @ BOUND-YES = if E-A64RA-BIND throw then
   c b DIALECT-CK
   b IR-BUILD:MODULE@ 0 BND-MOD !
   c b A64IR:GPR-TYPE 0 BND-TYP !
   c b A64IR:FPR-TYPE 0 BND-FPR !
   c b A64IR:MEM-TYPE 0 BND-MEM !
   c b A64IR:KEY-SLOT 0 BND-SLOT !
   c b A64IR:KEY-FRAME 0 BND-FRAME !
   c b A64IR:KEY-DSLOT  DK-SLOT BND-DKEY !
   c b A64IR:KEY-DBYTES DK-BYTES BND-DKEY !
   c b A64IR:KEY-DBACK  DK-BACK BND-DKEY !
   c b A64IR:KEY-ENTRY  0 BND-ENTRY !
   c b A64IR:KEY-TRAP-ENTRY 0 BND-TRAP !
   c b A64IR-OPCODE:MOV A64IR:OPCODE 0 BND-MOV !
   c b A64IR-OPCODE:MOVZ A64IR:OPCODE 0 BND-MOVZ !
   c b A64IR:KEY-ADDR 0 BND-ADDR !
   c b A64IR:KEY-SHIFT 0 BND-SHIFT !
   BOUND-YES BND-MODE ! ;

: BOUND? ( -- bool )
   BND-MODE @ BOUND-YES = ;

: RELEASE ( -- )
   BND-TAKE ;

\ ---- the pass ----------------------------------------------------------------
: WALK ( IR-CTX:ctx IR-BUILD:module A64EFF:gprs A64EFF:fprs A64EFF:conv A64EFF:placeseq A64EFF:placeseq A64EFF:traits A64EFF:link n -- )
   {: c:IR-CTX:ctx m:IR-BUILD:module pool:A64EFF:gprs fpool:A64EFF:fprs
      cv:A64EFF:conv args:A64EFF:placeseq outs:A64EFF:placeseq
      traits:A64EFF:traits link:A64EFF:link size:n :}
   BND-TAKE
   ST-EMPTY ST !
   m BND-MODULE-CK
   c TARGET-CK
   pool 0 S-POOL !
   fpool 0 S-FPOOL !
   m VIEWS!
   RESERVE-SCRATCH
   m IR-BUILD:FMODULE 0 S-MOD !
   traits link size BASE!
   TABLES-CLEAR
   args outs FIXED!
   FIXED-POOL-CK
   VALS-N!
   UF-INIT
   FUNS-CK N-FUNS !
   cv MEASURE-ALL
   COVER-CK
   MB-CLASSES
   MB-USES
   MB-KIND-CLEAR
   MB-SIZES
   MB-DECLS!
   KEEP-ALL
   MB-DUE-INDEX
   MB-FIT
   MB-FINISH
   PLAN-ALL ;

\ The pass times itself: no caller can time a pass it does not enter, and a
\ counter outside the package would have to know bounds only this entry point
\ knows. A refused allocation throws past the accumulate and is not counted, so
\ what is read back is time spent on allocations that completed.
: ALLOCATE ( IR-CTX:ctx IR-BUILD:module A64EFF:routine -- )
   mono-ns {: t0:n :}
   A64EFF:VALIDATE A64EFF-ROUTINE:UNMAKE
   {: cv:A64EFF:conv gi:A64EFF:placeseq gr:A64EFF:placeseq gc:A64EFF:gprs
      fi:A64EFF:fprs fr:A64EFF:fprs fc:A64EFF:fprs
      z:A64EFF:nzcv l:A64EFF:link ct:A64EFF:control
      t:A64EFF:traits size:n delta:n :}
   cv gi gr gc fi fr fc z l ct t size delta A64EFF-ROUTINE:MAKE
   A64EFF:GPR-WRITABLE {: pool:A64EFF:gprs :}
   cv gi gr gc fi fr fc z l ct t size delta A64EFF-ROUTINE:MAKE
   A64EFF:FPR-WRITABLE {: fpool:A64EFF:fprs :}
   pool fpool cv gi gr t l size WALK
   cv gi gr gc fi fr fc z l ct t size delta A64EFF-ROUTINE:MAKE SLOTS-CK
   GEN-N @ 1+ GEN-N !
   ST-SEALED ST !
   mono-ns t0 -  ALLOC-NS-ACC @ +  ALLOC-NS-ACC ! ;

\ ---- the sealed allocation ---------------------------------------------------
: SEALED? ( -- bool )
   ST @ ST-SEALED = ;

: GEN ( -- n )
   SEAL-CK GEN-N @ ;

\ Nanoseconds spent inside ALLOCATE since the counter was zeroed. Not sealed
\ state: it answers across allocations, which is the only way to ask what a
\ whole compile spent here.
: ALLOC-NS ( -- n )
   ALLOC-NS-ACC @ ;

: ALLOC-NS-RESET ( -- )
   0 ALLOC-NS-ACC ! ;

: MODULE@ ( -- IR-ID:ir-module-id )
   SEAL-CK 0 S-MOD @ ;

: POOL ( -- A64EFF:gprs )
   SEAL-CK 0 S-POOL @ ;

: FPOOL ( -- A64EFF:fprs )
   SEAL-CK 0 S-FPOOL @ ;

\ What the prologue owns plus every slot handed out, rounded to the alignment.
: FRAME ( -- n )
   SEAL-CK FRAME-WANT ;

: FRAME-USED ( -- n )
   SEAL-CK DEPTH-WANT ;

: VALUES ( -- n )
   SEAL-CK N-VALS @ ;

: CLAIM@ ( n -- n )
   SEAL-CK ORD-CK REG-AT ;

: DEF@ ( n -- n )
   SEAL-CK ORD-CK DEF-AT ;

: LAST@ ( n -- n )
   SEAL-CK ORD-CK LAST-AT ;

\ ---- the spill decisions -----------------------------------------------------
: SPILLS ( -- n )
   SEAL-CK N-SLOTS @ ;

: SLOT@ ( n -- n )
   SEAL-CK ORD-CK SLOT-AT ;

: PLAN-N ( -- n )
   SEAL-CK N-PLAN @ ;

: PLAN-ORD-CK ( n -- n )
   dup 0 < over N-PLAN @ >= or if E-A64RA-CAP throw then ;

\ The module block ordinal, shared with the frozen IR rather than local to a function.
: PLAN-BLOCK@ ( n -- n )
   SEAL-CK PLAN-ORD-CK cells PL-BLK + @ ;

: PLAN-POS@ ( n -- n )
   SEAL-CK PLAN-ORD-CK cells PL-POS + @ ;

: PLAN-VALUE@ ( n -- n )
   SEAL-CK PLAN-ORD-CK cells PL-VAL + @ ;

: PLAN-STORE? ( n -- bool )
   SEAL-CK PLAN-ORD-CK cells PL-KIND + @ P-STORE = ;

\ A row that is a re-emission names no slot.
: PLAN-REMAT? ( n -- bool )
   SEAL-CK PLAN-ORD-CK cells PL-KIND + @ P-REMAT = ;

: PLAN-MOVE? ( n -- bool )
   SEAL-CK PLAN-ORD-CK cells PL-KIND + @ P-MOVE = ;

: MOVES ( -- n )
   SEAL-CK
   0
   N-PLAN @ 0 ?do
      i cells PL-KIND + @ P-MOVE = if 1+ then
   loop ;

\ Counted because a decision that needs no frame slot is invisible in SPILLS.
: REMATS ( -- n )
   SEAL-CK
   0
   N-PLAN @ 0 ?do
      i cells PL-KIND + @ P-REMAT = if 1+ then
   loop ;

public
: RELEASE-SCRATCH ( -- )
   V-DEF-BUF-RELEASE
   V-LAST-BUF-RELEASE
   V-REG-BUF-RELEASE
   V-SET-BUF-RELEASE
   V-CLS-BUF-RELEASE
   V-SLOT-BUF-RELEASE
   V-REMAT-BUF-RELEASE
   V-DECL-BUF-RELEASE
   PL-BLK-BUF-RELEASE
   PL-POS-BUF-RELEASE
   PL-KIND-BUF-RELEASE
   PL-VAL-BUF-RELEASE
   F-BASE-BUF-RELEASE
   F-RET-BUF-RELEASE
   B-ST-BUF-RELEASE
   B-EN-BUF-RELEASE
   L-SETS-BUF-RELEASE
   TMPSET-BUF-RELEASE
   UF-BUF-RELEASE
   UF-NEXT-BUF-RELEASE
   UF-LAST-BUF-RELEASE
   CL-LO-BUF-RELEASE
   CL-HI-BUF-RELEASE
   CL-SLOT-BUF-RELEASE
   CL-REMAT-BUF-RELEASE
   EVICTED-ROOTS-RELEASE
   0 N-EVICTED !
   CL-DEF-BUF-RELEASE
   CL-ANCH-BUF-RELEASE
   CL-SIZE-BUF-RELEASE
   CL-KEEP-BUF-RELEASE
   CL-FRAME-BUF-RELEASE
   CL-FIX-BUF-RELEASE
   CL-WANT-BUF-RELEASE
   ANCH-HEAD-BUF-RELEASE
   ANCH-NEXT-BUF-RELEASE
   CL-USE-START-BUF-RELEASE
   CL-USE-NEXT-BUF-RELEASE
   USE-POS-BUF-RELEASE
   READ-END-BUF-RELEASE
   DUE-HEAD-BUF-RELEASE
   DUE-NEXT-BUF-RELEASE
   0 SCRATCH-VALUES ! 0 SCRATCH-BLOCKS ! 0 SCRATCH-FUNS ! 0 SCRATCH-OPS ! ;

private
get-current prot-wid-add

public
get-current prot-wid-add

;using
;package
