\ native-regalloc.f - checked register-allocation tests.
\
\ Proves the contract of src/compiler/native/regalloc.f and its validator
\ src/compiler/native/regalloc-verify.f: a frozen A64IR module gets
\ one real general register per value, taken only from the set the routine's own
\ contract says it may destroy, with the move-wide overwrite's tied register
\ honoured; the assignment is then accepted only after the validator has
\ re-derived every live range from the module itself; and a program that cannot
\ be allocated at all, a contract that cannot hold it, a module the allocation
\ was not made from, and a claim nobody has checked are each refused by name.
\
\ WHAT THE SPILL FIXTURES MEASURE. A block that holds five literals before it
\ reads any of them does not fit in three registers, and the walk decides where
\ the two that do not fit go. Three things are asserted about that, each
\ falsifiable on its own: the plan - which values lose their register, in front of
\ which operation, and into which slot - because that is the cost rule and nothing
\ else measures it; the exact registers of the lowered module, because a lowering
\ that put a store or a load anywhere else moves them; and that the lowered module
\ needs no further spill, because the walk that planned it claimed the operations
\ it would contain. A cost rule that chose the nearest next use instead of the
\ furthest reddens the first two: the plan names other values, and the registers
\ move with them.
\
\ WHY THE FRAME REFUSALS ARE BUILT BY HAND. A slot outside the declared frame,
\ two values in one slot, a reload of a slot nothing wrote, and a frame that is
\ not the contract's are shapes the lowering pass never produces. They are built
\ straight into the machine dialect, one wrong thing each, so what is measured is
\ the validator's own judgement about the module in front of it and not the
\ allocator's agreement with itself.
\
\ WHAT THE FIXED-REGISTER FIXTURES MEASURE. A routine contract can say which
\ register each argument arrives in and each returned value leaves in, and three
\ separate things are owed. That the scan FOLLOWS it: the two-argument shape is
\ allocated once with nothing declared and once with its arguments declared into
\ registers the default scan would never have chosen, so every register in the
\ answer moves and a scan that handed a pinned register out to something else
\ would collide. That a value which cannot be pre-coloured is MOVED instead: a
\ routine returning an argument that has to leave somewhere else plans a copy,
\ the copy is lowered into an operation, and the second allocation puts its
\ result in the declared register. And that the VALIDATOR decides the same thing
\ for itself: three cases allocate under a contract that declares nothing and ask
\ for an acceptance under one that declares something the assignment does not
\ satisfy, over the same registers, so nothing but the declaration differs.
\
\ WHAT THE POSITIVE FIXTURES MEASURE. Each one asserts the exact register of
\ every value, not merely that some allocation succeeded. That is what makes the
\ shapes falsifiable: a scan that never released a dead value's register gives
\ `- ` three registers instead of two, a scan that released one too early gives
\ the three-argument shape two values in one register, and a chain that lost the
\ move-wide tie puts the overwrite somewhere its own source is not.
\
\ WHY THE MODULES COME THROUGH INSTRUCTION SELECTION. The subject is the pass,
\ and its input is what the selector really produces, so the arithmetic and
\ constant fixtures are elaborated the way test/compiler/native-select.f builds
\ them and then run through the real A64SEL:SELECT. The hostile fixtures are
\ built straight into the machine dialect instead, because they are shapes the
\ selector will never produce - a value of the wrong register class, a second
\ function, a tied operand whose kept value is read again - and the allocator
\ must still refuse them.
\
\ WHERE THE TIE COMES FROM, AND HOW THAT IS MEASURED. The allocator holds no
\ opcode identity for the tie: it reads each operation's own schema. Two fixtures
\ below define a seventh form into the dialect's table, identical apart from the
\ tie, and allocate the same program with each. The tied one puts the result back
\ into its operand's register and the untied one gives it the lowest free
\ register, so the two exact register lists differ only because the schemas do.
\
\ WHAT THE MULTI-BLOCK FIXTURES MEASURE. There is ONE allocation path and a
\ routine of one block is the case N=1 of it, so the fixtures above and the ones
\ below exercise the same words; what the multi-block ones add is the rules only
\ a routine that branches can state, each asserted on a module built to state it:
\ the linear order and its global positions, the backward liveness, the hull of a
\ value live across a loop, the class one register per argument-carrying edge, the
\ schema tie unioned into those classes, and the copies coalesced into them where
\ the class invariant survives it. Each fixture asserts the exact register of
\ every value and names, where it stands, the allocator edit that reddens it.
\
\ AND WHAT A ROUTINE THAT BRANCHES DOES WHEN IT DOES NOT FIT. It spills, by the
\ same rule a routine of one block does, and the two fixtures for it assert the
\ two halves separately: the plan - which value goes into the frame, and the
\ BLOCK as well as the position of the store and of the load, because a row that
\ named the position alone would put one of them in the wrong block - and the
\ lowered module, which needs no further spill and is accepted. A declared result
\ register is delivered here too, which is the capability that let the second
\ allocator be retired. The refusals left are the two shapes this pass will not
\ put in a frame, a routine with no frame to put anything in, the class the edge
\ rule cannot serve, and the one edge shape only the validator refuses.
\
\ ONE FIXTURE PER CONTEXT. A module holds about seventeen arenas and the live
\ arena registry holds sixty-four, so a case that builds a source module and a
\ machine module is already close to full. Every case therefore runs in its own
\ context, and a refusing case runs inside an enclosing one because an abandoned
\ context gives its registry slots back only when a live enclosing context leaves
\ normally.

require lib/test.f
require src/compiler/native/select.f
require src/compiler/native/regalloc-verify.f
require src/compiler/native/spill.f

package A64RA-TEST
private

\ ---- bindings ----------------------------------------------------------------
: WBND ( -- CBIND:binding )
   CTARGET-ARCH:AARCH64 CTARGET-ABI:AAPCS64-DARWIN CTARGET-ENDIAN:LITTLE
   CTARGET-PTR--WIDTH:BITS64
   CTARGET:F-BASE CTARGET:F-FP CTARGET:WITH CTARGET:CONTRACT
   CNUM-OVERFLOW:WRAP CNUM-FLOAT--MODEL:IEEE754 CNUM-CONTRACTION:FORBIDDEN
   CNUM-FAST--MATH:BIT-EXACT CNUM-COMPARE:IEEE754-UNORDERED CNUM:POLICY
   CBIND:BIND ;

\ The same numeric policy on a machine whose registers these are not.
: PBND ( -- CBIND:binding )
   CTARGET-ARCH:PTX CTARGET-ABI:PTX-KERNEL CTARGET-ENDIAN:LITTLE
   CTARGET-PTR--WIDTH:BITS64
   CTARGET:F-BASE CTARGET:F-FP CTARGET:WITH CTARGET:CONTRACT
   CNUM-OVERFLOW:WRAP CNUM-FLOAT--MODEL:IEEE754 CNUM-CONTRACTION:FORBIDDEN
   CNUM-FAST--MATH:BIT-EXACT CNUM-COMPARE:IEEE754-UNORDERED CNUM:POLICY
   CBIND:BIND ;

\ ---- routine contracts -------------------------------------------------------
\ The lowest `n` general registers, as a set the schema itself built: every
\ member went through A64EFF:GPR-REG, which is what refuses a register no routine
\ may hold state in.
: POOL-N ( n -- A64EFF:gprs )
   {: n:n :}
   A64EFF:GPR-NONE
   n 0 ?do i A64EFF:GPR-REG A64EFF:GPR-WITH loop ;

\ A leaf routine that computes in the given registers, returns to its caller,
\ keeps the link register, touches no flags, reserves no frame and calls nothing.
: LEAF ( A64EFF:gprs -- A64EFF:routine )
   {: pool:A64EFF:gprs :}
   A64EFF-CONV:REGISTER A64EFF:SEQ-NONE A64EFF:SEQ-NONE pool
   A64EFF:FPR-NONE A64EFF:FPR-NONE A64EFF:FPR-NONE
   A64EFF-NZCV:UNTOUCHED A64EFF-LINK:PRESERVED A64EFF-CONTROL:RETURNS
   A64EFF:TRAITS-NONE 0 0 A64EFF:ROUTINE ;

: LEAF-N ( n -- A64EFF:routine )
   POOL-N LEAF ;

\ The same leaf under the data-stack convention: its arguments arrive in slots
\ 0.. of the caller's stack and its results are left in slots 0.., which is what
\ gives the routine a data-stack pointer to place at all.
: SLOTS-N ( n -- A64EFF:placeseq )
   {: n:n :}
   A64EFF:SEQ-NONE
   n 0 ?do i A64EFF:SEQ-WITH-SLOT loop ;

: HABU-N ( n n n -- A64EFF:routine )
   {: n:n in:n out:n :}
   A64EFF-CONV:DSTACK in SLOTS-N  out SLOTS-N  n POOL-N
   A64EFF:FPR-NONE A64EFF:FPR-NONE A64EFF:FPR-NONE
   A64EFF-NZCV:UNTOUCHED A64EFF-LINK:PRESERVED A64EFF-CONTROL:RETURNS
   A64EFF:TRAITS-NONE 0 0 A64EFF:ROUTINE ;

\ The same leaf with a frame of its own: a routine that spills has to have
\ somewhere to spill to, and how deep that is, is the contract's declaration.
: LEAF-FRAMED ( n n -- A64EFF:routine )
   {: n:n size:n :}
   n POOL-N {: pool:A64EFF:gprs :}
   A64EFF-CONV:REGISTER A64EFF:SEQ-NONE A64EFF:SEQ-NONE pool
   A64EFF:FPR-NONE A64EFF:FPR-NONE A64EFF:FPR-NONE
   A64EFF-NZCV:UNTOUCHED A64EFF-LINK:PRESERVED A64EFF-CONTROL:RETURNS
   A64EFF:TRAITS-NONE size 0 A64EFF:ROUTINE ;

\ `n` registers starting at `base`. A pool that does not start at register zero
\ is what tells the allocatable set apart from the low registers: nothing may be
\ handed out because it happened to be free, only because the contract named it.
: POOL-FROM ( n n -- A64EFF:gprs )
   {: base:n n:n :}
   A64EFF:GPR-NONE
   n 0 ?do base i + A64EFF:GPR-REG A64EFF:GPR-WITH loop ;

: LEAF-FROM ( n n -- A64EFF:routine )
   POOL-FROM LEAF ;

\ ---- contracts that declare where their arguments and results live -----------
\ An ordered list of one, two or three registers, so a case can say which
\ register each position takes without spelling the packing out.
: SQ ( n -- A64EFF:placeseq )
   A64EFF:SEQ-NONE swap A64EFF:SEQ-WITH ;

: SQ2 ( n n -- A64EFF:placeseq )
   {: a:n b:n :}
   a SQ b A64EFF:SEQ-WITH ;

: SQ3 ( n n n -- A64EFF:placeseq )
   {: a:n b:n c:n :}
   a b SQ2 c A64EFF:SEQ-WITH ;

\ A leaf routine that declares its interface: the arguments arrive in the first
\ list, the returned values leave in the second, and the registers it may destroy
\ are the given pool less the ones a result leaves in - one register cannot be
\ both. The pool is passed rather than derived, so a case can hand it one that
\ does NOT hold a declared register and get the refusal that earns.
: LEAF-DECL ( A64EFF:gprs A64EFF:conv A64EFF:placeseq A64EFF:placeseq -- A64EFF:routine )
   {: pool:A64EFF:gprs cv:A64EFF:conv
      args:A64EFF:placeseq outs:A64EFF:placeseq :}
   cv args outs
   pool outs A64EFF:SEQ-SET A64EFF:GPR-WITHOUT
   A64EFF:FPR-NONE A64EFF:FPR-NONE A64EFF:FPR-NONE
   A64EFF-NZCV:UNTOUCHED A64EFF-LINK:PRESERVED A64EFF-CONTROL:RETURNS
   A64EFF:TRAITS-NONE 0 0 A64EFF:ROUTINE ;

\ The same with a frame, for a program that declares an interface AND spills.
: LEAF-DECL-FRAMED ( A64EFF:gprs A64EFF:conv A64EFF:placeseq A64EFF:placeseq -- A64EFF:routine )
   {: pool:A64EFF:gprs cv:A64EFF:conv
      args:A64EFF:placeseq outs:A64EFF:placeseq :}
   cv args outs
   pool outs A64EFF:SEQ-SET A64EFF:GPR-WITHOUT
   A64EFF:FPR-NONE A64EFF:FPR-NONE A64EFF:FPR-NONE
   A64EFF-NZCV:UNTOUCHED A64EFF-LINK:PRESERVED A64EFF-CONTROL:RETURNS
   A64EFF:TRAITS-NONE 16 0 A64EFF:ROUTINE ;

\ ---- the fixture's source text -----------------------------------------------
create TXT
   58 c, 32 c, 83 c, 81 c, 85 c, 65 c, 82 c, 69 c,            \ ": SQUARE"
   32 c, 100 c, 117 c, 112 c, 32 c, 42 c, 32 c, 59 c,         \ " dup * ;"
16 constant TXT-N

2 constant NAME-ST                   \ the defined name inside TXT
6 constant NAME-LN
0 constant OPEN-ST                   \ the opening `:`
1 constant OPEN-LN
9 constant BODY-ST                   \ the body word
3 constant BODY-LN
15 constant CLOSE-ST                 \ the closing `;`
1 constant CLOSE-LN

\ ---- the module a fixture builds into ----------------------------------------
1 TYPED-BUFFER W-CTX IR-CTX:ctx
1 TYPED-BUFFER W-BLD IR-BUILD:builder
1 TYPED-BUFFER W-SRC IR-ID:ir-source-id

: CC ( -- IR-CTX:ctx )               0 W-CTX @ ;
: BB ( -- IR-BUILD:builder )         0 W-BLD @ ;
: SS ( -- IR-ID:ir-source-id )       0 W-SRC @ ;

: SPN ( n n -- IR-SOURCE:span )
   {: st:n ln:n :}
   BB SS st ln IR-BUILD:ADD-SPAN ;

: CELLT ( -- IR-ID:ir-type-id )
   CC BB IR--TYPE-WIDTH:W64 IR--TYPE-SIGN:SIGNED IR-BUILD:INTERN-INT ;

: WORDT ( -- IR-ID:ir-type-id )
   CC BB IR--TYPE-WIDTH:W32 IR--TYPE-SIGN:SIGNED IR-BUILD:INTERN-INT ;

: SIGN ( n n -- IR-ID:ir-type-id )
   {: in:n out:n :}
   CELLT {: t:IR-ID:ir-type-id :}
   IR-TYPE:FN-BEGIN
   in 0 ?do t IR-TYPE:FN-PARAM loop
   out 0 ?do t IR-TYPE:FN-RESULT loop
   CC BB IR-BUILD:INTERN-CODE-REF ;

: OPEN-FUN ( ptr u8 n n n -- )
   {: p u:n in:n out:n :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   CC BB  CC BB p u IR-BUILD:INTERN-SYMBOL  IR-BUILD:BEGIN-FUN
   CC BB  in out SIGN  IR-BUILD:SET-SIGNATURE
   CC BB IR--FUN-LINKAGE:DEFINED IR-BUILD:SET-LINKAGE
   CC BB IR--FUN-VISIBILITY:EXPORTED IR-BUILD:SET-VISIBILITY
   CC BB IR--FUN-CONVENTION:HABU IR-BUILD:SET-CONVENTION
   CC BB  NAME-ST NAME-LN SPN  IR-BUILD:SET-FUN-SPAN
   CC BB IR-BUILD:BEGIN-BLOCK
   CC BB  OPEN-ST OPEN-LN SPN  IR-BUILD:SET-BLOCK-SPAN ;

: ARG+ ( -- IR-ID:ir-value-id )
   CC BB CELLT IR-BUILD:ADD-BLOCK-ARG ;

: CLOSE-FUN ( -- )
   CC BB IR-BUILD:END-BLOCK drop
   CC BB IR-BUILD:END-FUN drop ;

\ ---- source modules ----------------------------------------------------------
: HIR-MOD ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   IR-BUILD:PLAN-BEGIN
   IR-BUILD:PLAN-DEFAULT
   c HIR:NEW-BUILDER {: b:IR-BUILD:builder :}
   c b HIR:REGISTER
   c 0 W-CTX !
   b 0 W-BLD !
   c b TXT TXT-N IR-BUILD:ADD-SOURCE 0 W-SRC ! ;

: OPEN-OP ( HIR:opcode n n -- )
   {: o:HIR:opcode st:n ln:n :}
   CC BB  CC BB o HIR:OPCODE  IR-BUILD:BEGIN-OP
   CC BB  st ln SPN  IR-BUILD:SET-OP-SPAN ;

: CLOSE-VALUE ( -- IR-ID:ir-value-id )
   CC BB IR-BUILD:END-OP {: id:IR-ID:ir-op-id :}
   CC BB id 0 IR-BUILD:OP-RESULT@ ;

: BINOP ( HIR:opcode IR-ID:ir-value-id IR-ID:ir-value-id -- IR-ID:ir-value-id )
   {: o:HIR:opcode x:IR-ID:ir-value-id y:IR-ID:ir-value-id :}
   o BODY-ST BODY-LN OPEN-OP
   CC BB x IR-BUILD:ADD-OPERAND
   CC BB y IR-BUILD:ADD-OPERAND
   CC BB CELLT IR-BUILD:ADD-RESULT
   CLOSE-VALUE ;

: CONSTOP ( n -- IR-ID:ir-value-id )
   {: v:n :}
   HIR-OPCODE:CONST BODY-ST BODY-LN OPEN-OP
   CC BB CELLT IR-BUILD:ADD-RESULT
   CC BB  CC BB HIR:KEY-VALUE  CC BB v IR-BUILD:INTERN-INT-ATTR
   IR-BUILD:ADD-ATTR
   CLOSE-VALUE ;

: RET1 ( IR-ID:ir-value-id -- )
   {: v:IR-ID:ir-value-id :}
   HIR-OPCODE:RETURN CLOSE-ST CLOSE-LN OPEN-OP
   CC BB v IR-BUILD:ADD-OPERAND
   CC BB IR-BUILD:END-OP drop ;

\ `: SQUARE ( n -- n ) dup * ;`: one multiply over one argument twice.
: BUILD-SQUARE ( -- )
   s" SQUARE" 1 1 OPEN-FUN
   ARG+ {: a:IR-ID:ir-value-id :}
   HIR-OPCODE:MUL a a BINOP RET1
   CLOSE-FUN ;

\ `: DIFF ( n n -- n ) - ;`: two arguments, so which register each one takes is
\ visible.
: BUILD-DIFF ( -- )
   s" DIFF" 2 1 OPEN-FUN
   ARG+ {: x:IR-ID:ir-value-id :}
   ARG+ {: y:IR-ID:ir-value-id :}
   HIR-OPCODE:SUB x y BINOP RET1
   CLOSE-FUN ;

\ `: SUM3 ( a b c -- n ) + + ;`: three arguments live at once, then two adds, so
\ the scan has to hand registers back in the right order to fit in three.
: BUILD-SUM3 ( -- )
   s" SUM3" 3 1 OPEN-FUN
   ARG+ {: x:IR-ID:ir-value-id :}
   ARG+ {: y:IR-ID:ir-value-id :}
   ARG+ {: z:IR-ID:ir-value-id :}
   HIR-OPCODE:ADD x y BINOP {: t:IR-ID:ir-value-id :}
   HIR-OPCODE:ADD t z BINOP RET1
   CLOSE-FUN ;

\ `: REUSE ( a b -- n ) over + + ;`: the first argument is read again after the
\ first sum, so a scan that hands its register back one operation early puts two
\ live values in one register.
: BUILD-REUSE ( -- )
   s" REUSE" 2 1 OPEN-FUN
   ARG+ {: x:IR-ID:ir-value-id :}
   ARG+ {: y:IR-ID:ir-value-id :}
   HIR-OPCODE:ADD x y BINOP {: t:IR-ID:ir-value-id :}
   HIR-OPCODE:ADD x t BINOP RET1
   CLOSE-FUN ;

\ `: KEEP ( a b -- n ) drop ;`: the second argument is never read. It still
\ arrives in a register of its own, because the caller had to put it somewhere,
\ which is the one case where two values written at the same instant are not
\ visibly live at the same instant.
: BUILD-UNUSED ( -- )
   s" KEEP" 2 1 OPEN-FUN
   ARG+ {: x:IR-ID:ir-value-id :}
   ARG+ drop
   x RET1
   CLOSE-FUN ;

\ A literal across two halves: a move-wide, then an overwrite that keeps it.
: BUILD-WIDE ( -- )
   s" WIDE" 0 1 OPEN-FUN
   $1234000000005678 CONSTOP RET1
   CLOSE-FUN ;

\ ---- running selection and allocation ----------------------------------------
: A64-BUILDER ( -- IR-BUILD:builder )
   IR-BUILD:PLAN-BEGIN
   IR-BUILD:PLAN-DEFAULT
   CC A64IR:NEW-BUILDER ;

\ Bind the source dialect to the module being read and the machine dialect to the
\ module about to be written, then select. Both bindings are taken while their
\ module is still live, which is the only moment either dialect can be asked what
\ its own symbols are.
: SELECTED ( -- IR-BUILD:module )
   CC BB A64SEL:BIND-SOURCE
   CC BB IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   A64-BUILDER {: ab:IR-BUILD:builder :}
   CC ab A64RA:BIND-DIALECT
   CC ab A64RAV:BIND-DIALECT
   CC m ab TXT TXT-N  A64EFF:GPR-NONE LEAF  A64SEL:SELECT ;

\ Allocate the selected module for a leaf routine of `n` registers and have the
\ validator accept it. Every positive case goes through both, so no case reads a
\ claim the validator has not agreed with.
: ALLOCATED ( n -- IR-BUILD:module )
   {: n:n :}
   SELECTED {: m:IR-BUILD:module :}
   CC m n LEAF-N A64RA:ALLOCATE
   m n LEAF-N A64RAV:ACCEPT
   m ;

\ ---- the corpus shapes -------------------------------------------------------
: SQUARE-BODY ( IR-CTX:ctx -- n n n n bool )
   HIR-MOD
   BUILD-SQUARE
   4 ALLOCATED drop
   A64RA:VALUES
   0 A64RAV:REG@
   1 A64RAV:REG@
   1 A64RA:DEF@
   A64RAV:ACCEPTED? ;

: SQUARE-CASE ( -- )
   s" a multiply reuses its own argument's register for its result" T-LABEL
   WBND [: SQUARE-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE 1 T= 0 T= 0 T= 2 T= ;

: DIFF-BODY ( IR-CTX:ctx -- n n n n )
   HIR-MOD
   BUILD-DIFF
   4 ALLOCATED drop
   A64RA:VALUES
   0 A64RAV:REG@
   1 A64RAV:REG@
   2 A64RAV:REG@ ;

: DIFF-CASE ( -- )
   s" two arguments take two registers and the difference reuses the first" T-LABEL
   WBND [: DIFF-BODY ;] IR-CTX:WITH-CONTEXT
   0 T= 1 T= 0 T= 3 T= ;

: SUM3-BODY ( IR-CTX:ctx -- n n n n n n )
   HIR-MOD
   BUILD-SUM3
   4 ALLOCATED drop
   A64RA:VALUES
   0 A64RAV:REG@
   1 A64RAV:REG@
   2 A64RAV:REG@
   3 A64RAV:REG@
   4 A64RAV:REG@ ;

: SUM3-CASE ( -- )
   s" three live arguments take three registers and both sums reuse the first" T-LABEL
   WBND [: SUM3-BODY ;] IR-CTX:WITH-CONTEXT
   0 T= 0 T= 2 T= 1 T= 0 T= 5 T= ;

\ Three registers are exactly enough for the three-argument shape: the refusal
\ below is a real bound and not a conservative one.
: SUM3-TIGHT-BODY ( IR-CTX:ctx -- n n )
   HIR-MOD
   BUILD-SUM3
   3 ALLOCATED drop
   2 A64RAV:REG@
   4 A64RAV:REG@ ;

: SUM3-TIGHT-CASE ( -- )
   s" the three-argument shape fits in exactly three registers" T-LABEL
   WBND [: SUM3-TIGHT-BODY ;] IR-CTX:WITH-CONTEXT
   0 T= 2 T= ;

: WIDE-BODY ( IR-CTX:ctx -- n n n n n n )
   HIR-MOD
   BUILD-WIDE
   4 ALLOCATED drop
   A64RA:VALUES
   0 A64RAV:REG@
   1 A64RAV:REG@
   0 A64RA:DEF@
   0 A64RA:LAST@
   1 A64RA:DEF@ ;

: WIDE-CASE ( -- )
   s" a move-wide overwrite lands in the register it keeps" T-LABEL
   WBND [: WIDE-BODY ;] IR-CTX:WITH-CONTEXT
   2 T= 2 T= 1 T= 0 T= 0 T= 2 T= ;

: REUSE-BODY ( IR-CTX:ctx -- n n n n n )
   HIR-MOD
   BUILD-REUSE
   4 ALLOCATED drop
   A64RA:VALUES
   0 A64RAV:REG@
   1 A64RAV:REG@
   2 A64RAV:REG@
   3 A64RAV:REG@ ;

: REUSE-CASE ( -- )
   s" an argument read after the first sum keeps its register until then" T-LABEL
   WBND [: REUSE-BODY ;] IR-CTX:WITH-CONTEXT
   0 T= 1 T= 1 T= 0 T= 4 T= ;

\ The same shape allocated out of a pool that starts at register four: the
\ registers follow the contract, not the numbering.
: SUM3-HIGH-BODY ( IR-CTX:ctx -- n n n n n )
   HIR-MOD
   BUILD-SUM3
   SELECTED {: m:IR-BUILD:module :}
   CC m 4 3 LEAF-FROM A64RA:ALLOCATE
   m 4 3 LEAF-FROM A64RAV:ACCEPT
   0 A64RAV:REG@
   1 A64RAV:REG@
   2 A64RAV:REG@
   3 A64RAV:REG@
   4 A64RAV:REG@ ;

: SUM3-HIGH-CASE ( -- )
   s" a pool that starts above register zero is followed exactly" T-LABEL
   WBND [: SUM3-HIGH-BODY ;] IR-CTX:WITH-CONTEXT
   4 T= 4 T= 6 T= 5 T= 4 T= ;

: UNUSED-BODY ( IR-CTX:ctx -- n n n n n )
   HIR-MOD
   BUILD-UNUSED
   4 ALLOCATED drop
   A64RA:VALUES
   0 A64RAV:REG@
   1 A64RAV:REG@
   1 A64RA:DEF@
   1 A64RA:LAST@ ;

: UNUSED-CASE ( -- )
   s" an argument nobody reads still gets a register of its own" T-LABEL
   WBND [: UNUSED-BODY ;] IR-CTX:WITH-CONTEXT
   0 T= 0 T= 1 T= 0 T= 2 T= ;

\ ---- the registers a routine's contract declares -----------------------------
\ The whole point of the declaration is that the scan follows it instead of the
\ numbering. This shape's arguments would take x0 and x1 if nothing said
\ otherwise - the DIFF case above measures exactly that - so declaring them into
\ x2 and x0 moves every register in the answer, and the result declared into x1
\ moves too. What it proves is that the scan never hands a pinned register out
\ while its argument is live: x2 is the lowest free register at the subtraction
\ and it is not the one the difference lands in.
: DECL-DIFF-BODY ( IR-CTX:ctx -- n n n n n )
   HIR-MOD
   BUILD-DIFF
   SELECTED {: m:IR-BUILD:module :}
   CC m 4 POOL-N A64EFF-CONV:REGISTER 2 0 SQ2 1 SQ LEAF-DECL A64RA:ALLOCATE
   m 4 POOL-N A64EFF-CONV:REGISTER 2 0 SQ2 1 SQ LEAF-DECL A64RAV:ACCEPT
   A64RA:MOVES
   A64RA:VALUES
   0 A64RAV:REG@
   1 A64RAV:REG@
   2 A64RAV:REG@ ;

: DECL-DIFF-CASE ( -- )
   s" declared argument and result registers are the ones the scan uses" T-LABEL
   WBND [: DECL-DIFF-BODY ;] IR-CTX:WITH-CONTEXT
   1 T= 0 T= 2 T= 3 T= 0 T= ;

\ A pool that starts above register zero, with the convention still naming the
\ low ones: the arguments arrive where the caller puts them whatever the pool's
\ base is, and everything else is allocated out of the pool. This is the shape
\ the comparison harness runs, and it is what makes a pool base a decision about
\ scratch registers rather than about the interface.
: DECL-HIGH-BODY ( IR-CTX:ctx -- n n n n n n )
   HIR-MOD
   BUILD-SUM3
   SELECTED {: m:IR-BUILD:module :}
   CC m  4 4 POOL-FROM 0 1 2 SQ3 A64EFF:SEQ-SET A64EFF:GPR-WITH
      A64EFF-CONV:REGISTER 0 1 2 SQ3  0 SQ LEAF-DECL A64RA:ALLOCATE
   m  4 4 POOL-FROM 0 1 2 SQ3 A64EFF:SEQ-SET A64EFF:GPR-WITH
      A64EFF-CONV:REGISTER 0 1 2 SQ3  0 SQ LEAF-DECL A64RAV:ACCEPT
   A64RA:MOVES
   0 A64RAV:REG@
   1 A64RAV:REG@
   2 A64RAV:REG@
   3 A64RAV:REG@
   4 A64RAV:REG@ ;

: DECL-HIGH-CASE ( -- )
   s" a pool above register zero still delivers the declared registers" T-LABEL
   WBND [: DECL-HIGH-BODY ;] IR-CTX:WITH-CONTEXT
   0 T= 0 T= 2 T= 1 T= 0 T= 0 T= ;

\ ---- machine modules built by hand -------------------------------------------
\ The shapes the selector never produces. Everything below builds straight into
\ the machine dialect, so there is one module per context rather than two.
: A64-MOD ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   IR-BUILD:PLAN-BEGIN
   IR-BUILD:PLAN-DEFAULT
   c A64IR:NEW-BUILDER {: b:IR-BUILD:builder :}
   c 0 W-CTX !
   b 0 W-BLD !
   c b A64RA:BIND-DIALECT
   c b A64RAV:BIND-DIALECT
   c b A64IR:REGISTER
   c b TXT TXT-N IR-BUILD:ADD-SOURCE 0 W-SRC ! ;

: M-OPEN ( A64IR:opcode -- )
   {: o:A64IR:opcode :}
   CC BB  CC BB o A64IR:OPCODE  IR-BUILD:BEGIN-OP
   CC BB  BODY-ST BODY-LN SPN  IR-BUILD:SET-OP-SPAN ;

: M-RESULT+ ( -- )
   CC BB  CC BB A64IR:GPR-TYPE  IR-BUILD:ADD-RESULT ;

: M-MOVE-ATTRS ( n n -- )
   {: imm:n sh:n :}
   CC BB  CC BB A64IR:KEY-IMM    CC BB imm A64IR:IMM-ATTR    IR-BUILD:ADD-ATTR
   CC BB  CC BB A64IR:KEY-SHIFT  CC BB sh A64IR:SHIFT-ATTR   IR-BUILD:ADD-ATTR ;

: M-MOVZ ( n -- IR-ID:ir-value-id )
   {: imm:n :}
   A64IR-OPCODE:MOVZ M-OPEN
   M-RESULT+
   imm 0 M-MOVE-ATTRS
   CLOSE-VALUE ;

: M-MOVK ( IR-ID:ir-value-id n n -- IR-ID:ir-value-id )
   {: keep:IR-ID:ir-value-id imm:n sh:n :}
   A64IR-OPCODE:MOVK M-OPEN
   CC BB keep IR-BUILD:ADD-OPERAND
   M-RESULT+
   imm sh M-MOVE-ATTRS
   CLOSE-VALUE ;

: M-ADD ( IR-ID:ir-value-id IR-ID:ir-value-id -- IR-ID:ir-value-id )
   {: x:IR-ID:ir-value-id y:IR-ID:ir-value-id :}
   A64IR-OPCODE:ADD M-OPEN
   CC BB x IR-BUILD:ADD-OPERAND
   CC BB y IR-BUILD:ADD-OPERAND
   M-RESULT+
   CLOSE-VALUE ;

: M-RET ( IR-ID:ir-value-id -- )
   {: v:IR-ID:ir-value-id :}
   A64IR-OPCODE:RET M-OPEN
   CC BB v IR-BUILD:ADD-OPERAND
   CC BB IR-BUILD:END-OP drop ;

: M-RET2 ( IR-ID:ir-value-id IR-ID:ir-value-id -- )
   {: x:IR-ID:ir-value-id y:IR-ID:ir-value-id :}
   A64IR-OPCODE:RET M-OPEN
   CC BB x IR-BUILD:ADD-OPERAND
   CC BB y IR-BUILD:ADD-OPERAND
   CC BB IR-BUILD:END-OP drop ;

: M-FREEZE ( -- IR-BUILD:module )
   CC BB IR-BUILD:FREEZE ;

\ A move-wide overwrite whose kept half is added to the finished value: the one
\ register field of the overwrite would have to hold both.
: BUILD-LIVE-TIE ( -- )
   s" TIED" 0 1 OPEN-FUN
   $5678 M-MOVZ {: lo:IR-ID:ir-value-id :}
   lo $1234 48 M-MOVK {: hi:IR-ID:ir-value-id :}
   lo hi M-ADD M-RET
   CLOSE-FUN ;

\ A move-wide overwrite with other work between it and the move it continues, so
\ the register it has to land in is no longer the lowest free one. A chain the
\ selector emits back to back cannot tell a tied register from a lucky one; this
\ shape can.
: BUILD-INTERLEAVED ( -- )
   s" MIX" 2 1 OPEN-FUN
   ARG+ {: a:IR-ID:ir-value-id :}
   ARG+ {: b:IR-ID:ir-value-id :}
   $5678 M-MOVZ {: lo:IR-ID:ir-value-id :}
   a b M-ADD {: sum:IR-ID:ir-value-id :}
   lo $1234 48 M-MOVK {: hi:IR-ID:ir-value-id :}
   sum hi M-ADD M-RET
   CLOSE-FUN ;

\ A block argument that is not a general register of this dialect.
: BUILD-WRONG-CLASS ( -- )
   s" NARROW" 0 1 OPEN-FUN
   CC BB WORDT IR-BUILD:ADD-BLOCK-ARG drop
   7 M-MOVZ M-RET
   CLOSE-FUN ;

\ TWO FUNCTIONS IN ONE MODULE, which is what a definition that makes a quotation
\ compiles to: the first is the routine the definition names and the second is the
\ body of its quotation. They are built to differ, because two identical functions
\ would allocate identically and an allocation that measured only one of them, or
\ measured the second with the first's layout, would answer the same numbers.
\ The first holds two literals live at once; the second holds three.
: BUILD-TWO-FUNS ( -- )
   s" ONE" 0 1 OPEN-FUN
   $11 M-MOVZ {: a:IR-ID:ir-value-id :}
   $22 M-MOVZ {: b:IR-ID:ir-value-id :}
   a b M-ADD M-RET
   CLOSE-FUN
   s" TWO" 0 1 OPEN-FUN
   $33 M-MOVZ {: c:IR-ID:ir-value-id :}
   $44 M-MOVZ {: d:IR-ID:ir-value-id :}
   $55 M-MOVZ {: e:IR-ID:ir-value-id :}
   c d M-ADD {: s1:IR-ID:ir-value-id :}
   s1 e M-ADD M-RET
   CLOSE-FUN ;

\ A seventh machine operation, defined into this dialect's own table, with a tie
\ only when the fixture asks for one. Nothing in the substrate forbids a dialect
\ giving itself another form, and the allocator has no list of forms to consult:
\ it honours whatever this schema declares. The two fixtures below are therefore
\ what proves the tie is read out of the schema rather than recognised by name -
\ neither of them is the move-wide overwrite.
: EXTRA-SCHEMA ( bool -- IR-ID:ir-symbol-id )
   {: tied:bool :}
   CC BB s" a64.neg" IR-BUILD:INTERN-SYMBOL {: op:IR-ID:ir-symbol-id :}
   op IR-SCHEMA:BEGIN-OP
   CC BB A64IR:GPR-TYPE IR-SCHEMA:ADD-OPERAND
   CC BB A64IR:GPR-TYPE IR-SCHEMA:ADD-RESULT
   tied if 0 0 IR-SCHEMA:ADD-TIE then
   false 0 0 IR-SCHEMA:SET-CONTROL
   IR-SCHEMA:SET-PURE
   false IR-SCHEMA:SET-TRAP
   CTARGET-ARCH:AARCH64 CTARGET:F-BASE IR-SCHEMA:SET-TARGET
   CC BB s" a64.rule.neg" IR-BUILD:INTERN-SYMBOL IR-SCHEMA:SET-RULE
   CC BB s" a64.render.neg" IR-BUILD:INTERN-SYMBOL IR-SCHEMA:SET-RENDERER
   CC BB IR-BUILD:DEFINE-OP
   op ;

: M-NEG ( IR-ID:ir-symbol-id IR-ID:ir-value-id -- IR-ID:ir-value-id )
   {: op:IR-ID:ir-symbol-id v:IR-ID:ir-value-id :}
   CC BB op IR-BUILD:BEGIN-OP
   CC BB  BODY-ST BODY-LN SPN  IR-BUILD:SET-OP-SPAN
   CC BB v IR-BUILD:ADD-OPERAND
   M-RESULT+
   CLOSE-VALUE ;

\ The seventh form over a value produced well before it, with other work in
\ between, so a declared tie and no tie land in different registers: tied, the
\ result has to return to the register its operand is in; untied, it takes the
\ lowest free one, which by then is a different register.
: BUILD-EXTRA ( bool -- )
   {: tied:bool :}
   tied EXTRA-SCHEMA {: op:IR-ID:ir-symbol-id :}
   s" NEG" 2 1 OPEN-FUN
   ARG+ {: a:IR-ID:ir-value-id :}
   ARG+ {: b:IR-ID:ir-value-id :}
   $5678 M-MOVZ {: lo:IR-ID:ir-value-id :}
   a b M-ADD {: sum:IR-ID:ir-value-id :}
   op lo M-NEG {: neg:IR-ID:ir-value-id :}
   sum neg M-ADD M-RET
   CLOSE-FUN ;

\ An eighth form that ties both of its results, each to its own operand. Two ties
\ on one operation is what the move-wide overwrite cannot show, and a form is
\ free to declare it, so the walk has to honour every tie rather than the first.
: PAIR-SCHEMA ( -- IR-ID:ir-symbol-id )
   CC BB s" a64.pair" IR-BUILD:INTERN-SYMBOL {: op:IR-ID:ir-symbol-id :}
   op IR-SCHEMA:BEGIN-OP
   CC BB A64IR:GPR-TYPE IR-SCHEMA:ADD-OPERAND
   CC BB A64IR:GPR-TYPE IR-SCHEMA:ADD-OPERAND
   CC BB A64IR:GPR-TYPE IR-SCHEMA:ADD-RESULT
   CC BB A64IR:GPR-TYPE IR-SCHEMA:ADD-RESULT
   0 0 IR-SCHEMA:ADD-TIE
   1 1 IR-SCHEMA:ADD-TIE
   false 0 0 IR-SCHEMA:SET-CONTROL
   IR-SCHEMA:SET-PURE
   false IR-SCHEMA:SET-TRAP
   CTARGET-ARCH:AARCH64 CTARGET:F-BASE IR-SCHEMA:SET-TARGET
   CC BB s" a64.rule.pair" IR-BUILD:INTERN-SYMBOL IR-SCHEMA:SET-RULE
   CC BB s" a64.render.pair" IR-BUILD:INTERN-SYMBOL IR-SCHEMA:SET-RENDERER
   CC BB IR-BUILD:DEFINE-OP
   op ;

: M-PAIR ( IR-ID:ir-symbol-id IR-ID:ir-value-id IR-ID:ir-value-id -- IR-ID:ir-value-id IR-ID:ir-value-id )
   {: op:IR-ID:ir-symbol-id x:IR-ID:ir-value-id y:IR-ID:ir-value-id :}
   CC BB op IR-BUILD:BEGIN-OP
   CC BB  BODY-ST BODY-LN SPN  IR-BUILD:SET-OP-SPAN
   CC BB x IR-BUILD:ADD-OPERAND
   CC BB y IR-BUILD:ADD-OPERAND
   M-RESULT+
   M-RESULT+
   CC BB IR-BUILD:END-OP {: id:IR-ID:ir-op-id :}
   CC BB id 0 IR-BUILD:OP-RESULT@
   CC BB id 1 IR-BUILD:OP-RESULT@ ;

\ Both ties over two values of their own: each result returns to its own
\ operand's register.
: BUILD-PAIR ( -- )
   PAIR-SCHEMA {: op:IR-ID:ir-symbol-id :}
   s" PAIR" 0 1 OPEN-FUN
   $1111 M-MOVZ {: u:IR-ID:ir-value-id :}
   $2222 M-MOVZ {: v:IR-ID:ir-value-id :}
   op u v M-PAIR {: x:IR-ID:ir-value-id y:IR-ID:ir-value-id :}
   x y M-ADD M-RET
   CLOSE-FUN ;

\ The same form handed one value as both of its tied operands: the two results
\ would have to share the one register field that value is in.
: BUILD-PAIR-SHARED ( -- )
   PAIR-SCHEMA {: op:IR-ID:ir-symbol-id :}
   s" SHARED" 0 1 OPEN-FUN
   $1111 M-MOVZ {: u:IR-ID:ir-value-id :}
   op u u M-PAIR {: x:IR-ID:ir-value-id y:IR-ID:ir-value-id :}
   x y M-ADD M-RET
   CLOSE-FUN ;

\ The same refusal the move-wide overwrite gets, on a form that is not it: the
\ tied operand is read again after the operation that overwrites it.
: BUILD-EXTRA-LIVE-TIE ( -- )
   true EXTRA-SCHEMA {: op:IR-ID:ir-symbol-id :}
   s" NEGTIED" 0 1 OPEN-FUN
   $5678 M-MOVZ {: lo:IR-ID:ir-value-id :}
   op lo M-NEG {: neg:IR-ID:ir-value-id :}
   lo neg M-ADD M-RET
   CLOSE-FUN ;

\ ---- the frame forms, built by hand ------------------------------------------
\ The shapes a lowered module has, so the validator's frame rules can be measured
\ on modules that are wrong in exactly one way. Every one of them threads the
\ memory token the dialect's forms declare, because a module that does not is
\ refused by the freeze verifier and would prove nothing about this file.
: M-TOKEN+ ( -- )
   CC BB  CC BB A64IR:MEM-TYPE  IR-BUILD:ADD-RESULT ;

: M-FRAME-ATTR ( n -- )
   {: size:n :}
   CC BB  CC BB A64IR:KEY-FRAME  CC BB size A64IR:FRAME-ATTR  IR-BUILD:ADD-ATTR ;

: M-SLOT-ATTR ( n -- )
   {: off:n :}
   CC BB  CC BB A64IR:KEY-SLOT  CC BB off A64IR:SLOT-ATTR  IR-BUILD:ADD-ATTR ;

: M-RESERVE ( n -- IR-ID:ir-value-id )
   {: size:n :}
   A64IR-OPCODE:RESERVE M-OPEN
   M-TOKEN+
   size M-FRAME-ATTR
   CLOSE-VALUE ;

: M-RELEASE ( IR-ID:ir-value-id n -- )
   {: tok:IR-ID:ir-value-id size:n :}
   A64IR-OPCODE:RELEASE M-OPEN
   CC BB tok IR-BUILD:ADD-OPERAND
   size M-FRAME-ATTR
   CC BB IR-BUILD:END-OP drop ;

: M-STORE ( IR-ID:ir-value-id IR-ID:ir-value-id n -- IR-ID:ir-value-id )
   {: v:IR-ID:ir-value-id tok:IR-ID:ir-value-id off:n :}
   A64IR-OPCODE:STORE M-OPEN
   CC BB v IR-BUILD:ADD-OPERAND
   CC BB tok IR-BUILD:ADD-OPERAND
   M-TOKEN+
   off M-SLOT-ATTR
   CLOSE-VALUE ;

: M-LOAD ( IR-ID:ir-value-id n -- IR-ID:ir-value-id IR-ID:ir-value-id )
   {: tok:IR-ID:ir-value-id off:n :}
   A64IR-OPCODE:LOAD M-OPEN
   CC BB tok IR-BUILD:ADD-OPERAND
   M-RESULT+
   M-TOKEN+
   off M-SLOT-ATTR
   CC BB IR-BUILD:END-OP {: id:IR-ID:ir-op-id :}
   CC BB id 0 IR-BUILD:OP-RESULT@
   CC BB id 1 IR-BUILD:OP-RESULT@ ;

\ ---- the four data-stack forms, built by hand --------------------------------
\ Every number one of these carries is a DISTANCE from where the routine's
\ data-stack pointer stands, and where it stands is the entry form's own field
\ read against the place the caller left it. Building them by hand is the only
\ way to hand the validator a routine that stands somewhere the placement in
\ src/compiler/native/select.f would never have chosen - which is exactly what
\ the cases below are for.
: M-DSLOT-ATTR ( n -- )
   {: off:n :}
   CC BB  CC BB A64IR:KEY-DSLOT  CC BB off A64IR:DSLOT-ATTR  IR-BUILD:ADD-ATTR ;

: M-DBYTES-ATTR ( n -- )
   {: d:n :}
   CC BB  CC BB A64IR:KEY-DBYTES  CC BB d A64IR:DBYTES-ATTR  IR-BUILD:ADD-ATTR ;

: M-DTAKE ( n -- IR-ID:ir-value-id )
   {: d:n :}
   A64IR-OPCODE:DTAKE M-OPEN
   M-TOKEN+
   d M-DBYTES-ATTR
   CLOSE-VALUE ;

: M-DLOAD ( IR-ID:ir-value-id n -- IR-ID:ir-value-id IR-ID:ir-value-id )
   {: tok:IR-ID:ir-value-id off:n :}
   A64IR-OPCODE:DLOAD M-OPEN
   CC BB tok IR-BUILD:ADD-OPERAND
   M-RESULT+
   M-TOKEN+
   off M-DSLOT-ATTR
   CC BB IR-BUILD:END-OP {: id:IR-ID:ir-op-id :}
   CC BB id 0 IR-BUILD:OP-RESULT@
   CC BB id 1 IR-BUILD:OP-RESULT@ ;

: M-DSTORE ( IR-ID:ir-value-id IR-ID:ir-value-id n -- IR-ID:ir-value-id )
   {: v:IR-ID:ir-value-id tok:IR-ID:ir-value-id off:n :}
   A64IR-OPCODE:DSTORE M-OPEN
   CC BB v IR-BUILD:ADD-OPERAND
   CC BB tok IR-BUILD:ADD-OPERAND
   M-TOKEN+
   off M-DSLOT-ATTR
   CLOSE-VALUE ;

: M-DPUBLISH ( IR-ID:ir-value-id n -- )
   {: tok:IR-ID:ir-value-id d:n :}
   A64IR-OPCODE:DPUBLISH M-OPEN
   CC BB tok IR-BUILD:ADD-OPERAND
   d M-DBYTES-ATTR
   CC BB IR-BUILD:END-OP drop ;

\ The return of a routine under that convention carries nothing: its results are
\ already in the cells the caller reads them out of.
: M-RET0 ( -- )
   A64IR-OPCODE:RET M-OPEN
   CC BB IR-BUILD:END-OP drop ;

\ ---- a call site, built by hand ----------------------------------------------
\ The branch and the frame the caller's own return address lives in. A call site
\ carries TWO distances: where the pointer has to stand when the branch is taken,
\ which is the callee's argument base, and where it stands when the branch comes
\ back, which is the callee's result base. Both are measured from the place the
\ routine stands at, so a site can be moved without touching anything else in the
\ module - which is what the case below does.
: M-DBACK-ATTR ( n -- )
   {: d:n :}
   CC BB  CC BB A64IR:KEY-DBACK  CC BB d A64IR:DBACK-ATTR  IR-BUILD:ADD-ATTR ;

: M-LINK ( IR-ID:ir-value-id A64IR:opcode -- IR-ID:ir-value-id )
   {: tok:IR-ID:ir-value-id o:A64IR:opcode :}
   o M-OPEN
   CC BB tok IR-BUILD:ADD-OPERAND
   M-TOKEN+
   A64FRAME:LINK-SLOT M-SLOT-ATTR
   CLOSE-VALUE ;

: M-CALL ( IR-ID:ir-value-id n n -- IR-ID:ir-value-id )
   {: tok:IR-ID:ir-value-id give:n back:n :}
   A64IR-OPCODE:CALL M-OPEN
   CC BB tok IR-BUILD:ADD-OPERAND
   M-TOKEN+
   give M-DBYTES-ATTR
   back M-DBACK-ATTR
   CLOSE-VALUE ;

\ ---- the shape that cannot fit -----------------------------------------------
\ Five literals are materialised before any of them is read, so all five are live
\ at once and no pool of three can hold them. The sum then reads them in the
\ order they were made, which is what makes the cost rule visible: the two values
\ read LAST are the two the allocator has to put away.
: BUILD-CHAIN ( -- )
   s" CHAIN" 0 1 OPEN-FUN
   $11 M-MOVZ {: a:IR-ID:ir-value-id :}
   $22 M-MOVZ {: b:IR-ID:ir-value-id :}
   $33 M-MOVZ {: c:IR-ID:ir-value-id :}
   $44 M-MOVZ {: d:IR-ID:ir-value-id :}
   $55 M-MOVZ {: e:IR-ID:ir-value-id :}
   a b M-ADD {: s1:IR-ID:ir-value-id :}
   s1 c M-ADD {: s2:IR-ID:ir-value-id :}
   s2 d M-ADD {: s3:IR-ID:ir-value-id :}
   s3 e M-ADD M-RET
   CLOSE-FUN ;

\ THE SHAPE S1 REFUSES: a module whose SECOND function is the one that cannot fit.
\ The first function is a literal and a return, so nothing about it needs a slot;
\ the second is the five-literal chain above, which needs one. A module has one
\ frame and its first function owns it, so a slot handed to the second would be
\ addressed from a stack pointer that function never moved - and the walk refuses
\ by name instead. dot habu-give-each-fn-c1fd7c5a is what inverts this into a
\ publication; until then this is the boundary and this fixture is where it is
\ written down.
: BUILD-SECOND-SPILLS ( -- )
   s" FIRST" 0 1 OPEN-FUN
   $77 M-MOVZ M-RET
   CLOSE-FUN
   s" SECOND" 0 1 OPEN-FUN
   $11 M-MOVZ {: a:IR-ID:ir-value-id :}
   $22 M-MOVZ {: b:IR-ID:ir-value-id :}
   $33 M-MOVZ {: c:IR-ID:ir-value-id :}
   $44 M-MOVZ {: d:IR-ID:ir-value-id :}
   $55 M-MOVZ {: e:IR-ID:ir-value-id :}
   a b M-ADD {: s1:IR-ID:ir-value-id :}
   s1 c M-ADD {: s2:IR-ID:ir-value-id :}
   s2 d M-ADD {: s3:IR-ID:ir-value-id :}
   s3 e M-ADD M-RET
   CLOSE-FUN ;

\ Three literals in two registers, where the two candidates for eviction are
\ read by the same operation. Their next reads are equally far away, so nothing
\ but the tie rule decides which one goes into the frame - and the rule is the
\ lower register number.
: BUILD-TIE ( -- )
   s" TIE" 0 1 OPEN-FUN
   $11 M-MOVZ {: a:IR-ID:ir-value-id :}
   $22 M-MOVZ {: b:IR-ID:ir-value-id :}
   $33 M-MOVZ {: c:IR-ID:ir-value-id :}
   a b M-ADD {: s1:IR-ID:ir-value-id :}
   s1 c M-ADD M-RET
   CLOSE-FUN ;

\ An operation that reads one spilled value twice. One reload serves both reads,
\ because the value is in one register once it is back; a reload per read would
\ take a second register and spill something else to get it.
: BUILD-DOUBLE ( -- )
   s" DOUBLE" 0 1 OPEN-FUN
   $11 M-MOVZ {: a:IR-ID:ir-value-id :}
   $22 M-MOVZ {: b:IR-ID:ir-value-id :}
   $33 M-MOVZ {: c:IR-ID:ir-value-id :}
   b c M-ADD {: s1:IR-ID:ir-value-id :}
   a a M-ADD {: s2:IR-ID:ir-value-id :}
   s1 s2 M-ADD M-RET
   CLOSE-FUN ;

\ A module that already reserves a frame and still cannot fit its values in the
\ pool, so a second lowering is something a caller could really ask for. Lowering
\ it would build a second frame inside the first, and the slots the allocator
\ hands out start at the top of a frame it does not know is already in use.
: BUILD-FRAMED ( -- )
   s" FRAMED" 0 1 OPEN-FUN
   16 M-RESERVE {: tok:IR-ID:ir-value-id :}
   $11 M-MOVZ {: a:IR-ID:ir-value-id :}
   $22 M-MOVZ {: b:IR-ID:ir-value-id :}
   $33 M-MOVZ {: c:IR-ID:ir-value-id :}
   a tok 0 M-STORE {: t1:IR-ID:ir-value-id :}
   t1 0 M-LOAD {: w:IR-ID:ir-value-id t2:IR-ID:ir-value-id :}
   a b M-ADD {: s1:IR-ID:ir-value-id :}
   s1 c M-ADD {: s2:IR-ID:ir-value-id :}
   s2 w M-ADD {: s3:IR-ID:ir-value-id :}
   t2 16 M-RELEASE
   s3 M-RET
   CLOSE-FUN ;

\ The same shape with the store past the end of the frame the contract declares.
: BUILD-FAR-SLOT ( -- )
   s" FAR" 0 1 OPEN-FUN
   16 M-RESERVE {: tok:IR-ID:ir-value-id :}
   7 M-MOVZ {: v:IR-ID:ir-value-id :}
   v tok 16 M-STORE {: t1:IR-ID:ir-value-id :}
   t1 16 M-LOAD {: w:IR-ID:ir-value-id t2:IR-ID:ir-value-id :}
   t2 16 M-RELEASE
   w M-RET
   CLOSE-FUN ;

\ Two values put into one slot while both are still going to be read.
: BUILD-SHARED-SLOT ( -- )
   s" SHARED" 0 1 OPEN-FUN
   16 M-RESERVE {: tok:IR-ID:ir-value-id :}
   7 M-MOVZ {: v:IR-ID:ir-value-id :}
   9 M-MOVZ {: u:IR-ID:ir-value-id :}
   v tok 0 M-STORE {: t1:IR-ID:ir-value-id :}
   u t1 0 M-STORE {: t2:IR-ID:ir-value-id :}
   t2 0 M-LOAD {: w:IR-ID:ir-value-id t3:IR-ID:ir-value-id :}
   t3 16 M-RELEASE
   w M-RET
   CLOSE-FUN ;

\ A reload of a slot nothing ever stored to.
: BUILD-EMPTY-SLOT ( -- )
   s" EMPTY" 0 1 OPEN-FUN
   16 M-RESERVE {: tok:IR-ID:ir-value-id :}
   tok 0 M-LOAD {: w:IR-ID:ir-value-id t2:IR-ID:ir-value-id :}
   t2 16 M-RELEASE
   w M-RET
   CLOSE-FUN ;

\ A store whose order nothing reads. The release takes the order the RESERVE
\ minted rather than the one the store answered, so the module never says that
\ anything happens after the store - and it would still emit the same four
\ instructions in the same printed order, which is exactly why the claim has to
\ be checked rather than run.
: BUILD-LOOSE-ORDER ( -- )
   s" LOOSE" 0 1 OPEN-FUN
   16 M-RESERVE {: tok:IR-ID:ir-value-id :}
   7 M-MOVZ {: v:IR-ID:ir-value-id :}
   v tok 0 M-STORE drop
   tok 16 M-RELEASE
   v M-RET
   CLOSE-FUN ;

\ A frame the module takes that is not the frame the contract declares.
: BUILD-WRONG-FRAME ( -- )
   s" WRONGF" 0 1 OPEN-FUN
   32 M-RESERVE {: tok:IR-ID:ir-value-id :}
   7 M-MOVZ {: v:IR-ID:ir-value-id :}
   v tok 0 M-STORE {: t1:IR-ID:ir-value-id :}
   t1 0 M-LOAD {: w:IR-ID:ir-value-id t2:IR-ID:ir-value-id :}
   t2 32 M-RELEASE
   w M-RET
   CLOSE-FUN ;

\ A plain machine module the negative cases about state and binding can use.
: BUILD-PLAIN ( -- )
   s" PLAIN" 0 1 OPEN-FUN
   7 M-MOVZ M-RET
   CLOSE-FUN ;

: M-ALLOCATE ( n -- IR-BUILD:module )
   {: n:n :}
   M-FREEZE {: m:IR-BUILD:module :}
   CC m n LEAF-N A64RA:ALLOCATE
   m ;

\ A block that returns its first argument and holds no operation but the return.
\ Nothing about it can be decided except where its arguments arrive and where its
\ returned value leaves, which is why the fixed-register cases use it.
: BUILD-KEEP ( -- )
   s" KEEPM" 2 1 OPEN-FUN
   ARG+ {: a:IR-ID:ir-value-id :}
   ARG+ drop
   a M-RET
   CLOSE-FUN ;

\ Its contract: the arguments arrive in x0 and x1 the way a caller puts them, and
\ the returned value has to leave in x1 - which is not where the argument it
\ returns arrives. That is the one shape pre-colouring cannot serve.
: DECL-KEEP ( -- A64EFF:routine )
   4 POOL-N  A64EFF-CONV:REGISTER 0 1 SQ2  1 SQ  LEAF-DECL ;

\ ---- a hand-built module allocates the same way ------------------------------
\ The tie fixture below is the same chain the selector produces, so a module
\ built by hand and a module selected from source have to allocate alike; this is
\ what lets the hostile cases share the builder.
: PLAIN-BODY ( IR-CTX:ctx -- n n n )
   A64-MOD
   BUILD-PLAIN
   4 M-ALLOCATE {: m:IR-BUILD:module :}
   m 4 LEAF-N A64RAV:ACCEPT
   A64RA:VALUES
   0 A64RAV:REG@
   0 A64RA:LAST@ ;

: PLAIN-CASE ( -- )
   s" a hand-built machine module allocates and is accepted" T-LABEL
   WBND [: PLAIN-BODY ;] IR-CTX:WITH-CONTEXT
   2 T= 0 T= 1 T= ;

: INTERLEAVED-BODY ( IR-CTX:ctx -- n n n n n n n )
   A64-MOD
   BUILD-INTERLEAVED
   4 M-ALLOCATE {: m:IR-BUILD:module :}
   m 4 LEAF-N A64RAV:ACCEPT
   A64RA:VALUES
   0 A64RAV:REG@
   1 A64RAV:REG@
   2 A64RAV:REG@
   3 A64RAV:REG@
   4 A64RAV:REG@
   5 A64RAV:REG@ ;

: INTERLEAVED-CASE ( -- )
   s" an interleaved overwrite still lands in the register it keeps" T-LABEL
   WBND [: INTERLEAVED-BODY ;] IR-CTX:WITH-CONTEXT
   0 T= 2 T= 0 T= 2 T= 1 T= 0 T= 6 T= ;

\ The same interleaved shape over the dialect's seventh form, once with the tie
\ declared and once without. Nothing in the allocator names this form, so the two
\ answers differ only because the schemas do.
: EXTRA-BODY ( bool IR-CTX:ctx -- n n n n n n )
   {: tied:bool c:IR-CTX:ctx :}
   c A64-MOD
   tied BUILD-EXTRA
   4 M-ALLOCATE {: m:IR-BUILD:module :}
   m 4 LEAF-N A64RAV:ACCEPT
   0 A64RAV:REG@
   1 A64RAV:REG@
   2 A64RAV:REG@
   3 A64RAV:REG@
   4 A64RAV:REG@
   5 A64RAV:REG@ ;

: TIED-EXTRA-CASE ( -- )
   s" a tie declared on a form that is not the overwrite is honoured" T-LABEL
   true WBND [: EXTRA-BODY ;] IR-CTX:WITH-CONTEXT
   0 T= 2 T= 0 T= 2 T= 1 T= 0 T= ;

: UNTIED-EXTRA-CASE ( -- )
   s" the same form without the tie takes the lowest free register" T-LABEL
   false WBND [: EXTRA-BODY ;] IR-CTX:WITH-CONTEXT
   0 T= 1 T= 0 T= 2 T= 1 T= 0 T= ;

: PAIR-BODY ( IR-CTX:ctx -- n n n n n n )
   A64-MOD
   BUILD-PAIR
   4 M-ALLOCATE {: m:IR-BUILD:module :}
   m 4 LEAF-N A64RAV:ACCEPT
   A64RA:VALUES
   0 A64RAV:REG@
   1 A64RAV:REG@
   2 A64RAV:REG@
   3 A64RAV:REG@
   4 A64RAV:REG@ ;

: PAIR-CASE ( -- )
   s" a form that ties both of its results honours both ties" T-LABEL
   WBND [: PAIR-BODY ;] IR-CTX:WITH-CONTEXT
   0 T= 1 T= 0 T= 1 T= 0 T= 5 T= ;

\ ---- routines of more than one block -----------------------------------------
\ Everything above is one block, which the allocator's one rule covers as the
\ case N=1. That rule - a linear block order with global positions, liveness by
\ backward dataflow, one hull interval per value, one register per block-argument
\ class, the schema ties unioned into those classes, and the copies coalesced
\ into them where that keeps the class invariant - is what the fixtures below
\ state clause by clause, each asserting the exact register of every value.
\
\ THEY ARE BUILT BY HAND FOR THE REASON THE HOSTILE FIXTURES ARE. What is being
\ measured is a shape - an edge that carries a value, a back edge, a tie inside
\ an arm, a copy whose ends interfere and one whose ends do not - and a shape is
\ what a hand-built module can state and a compiled one can only happen to
\ contain. Every one of them goes through the real builder, the real freeze
\ verifier, the real A64RA:ALLOCATE and, when it is meant to be allocatable, the
\ real A64RAV:ACCEPT; nothing here re-implements a rule it is checking.
\
\ WHAT EACH ONE WOULD CATCH is written above it, as the allocator edit that
\ reddens it. Three of those were made and watched go red before this was
\ published: dropping the tie union, extending a live-IN value to the end of its
\ block, and dropping the coalescing step.
: BLOCK-ID ( n -- IR-ID:ir-block-id )
   {: k:n :}
   BB IR-BUILD:MODULE-KEY k IR-ID:PACK-BLOCK ;

: M-BLOCK+ ( -- )
   CC BB IR-BUILD:END-BLOCK drop
   CC BB IR-BUILD:BEGIN-BLOCK
   CC BB  OPEN-ST OPEN-LN SPN  IR-BUILD:SET-BLOCK-SPAN ;

\ The unconditional branch, whose operands are the values it hands its one
\ successor as that block's arguments.
: M-BR1 ( IR-ID:ir-value-id n -- )
   {: v:IR-ID:ir-value-id t:n :}
   A64IR-OPCODE:BR M-OPEN
   CC BB v IR-BUILD:ADD-OPERAND
   CC BB t BLOCK-ID IR-BUILD:ADD-SUCCESSOR
   CC BB IR-BUILD:END-OP drop ;

: M-BR2 ( IR-ID:ir-value-id IR-ID:ir-value-id n -- )
   {: x:IR-ID:ir-value-id y:IR-ID:ir-value-id t:n :}
   A64IR-OPCODE:BR M-OPEN
   CC BB x IR-BUILD:ADD-OPERAND
   CC BB y IR-BUILD:ADD-OPERAND
   CC BB t BLOCK-ID IR-BUILD:ADD-SUCCESSOR
   CC BB IR-BUILD:END-OP drop ;

\ The two-way branch, whose one operand is the register it tests and not a block
\ argument, so neither destination may take one.
: M-BRZ ( IR-ID:ir-value-id n n -- )
   {: v:IR-ID:ir-value-id z:n o:n :}
   A64IR-OPCODE:BRZ M-OPEN
   CC BB v IR-BUILD:ADD-OPERAND
   CC BB z BLOCK-ID IR-BUILD:ADD-SUCCESSOR
   CC BB o BLOCK-ID IR-BUILD:ADD-SUCCESSOR
   CC BB IR-BUILD:END-OP drop ;

: M-MOV ( IR-ID:ir-value-id -- IR-ID:ir-value-id )
   {: v:IR-ID:ir-value-id :}
   A64IR-OPCODE:MOV M-OPEN
   CC BB v IR-BUILD:ADD-OPERAND
   M-RESULT+
   CLOSE-VALUE ;

: M-SUB ( IR-ID:ir-value-id IR-ID:ir-value-id -- IR-ID:ir-value-id )
   {: x:IR-ID:ir-value-id y:IR-ID:ir-value-id :}
   A64IR-OPCODE:SUB M-OPEN
   CC BB x IR-BUILD:ADD-OPERAND
   CC BB y IR-BUILD:ADD-OPERAND
   M-RESULT+
   CLOSE-VALUE ;

\ Two blocks and one edge that carries a value. The branch moves nothing, so its
\ operand and the argument it lands in are one class and one register - and the
\ argument is written at the join's own position, which is one past the branch,
\ so the class is held across the edge rather than re-placed on the far side.
\ Reddened by: dropping the edge union in MB-EDGES-OF, which leaves the argument
\ a class of its own and the validator's VEDGE1 refusing the answer.
: BUILD-MB-EDGE ( -- )
   s" MBEDGE" 0 1 OPEN-FUN
   $11 M-MOVZ {: a:IR-ID:ir-value-id :}
   $22 M-MOVZ {: b:IR-ID:ir-value-id :}
   a b M-ADD {: s:IR-ID:ir-value-id :}
   s 1 M-BR1
   M-BLOCK+
   ARG+ M-RET
   CLOSE-FUN ;

\ A loop: an entry that sets the accumulator, the count and the constant one, a
\ header that takes the two loop-carried values as its arguments and tests the
\ count, an exit that returns the accumulator, and a body that computes the next
\ pair and hands them back round the back edge. The exit is laid out BEFORE the
\ body, which is what the selector's own block order does with a Forth loop, so
\ the body is the last stretch of the linear order and the back edge runs
\ backwards over it.
\
\ WHAT IT MEASURES, AND WHY IT IS THE SUM-TO SHAPE. Three things at once. The
\ constant one is live across the whole loop and never re-placed, so its hull
\ spans it. Each loop-carried value shares one register with the entry value that
\ starts it and the body value that replaces it - three values, one class, one
\ register. And the accumulator is live-IN to the body and not live-OUT of it: it
\ dies at the addition that reads it, which is the very operation that writes its
\ replacement, so the two do not clash and the class is legal.
\
\ Reddened by: extending a live-IN value to the end of its block in MB-EXTEND1
\ (the over-extension this allocator once had). The accumulator would then reach
\ the end of the body, overlap the value that replaces it, and the class the back
\ edge forces would be refused with E-A64RA-EDGE.
: BUILD-MB-LOOP ( -- )
   s" MBLOOP" 0 1 OPEN-FUN
   $0 M-MOVZ {: a0:IR-ID:ir-value-id :}
   $5 M-MOVZ {: n0:IR-ID:ir-value-id :}
   $1 M-MOVZ {: one:IR-ID:ir-value-id :}
   a0 n0 1 M-BR2
   M-BLOCK+                             \ block one: the header
   ARG+ {: acc:IR-ID:ir-value-id :}
   ARG+ {: n:IR-ID:ir-value-id :}
   n 2 3 M-BRZ
   M-BLOCK+                             \ block two: the exit
   acc M-RET
   M-BLOCK+                             \ block three: the body
   acc n M-ADD {: acc2:IR-ID:ir-value-id :}
   n one M-SUB {: n2:IR-ID:ir-value-id :}
   acc2 n2 1 M-BR2
   CLOSE-FUN ;

\ A move-wide overwrite inside a branch arm, with the arm arranged so that the
\ register the tie needs is NOT the lowest free one where the overwrite is
\ written. Two values reach the arm in the low two registers, the half-built
\ constant is made while they are still live and therefore lands in the third,
\ and the addition that reads them both stands between it and the overwrite - so
\ by the overwrite's own position the low registers are free again and the
\ overwrite's result would be handed one of them if nothing said it must return
\ to its operand's.
\
\ THIS IS THE REGRESSION TEST FOR THE DEFECT THAT MOTIVATED THIS FILE. The
\ multi-block walk once left the tie to the scan, and it held only because the
\ operand of an overwrite dies at the overwrite and the lowest free register
\ happened to be the one it had just given up. Reddened by: dropping the tie
\ union - removing MB-TIES from MB-RUN, or the UF-UNION from MB-TIE1 - which
\ gives the overwrite a register its own operand is not in, and the validator
\ refuses the routine with E-A64RAV-TIE.
: BUILD-MB-TIE ( -- )
   s" MBTIE" 0 1 OPEN-FUN
   $11 M-MOVZ {: p:IR-ID:ir-value-id :}
   $22 M-MOVZ {: q:IR-ID:ir-value-id :}
   $0 M-MOVZ {: t:IR-ID:ir-value-id :}
   t 1 2 M-BRZ
   M-BLOCK+                             \ block one: the arm that holds the tie
   $5678 M-MOVZ {: lo:IR-ID:ir-value-id :}
   p q M-ADD {: s:IR-ID:ir-value-id :}
   lo $1234 48 M-MOVK {: hi:IR-ID:ir-value-id :}
   s hi M-ADD {: r:IR-ID:ir-value-id :}
   r 3 M-BR1
   M-BLOCK+                             \ block two: the other arm
   $99 M-MOVZ {: u:IR-ID:ir-value-id :}
   u 3 M-BR1
   M-BLOCK+                             \ block three: the join
   ARG+ M-RET
   CLOSE-FUN ;

\ A copy whose two ends are never live at the same instant, with the same
\ arrangement as the tie above: the value it copies is made while two others hold
\ the low registers, and by the copy's own position those registers are free. Its
\ result therefore lands somewhere else unless the two ends are merged - and
\ merged they are one register, so the copy is a move of a register into itself
\ and the emitter writes no instruction for it.
\
\ Reddened by: dropping the coalescing step - removing MB-COALESCE from MB-RUN,
\ or the UF-UNION from MB-COALESCE1 - which leaves the copy's two ends in two
\ registers and a real instruction where a nothing should be.
: BUILD-MB-COPY ( -- )
   s" MBCOPY" 0 1 OPEN-FUN
   $11 M-MOVZ {: a:IR-ID:ir-value-id :}
   $22 M-MOVZ {: b:IR-ID:ir-value-id :}
   $33 M-MOVZ {: c:IR-ID:ir-value-id :}
   a b M-ADD {: s:IR-ID:ir-value-id :}
   c M-MOV {: c2:IR-ID:ir-value-id :}
   s c2 1 M-BR2
   M-BLOCK+
   ARG+ {: x:IR-ID:ir-value-id :}
   ARG+ {: y:IR-ID:ir-value-id :}
   x y M-ADD M-RET
   CLOSE-FUN ;

\ A copy whose source is read AFTER it - the shape a swap leaves, where the value
\ being copied out of the way is still wanted. Its two ends are live at the same
\ instant, so merging them would put two values in one register: the merge is not
\ made, the two ends keep two registers, and the copy stays a real instruction.
\
\ Reddened by: dropping the MB-CLASH? guard from MB-COALESCE1, which merges them
\ anyway and is caught by the class invariant with E-A64RA-EDGE.
: BUILD-MB-LIVE-COPY ( -- )
   s" MBLIVE" 0 1 OPEN-FUN
   $11 M-MOVZ {: a:IR-ID:ir-value-id :}
   a M-MOV {: a2:IR-ID:ir-value-id :}
   a a2 M-ADD {: s:IR-ID:ir-value-id :}
   s 1 M-BR1
   M-BLOCK+
   ARG+ M-RET
   CLOSE-FUN ;

\ An edge that hands over a value the destination still reads on its own account:
\ the branch's operand and the argument it lands in are one class by the edge
\ rule, and they are live at the same instant, so one register would have to hold
\ two values. The selector never builds it - it copies every value crossing an
\ argument-carrying edge into a value of its own first - so it is built here.
\ Reddened by: dropping MB-MEMBER-CK, or the MB-CLASSES loop that calls it, which
\ lets the class through and hands two live values one register.
: BUILD-MB-EDGE-CLASH ( -- )
   s" MBCLASH" 0 1 OPEN-FUN
   $11 M-MOVZ {: a:IR-ID:ir-value-id :}
   a 1 M-BR1
   M-BLOCK+
   ARG+ {: x:IR-ID:ir-value-id :}
   a x M-ADD M-RET
   CLOSE-FUN ;

\ A two-way branch one of whose destinations takes an argument. Nothing hands
\ that argument a value: a branch with two successors carries no operands but the
\ register it tests, so the argument arrives in whatever register the allocation
\ happened to give it. The allocator says nothing about it - its edge rule reads
\ single-successor terminators only - and the freeze verifier says nothing either,
\ because its own successor-argument rule is the single-successor one. The
\ validator is the only reader that refuses it, which is why this is the shape
\ that reaches VMULTI-CK. Reddened by: dropping VMULTI-CK from VEDGE-OF, which
\ accepts an argument no edge ever filled.
: BUILD-MB-MULTI-ARG ( -- )
   s" MBMULTI" 0 1 OPEN-FUN
   $0 M-MOVZ {: t:IR-ID:ir-value-id :}
   t 1 2 M-BRZ
   M-BLOCK+                             \ block one: takes an argument nobody fills
   ARG+ {: x:IR-ID:ir-value-id :}
   x 3 M-BR1
   M-BLOCK+                             \ block two
   $22 M-MOVZ {: u:IR-ID:ir-value-id :}
   u 3 M-BR1
   M-BLOCK+                             \ block three: the join
   ARG+ M-RET
   CLOSE-FUN ;

\ Four literals made before any of them is read, handed on through a branch: all
\ four are live where the fourth is written and a pool of three cannot hold them,
\ so one goes into the frame. WHICH one is the cost rule read over the whole
\ linear order - the first literal is the one read furthest away when the
\ register runs out - and the value crossing the edge is not a candidate at all,
\ because a class of more than one value would write one slot twice.
\
\ IT IS BUILT SO THE BLOCK COLUMN IS LOAD-BEARING. The store belongs in block
\ zero at operation one and the load in block one at operation three, and block
\ zero HAS an operation three. A lowering pass that matched a plan row on its
\ position alone would therefore consume the load's row while it was still
\ walking block zero and put the load there - which is why the two positions are
\ what they are rather than whatever fell out.
: BUILD-MB-CHAIN ( -- )
   s" MBCHAIN" 0 1 OPEN-FUN
   $33 M-MOVZ {: c:IR-ID:ir-value-id :}
   $11 M-MOVZ {: a:IR-ID:ir-value-id :}
   $22 M-MOVZ {: b:IR-ID:ir-value-id :}
   $44 M-MOVZ {: d:IR-ID:ir-value-id :}
   d 1 M-BR1
   M-BLOCK+
   ARG+ {: x:IR-ID:ir-value-id :}
   a b M-ADD {: s1:IR-ID:ir-value-id :}
   $55 M-MOVZ {: t:IR-ID:ir-value-id :}
   s1 t M-ADD {: s2:IR-ID:ir-value-id :}
   s2 c M-ADD {: s3:IR-ID:ir-value-id :}
   s3 x M-ADD M-RET
   CLOSE-FUN ;

\ ---- what the multi-block fixtures assert ------------------------------------
: MB-EDGE-BODY ( IR-CTX:ctx -- n n n n n )
   A64-MOD
   BUILD-MB-EDGE
   4 M-ALLOCATE {: m:IR-BUILD:module :}
   m 4 LEAF-N A64RAV:ACCEPT
   A64RA:VALUES
   0 A64RAV:REG@
   1 A64RAV:REG@
   2 A64RAV:REG@
   3 A64RAV:REG@ ;

: MB-EDGE-CASE ( -- )
   s" a branch's operand and the argument it lands in take one register" T-LABEL
   WBND [: MB-EDGE-BODY ;] IR-CTX:WITH-CONTEXT
   0 T= 0 T= 1 T= 0 T= 4 T= ;

: MB-LOOP-BODY ( IR-CTX:ctx -- n n n n n n n n )
   A64-MOD
   BUILD-MB-LOOP
   4 M-ALLOCATE {: m:IR-BUILD:module :}
   m 4 LEAF-N A64RAV:ACCEPT
   A64RA:VALUES
   0 A64RAV:REG@
   1 A64RAV:REG@
   2 A64RAV:REG@
   3 A64RAV:REG@
   4 A64RAV:REG@
   5 A64RAV:REG@
   6 A64RAV:REG@ ;

: MB-LOOP-CASE ( -- )
   s" a loop-carried value keeps one register from entry to back edge" T-LABEL
   WBND [: MB-LOOP-BODY ;] IR-CTX:WITH-CONTEXT
   1 T= 0 T= 1 T= 0 T= 2 T= 1 T= 0 T= 7 T= ;

\ The constant one is live over the whole loop, so its hull runs from where it is
\ written to the last position of the last block. Asserting the hull rather than
\ only the register is what tells a value held across a loop apart from one that
\ happened to keep a register nobody else wanted.
: MB-LOOP-HULL-BODY ( IR-CTX:ctx -- n n n n )
   A64-MOD
   BUILD-MB-LOOP
   4 M-ALLOCATE {: m:IR-BUILD:module :}
   m 4 LEAF-N A64RAV:ACCEPT
   2 A64RA:DEF@
   2 A64RA:LAST@
   3 A64RA:DEF@
   3 A64RA:LAST@ ;

: MB-LOOP-HULL-CASE ( -- )
   s" a value live across a loop is held over the whole of it" T-LABEL
   WBND [: MB-LOOP-HULL-BODY ;] IR-CTX:WITH-CONTEXT
   10 T= 5 T= 12 T= 3 T= ;

: MB-TIE-BODY ( IR-CTX:ctx -- n n n n n n n n n n )
   A64-MOD
   BUILD-MB-TIE
   4 M-ALLOCATE {: m:IR-BUILD:module :}
   m 4 LEAF-N A64RAV:ACCEPT
   A64RA:VALUES
   0 A64RAV:REG@
   1 A64RAV:REG@
   2 A64RAV:REG@
   3 A64RAV:REG@
   4 A64RAV:REG@
   5 A64RAV:REG@
   6 A64RAV:REG@
   7 A64RAV:REG@
   8 A64RAV:REG@ ;

: MB-TIE-CASE ( -- )
   s" a move-wide overwrite in a branch arm shares its operand's register" T-LABEL
   WBND [: MB-TIE-BODY ;] IR-CTX:WITH-CONTEXT
   0 T= 0 T= 0 T= 2 T= 0 T= 2 T= 2 T= 1 T= 0 T= 9 T= ;

: MB-COPY-BODY ( IR-CTX:ctx -- n n n n n n n n n )
   A64-MOD
   BUILD-MB-COPY
   4 M-ALLOCATE {: m:IR-BUILD:module :}
   m 4 LEAF-N A64RAV:ACCEPT
   A64RA:VALUES
   0 A64RAV:REG@
   1 A64RAV:REG@
   2 A64RAV:REG@
   3 A64RAV:REG@
   4 A64RAV:REG@
   5 A64RAV:REG@
   6 A64RAV:REG@
   7 A64RAV:REG@ ;

: MB-COPY-CASE ( -- )
   s" a copy whose ends never overlap becomes a move into the same register" T-LABEL
   WBND [: MB-COPY-BODY ;] IR-CTX:WITH-CONTEXT
   0 T= 2 T= 0 T= 2 T= 0 T= 2 T= 1 T= 0 T= 8 T= ;

: MB-LIVE-COPY-BODY ( IR-CTX:ctx -- n n n n n )
   A64-MOD
   BUILD-MB-LIVE-COPY
   4 M-ALLOCATE {: m:IR-BUILD:module :}
   m 4 LEAF-N A64RAV:ACCEPT
   A64RA:VALUES
   0 A64RAV:REG@
   1 A64RAV:REG@
   2 A64RAV:REG@
   3 A64RAV:REG@ ;

: MB-LIVE-COPY-CASE ( -- )
   s" a copy whose source outlives it keeps two registers" T-LABEL
   WBND [: MB-LIVE-COPY-BODY ;] IR-CTX:WITH-CONTEXT
   0 T= 0 T= 1 T= 0 T= 4 T= ;

\ What the walk decided for the branching shape, before anything was lowered: one
\ value in the frame, two rows, and each row's BLOCK as well as its position - the
\ store in block zero in front of the fourth literal, the load in block one in
\ front of the sum that reads it.
: MB-PLAN-BODY ( IR-CTX:ctx -- n n n n n n n n )
   A64-MOD
   BUILD-MB-CHAIN
   M-FREEZE {: m0:IR-BUILD:module :}
   CC m0 3 16 LEAF-FRAMED A64RA:ALLOCATE
   A64RA:SPILLS
   A64RA:PLAN-N
   0 A64RA:PLAN-VALUE@
   0 A64RA:PLAN-BLOCK@
   0 A64RA:PLAN-POS@
   1 A64RA:PLAN-VALUE@
   1 A64RA:PLAN-BLOCK@
   1 A64RA:PLAN-POS@ ;

: MB-PLAN-CASE ( -- )
   s" a spill decision in a routine that branches names the block it belongs in"
   T-LABEL
   WBND [: MB-PLAN-BODY ;] IR-CTX:WITH-CONTEXT
   3 T= 1 T= 0 T= 1 T= 0 T= 0 T= 2 T= 1 T= ;

\ ---- the multi-block refusals ------------------------------------------------
: MB-EDGE-CLASH-BODY ( IR-CTX:ctx -- )
   A64-MOD
   BUILD-MB-EDGE-CLASH
   4 M-ALLOCATE drop ;

\ One register cannot hold the two literals the sum reads, and no slot can help:
\ both are operands of the SAME operation, so putting either away does not free a
\ register that operation can use. That is E-A64RA-POOL and not a frame running
\ out. This body used to be refused for its frame - it declares none - but a walk
\ is not held to its contract's frame any more, so what is left is the wall that
\ was always underneath it.
: MB-SPILL-BODY ( IR-CTX:ctx -- )
   A64-MOD
   BUILD-MB-EDGE
   1 M-ALLOCATE drop ;

\ The chain of DERIVE-FRAME-CASES again with two more registers, which is enough
\ that only ONE value has to be put away. It is here because that is the case
\ where the rounding is visible: eight bytes reached and sixteen declared are
\ different numbers, where four slots reached and declared are both thirty-two.
\ A change that answered the depth where the declaration belongs would leave
\ DERIVE-FRAME-CASES green and only this red.
: ROUND-FRAME-BODY ( IR-CTX:ctx -- )
   A64-MOD
   BUILD-CHAIN
   M-FREEZE {: m0:IR-BUILD:module :}
   CC m0 4 16 LEAF-FRAMED A64RA:ALLOCATE ;

\ The same pressure over a loop, where every class holding a register is one an
\ edge forced: the loop-carried accumulator and count each hold three values, so
\ neither can go into a slot - one value per slot is what makes a reload's value
\ decidable from the module alone. Nothing here can be put away, and that is the
\ refusal E-A64RA-SPILL has narrowed to.
: MB-CARRIED-BODY ( IR-CTX:ctx -- )
   A64-MOD
   BUILD-MB-LOOP
   1 M-ALLOCATE drop ;

\ A convention that names a register for the value this routine returns, on a
\ routine that branches. It used to be refused, because pre-colouring an argument
\ and planning a move in front of the return were both written against one block;
\ there is one allocation path now and both are stated about a CLASS, so the
\ declaration is honoured here exactly as it is on a routine of one block.
\
\ WHAT MAKES IT MORE THAN "IT DID NOT THROW". The value the return carries is the
\ join block's own argument, which the edge rule has already joined into one class
\ with the sum that feeds it - so honouring the declaration means giving that
\ whole class x0, and the validator's OUT-CK reads the register off the accepted
\ assignment at the terminator. A walk that pre-coloured only the member the
\ return names would hand the successor's argument one register and the operand
\ feeding it another, and A64RAV:VEDGE1 would refuse it. And it costs no copy:
\ MOVES is zero because the class was free to take x0 where it was written.
: MB-FIXED-CONTRACT ( -- A64EFF:routine )
   4 POOL-N  A64EFF-CONV:REGISTER A64EFF:SEQ-NONE  0 SQ LEAF-DECL ;

: MB-FIXED-BODY ( IR-CTX:ctx -- n n n n n )
   A64-MOD
   BUILD-MB-EDGE
   M-FREEZE {: m:IR-BUILD:module :}
   CC m MB-FIXED-CONTRACT A64RA:ALLOCATE
   m MB-FIXED-CONTRACT A64RAV:ACCEPT
   A64RA:MOVES
   A64RA:SPILLS
   A64RA:VALUES
   2 A64RAV:REG@
   3 A64RAV:REG@ ;

: MB-FIXED-CASE ( -- )
   s" a declared result register is delivered by a routine that branches" T-LABEL
   WBND [: MB-FIXED-BODY ;] IR-CTX:WITH-CONTEXT
   0 T= 0 T= 4 T= 0 T= 0 T= ;

: MB-MULTI-ARG-BODY ( IR-CTX:ctx -- )
   A64-MOD
   BUILD-MB-MULTI-ARG
   4 M-ALLOCATE {: m:IR-BUILD:module :}
   m 4 LEAF-N A64RAV:ACCEPT ;

\ ---- lowering a spill --------------------------------------------------------
\ The whole route a program that does not fit takes: allocate it, and if the walk
\ decided any spill, build the module those decisions are operations in and
\ allocate that. The second walk decides nothing, because it reads a module whose
\ operations already are the ones the first walk assumed.
: SPILL-BIND ( -- )
   CC BB A64SPILL:BIND-DIALECT ;

: LOWERED ( n n -- IR-BUILD:module )
   {: n:n f:n :}
   M-FREEZE {: m0:IR-BUILD:module :}
   CC m0 n f LEAF-FRAMED A64RA:ALLOCATE
   A64-BUILDER {: nb:IR-BUILD:builder :}
   CC nb A64RA:BIND-DIALECT
   CC nb A64RAV:BIND-DIALECT
   CC m0 nb TXT TXT-N A64SPILL:REWRITE {: m1:IR-BUILD:module :}
   CC m1 n f LEAF-FRAMED A64RA:ALLOCATE
   m1 n f LEAF-FRAMED A64RAV:ACCEPT
   m1 ;

\ What the walk decided, before anything was lowered. Five values are live where
\ the fifth literal is written and three registers hold them, so two have to go
\ into the frame - and WHICH two is the cost rule: the sum reads the literals in
\ the order they were made, so the third and fourth are the ones read furthest
\ away when the register runs out.
: PLAN-BODY ( IR-CTX:ctx -- n n n n n n n n n n )
   A64-MOD
   SPILL-BIND
   BUILD-CHAIN
   M-FREEZE {: m0:IR-BUILD:module :}
   CC m0 3 16 LEAF-FRAMED A64RA:ALLOCATE
   A64SPILL:RELEASE
   A64RA:SPILLS
   A64RA:PLAN-N
   0 A64RA:PLAN-VALUE@
   0 A64RA:PLAN-POS@
   1 A64RA:PLAN-VALUE@
   1 A64RA:PLAN-POS@
   2 A64RA:PLAN-VALUE@
   2 A64RA:PLAN-POS@
   2 A64RA:SLOT@
   3 A64RA:SLOT@ ;

: PLAN-CASE ( -- )
   s" the values read furthest away are the ones that lose their register" T-LABEL
   WBND [: PLAN-BODY ;] IR-CTX:WITH-CONTEXT
   8 T= 0 T= 6 T= 2 T= 4 T= 3 T= 3 T= 2 T= 4 T= 2 T= ;

\ The lowered module allocates with no spill left, and every value of it is
\ accepted. The exact registers are asserted, so a cost rule that chose another
\ victim - or a lowering that put a store or a load anywhere else - moves them.
: LOWER-BODY ( IR-CTX:ctx -- n n bool bool n n n bool n )
   A64-MOD
   SPILL-BIND
   BUILD-CHAIN
   3 16 LOWERED drop
   A64RA:SPILLS
   A64RA:VALUES
   A64RAV:ACCEPTED?
   0 A64RAV:REGISTERED?
   1 A64RAV:REG@
   2 A64RAV:REG@
   3 A64RAV:REG@
   4 A64RAV:REGISTERED?
   5 A64RAV:REG@ ;

: LOWER-CASE ( -- )
   s" a block that does not fit is lowered and then allocates" T-LABEL
   WBND [: LOWER-BODY ;] IR-CTX:WITH-CONTEXT
   2 T= TFALSE 2 T= 1 T= 0 T= TFALSE TTRUE 16 T= 0 T= ;

\ The same program through the whole route: lower the store and the load into
\ operations of the blocks they belong to, allocate the module that holds them,
\ and have the validator accept it. The second walk decides nothing, because it
\ reads a module whose operations already are the ones the first walk assumed.
\ Value zero of the lowered module is the memory order the reserve mints, value
\ one is the literal that goes into the frame, and value ten is the register the
\ load brings it back into.
: MB-LOWER-BODY ( IR-CTX:ctx -- n n bool bool n n )
   A64-MOD
   SPILL-BIND
   BUILD-MB-CHAIN
   3 16 LOWERED drop
   A64RA:SPILLS
   A64RA:VALUES
   A64RAV:ACCEPTED?
   0 A64RAV:REGISTERED?
   1 A64RAV:REG@
   10 A64RAV:REG@ ;

: MB-LOWER-CASE ( -- )
   s" a routine that branches and does not fit is lowered and then allocates"
   T-LABEL
   WBND [: MB-LOWER-BODY ;] IR-CTX:WITH-CONTEXT
   1 T= 0 T= TFALSE TTRUE 14 T= 0 T= ;

\ Which of two equally distant values loses its register. Both are read by the
\ same operation, so the cost rule cannot separate them and the tie rule does:
\ the first plan row names the value in the lower register.
\
\ AND THE STORE STANDS WHERE THE VALUE IS WRITTEN, not where the register runs
\ out. A class that loses its register loses it for the whole of its life, so the
\ store is anchored to the operation after the one that made the value - index
\ one, the second move-wide - rather than to the third, which is where the pool
\ happened to run short.
: TIE-SPILL-BODY ( IR-CTX:ctx -- n n n )
   A64-MOD
   BUILD-TIE
   M-FREEZE {: m0:IR-BUILD:module :}
   CC m0 2 16 LEAF-FRAMED A64RA:ALLOCATE
   A64RA:SPILLS
   0 A64RA:PLAN-VALUE@
   0 A64RA:PLAN-POS@ ;

: TIE-SPILL-CASE ( -- )
   s" a tie between two equally distant values goes to the lower register" T-LABEL
   WBND [: TIE-SPILL-BODY ;] IR-CTX:WITH-CONTEXT
   1 T= 0 T= 2 T= ;

\ One reload serves every read of one value by one operation. A reload per read
\ would need a second register at that operation, and taking one means spilling
\ something else - so the count of decisions is what measures it.
: DOUBLE-BODY ( IR-CTX:ctx -- n n n n )
   A64-MOD
   BUILD-DOUBLE
   M-FREEZE {: m0:IR-BUILD:module :}
   CC m0 2 32 LEAF-FRAMED A64RA:ALLOCATE
   A64RA:SPILLS
   A64RA:PLAN-N
   0 A64RA:PLAN-VALUE@
   0 A64RA:PLAN-POS@ ;

: DOUBLE-CASE ( -- )
   s" one reload serves both reads of a value by one operation" T-LABEL
   WBND [: DOUBLE-BODY ;] IR-CTX:WITH-CONTEXT
   1 T= 0 T= 2 T= 1 T= ;

\ ---- a returned value that has to be moved -----------------------------------
\ The value the return carries is an argument, pinned where the caller put it, and
\ the contract says it leaves somewhere else. The walk cannot pre-colour it - the
\ caller's choice wins - so it plans a copy in front of the return, exactly the
\ way it plans a store for a value that lost its register.
: MOVE-PLAN-BODY ( IR-CTX:ctx -- n n bool n n n )
   A64-MOD
   BUILD-KEEP
   M-FREEZE {: m:IR-BUILD:module :}
   CC m DECL-KEEP A64RA:ALLOCATE
   A64RA:MOVES
   A64RA:PLAN-N
   0 A64RA:PLAN-MOVE?
   0 A64RA:PLAN-VALUE@
   0 A64RA:PLAN-POS@
   0 A64RA:CLAIM@ ;

: MOVE-PLAN-CASE ( -- )
   s" a returned value that is not where it has to leave is planned a copy" T-LABEL
   WBND [: MOVE-PLAN-BODY ;] IR-CTX:WITH-CONTEXT
   0 T= 0 T= 0 T= TTRUE 1 T= 1 T= ;

\ The same program through the whole route: lower the copy into an operation and
\ allocate the module that holds it. The second walk plans nothing, because the
\ copy's own result is a value it can put in the declared register - and the
\ validator accepts, which is what makes the answer readable at all.
: MOVE-LOWER-BODY ( IR-CTX:ctx -- n n n n n n )
   A64-MOD
   SPILL-BIND
   BUILD-KEEP
   M-FREEZE {: m0:IR-BUILD:module :}
   CC m0 DECL-KEEP A64RA:ALLOCATE
   A64-BUILDER {: nb:IR-BUILD:builder :}
   CC nb A64RA:BIND-DIALECT
   CC nb A64RAV:BIND-DIALECT
   CC m0 nb TXT TXT-N A64SPILL:REWRITE {: m1:IR-BUILD:module :}
   CC m1 DECL-KEEP A64RA:ALLOCATE
   m1 DECL-KEEP A64RAV:ACCEPT
   A64RA:MOVES
   A64RA:SPILLS
   A64RA:VALUES
   0 A64RAV:REG@
   1 A64RAV:REG@
   2 A64RAV:REG@ ;

: MOVE-LOWER-CASE ( -- )
   s" the lowered copy lands in the register the contract declares" T-LABEL
   WBND [: MOVE-LOWER-BODY ;] IR-CTX:WITH-CONTEXT
   1 T= 1 T= 0 T= 3 T= 0 T= 0 T= ;

\ ---- refusals ----------------------------------------------------------------
\ Each of these four runs the validator as well, even though the allocator
\ refuses first. That is what makes the validator's own line reachable: an
\ allocator that stopped refusing would hand the shape to the validator, and the
\ refusal would come from there under a name of its own rather than not at all.
: REFUSE-SHAPE ( -- )
   4 M-ALLOCATE {: m:IR-BUILD:module :}
   m 4 LEAF-N A64RAV:ACCEPT ;

: LIVE-TIE-BODY ( IR-CTX:ctx -- )
   A64-MOD
   BUILD-LIVE-TIE
   REFUSE-SHAPE ;

: WRONG-CLASS-BODY ( IR-CTX:ctx -- )
   A64-MOD
   BUILD-WRONG-CLASS
   REFUSE-SHAPE ;

\ BOTH FUNCTIONS' ASSIGNMENTS, AND WHERE EACH ONE SITS ON THE MODULE'S ONE NUMBER
\ LINE. The definition positions are the whole point: they say that the second
\ function was measured AFTER the first rather than from its own zero, which is
\ what makes the intervals of the two disjoint and what lets the second reuse the
\ first's registers without the two ever being live at once. An allocation that
\ restarted the line would give the second function's first value the same
\ definition position as the first function's, and the validator would refuse the
\ shared register - so this is asserted through A64RAV:ACCEPT, not around it.
: TWO-FUNS-BODY ( IR-CTX:ctx -- n n n n n n n n n n n n n )
   A64-MOD
   BUILD-TWO-FUNS
   4 M-ALLOCATE {: m:IR-BUILD:module :}
   m 4 LEAF-N A64RAV:ACCEPT
   A64RA:VALUES
   0 A64RAV:REG@   1 A64RAV:REG@   2 A64RAV:REG@
   3 A64RAV:REG@   4 A64RAV:REG@   5 A64RAV:REG@
   6 A64RAV:REG@   7 A64RAV:REG@
   0 A64RA:DEF@   2 A64RA:LAST@
   3 A64RA:DEF@   7 A64RA:LAST@ ;

\ The numbers, read top down as every case here reads them. The four positions
\ come first because they are the claim: function ONE occupies 1 to 4 and
\ function TWO 6 to 11, one continuous line with no overlap - and the registers
\ then show function TWO's first value taking register 0 back, which is sound
\ only because ONE's values are all dead by then.
: SECOND-SPILLS-BODY ( IR-CTX:ctx -- )
   A64-MOD
   BUILD-SECOND-SPILLS
   M-FREEZE {: m:IR-BUILD:module :}
   CC m 3 16 LEAF-FRAMED A64RA:ALLOCATE ;

: SECOND-SPILLS ( -- )
   WBND [: SECOND-SPILLS-BODY ;] IR-CTX:WITH-CONTEXT ;

: TWO-FUNS-CASE ( -- )
   s" a module of two functions allocates both of them on one number line" T-LABEL
   WBND [: TWO-FUNS-BODY ;] IR-CTX:WITH-CONTEXT
   11 T=  6 T=                          \ function TWO: first definition, last read
   4 T=   1 T=                          \ function ONE: first definition, last read
   0 T=   0 T=   2 T=   1 T=   0 T=     \ TWO's registers, top value first
   0 T=   1 T=   0 T=                   \ ONE's registers
   8 T=                                 \ every value of the module was measured

   s" a function after the first that needs a frame slot is refused by name" T-LABEL
   [: SECOND-SPILLS ;] E-A64RA-FRAME TTHROWSQ ;

: EXTRA-LIVE-TIE-BODY ( IR-CTX:ctx -- )
   A64-MOD
   BUILD-EXTRA-LIVE-TIE
   REFUSE-SHAPE ;

: PAIR-SHARED-BODY ( IR-CTX:ctx -- )
   A64-MOD
   BUILD-PAIR-SHARED
   REFUSE-SHAPE ;

\ A program that spills AND declares where its result leaves. The two are one
\ plan and one lowering, so this is what proves they compose: the lowered module
\ holds the sixteen values the stores and loads make of the original nine, needs
\ no further spill, and the value the return carries is in the declared register
\ - x1, not the x0 the same program lands in when nothing is declared. It costs
\ no copy either, because the walk pre-colours the last sum straight into x1.
: DECL-SPILL-CONTRACT ( -- A64EFF:routine )
   3 POOL-N  A64EFF-CONV:REGISTER A64EFF:SEQ-NONE  1 SQ  LEAF-DECL-FRAMED ;

: DECL-SPILL-BODY ( IR-CTX:ctx -- n n n n )
   A64-MOD
   SPILL-BIND
   BUILD-CHAIN
   M-FREEZE {: m0:IR-BUILD:module :}
   CC m0 DECL-SPILL-CONTRACT A64RA:ALLOCATE
   A64-BUILDER {: nb:IR-BUILD:builder :}
   CC nb A64RA:BIND-DIALECT
   CC nb A64RAV:BIND-DIALECT
   CC m0 nb TXT TXT-N A64SPILL:REWRITE {: m1:IR-BUILD:module :}
   CC m1 DECL-SPILL-CONTRACT A64RA:ALLOCATE
   m1 DECL-SPILL-CONTRACT A64RAV:ACCEPT
   A64RA:SPILLS
   A64RA:MOVES
   A64RA:VALUES
   A64RA:VALUES 1- A64RAV:REG@ ;

: DECL-SPILL-CASE ( -- )
   s" a program that spills still leaves its result where it is declared" T-LABEL
   WBND [: DECL-SPILL-BODY ;] IR-CTX:WITH-CONTEXT
   1 T= 16 T= 0 T= 0 T= ;

\ Two returned values whose declared registers cross. The first literal is still
\ live when the second is written, so the value that has to leave in x0 cannot
\ have x0 - the value that has to leave in x1 was given it when x0 was busy - and
\ putting them both right needs the two copies ordered, or a temporary when they
\ need each other's register. That is the parallel copy this pass does not have,
\ so the shape is refused rather than half-served.
: BUILD-CROSS ( -- )
   s" CROSS" 0 2 OPEN-FUN
   $11 M-MOVZ {: w:IR-ID:ir-value-id :}
   $22 M-MOVZ {: b:IR-ID:ir-value-id :}
   w b M-ADD {: t:IR-ID:ir-value-id :}
   $33 M-MOVZ {: a:IR-ID:ir-value-id :}
   b a M-RET2
   CLOSE-FUN ;

: CROSS-BODY ( IR-CTX:ctx -- )
   A64-MOD
   BUILD-CROSS
   M-FREEZE {: m:IR-BUILD:module :}
   CC m  2 POOL-N  A64EFF-CONV:REGISTER A64EFF:SEQ-NONE  0 1 SQ2 LEAF-DECL A64RA:ALLOCATE ;

\ ---- fixed constraints an allocation cannot honour ---------------------------
\ A declared argument register the routine may not write. The pool is x0 to x3
\ and the convention says the first argument arrives in x5, which this routine
\ has promised to preserve: it could not be held there at all.
: ARG-OUT-OF-POOL-BODY ( IR-CTX:ctx -- )
   A64-MOD
   BUILD-KEEP
   M-FREEZE {: m:IR-BUILD:module :}
   CC m  4 POOL-N  A64EFF-CONV:REGISTER 5 0 SQ2  A64EFF:SEQ-NONE LEAF-DECL A64RA:ALLOCATE ;

\ A convention that names three argument positions for a routine that has two.
: OVER-ARG-BODY ( IR-CTX:ctx -- )
   A64-MOD
   BUILD-KEEP
   M-FREEZE {: m:IR-BUILD:module :}
   CC m  4 POOL-N  A64EFF-CONV:REGISTER 0 1 2 SQ3  A64EFF:SEQ-NONE LEAF-DECL A64RA:ALLOCATE ;

\ And one that names two returned values for a routine that returns one.
: OVER-OUT-BODY ( IR-CTX:ctx -- )
   A64-MOD
   BUILD-KEEP
   M-FREEZE {: m:IR-BUILD:module :}
   CC m  4 POOL-N  A64EFF-CONV:REGISTER A64EFF:SEQ-NONE  0 1 SQ2 LEAF-DECL A64RA:ALLOCATE ;

\ ---- data-stack places the allocation cannot honour -------------------------
\ A convention declaring an argument in a data-stack slot describes a module the
\ selector already turned that place into a load in - so the block has no
\ argument for it. Handed a module that still carries its arguments as block
\ arguments, this allocation would leave them in registers no caller ever wrote
\ to, and it refuses instead. A side that names a register place and a data-stack
\ place at once never reaches this pass: A64EFF:ROUTINE refuses that contract
\ where it is built, and test/compiler/a64-effect.f is where the case lives.
: DSLOT-Q ( n -- A64EFF:placeseq )
   A64EFF:SEQ-NONE swap A64EFF:SEQ-WITH-SLOT ;

: UNLOWERED-BODY ( IR-CTX:ctx -- )
   A64-MOD
   BUILD-KEEP
   M-FREEZE {: m:IR-BUILD:module :}
   CC m  4 POOL-N  A64EFF-CONV:DSTACK 0 DSLOT-Q  A64EFF:SEQ-NONE LEAF-DECL A64RA:ALLOCATE ;

\ Two registers cannot hold three arguments at once, and a block argument is one
\ of the things this pass may not put in a frame: the values feeding it across
\ every edge would have to move with it, and the caller has already put it where
\ it is. So the refusal is not the frame at all - the routine declares none, and
\ the walk never gets as far as asking for a slot - but "nothing here may be
\ taken", which is E-A64RA-SPILL. The frame's own wall is SMALL-FRAME below.
: PRESSURE-BODY ( IR-CTX:ctx -- )
   HIR-MOD
   BUILD-SUM3
   SELECTED {: m:IR-BUILD:module :}
   CC m 2 LEAF-N A64RA:ALLOCATE ;

\ The same chain in two registers, which needs three slots, against a frame that
\ holds two. A frame is a multiple of the stack alignment and a slot is half of
\ one, so "one slot short" is a frame of sixteen bytes and a program that wants
\ twenty-four.
: SMALL-FRAME-BODY ( IR-CTX:ctx -- )
   A64-MOD
   BUILD-CHAIN
   M-FREEZE {: m0:IR-BUILD:module :}
   CC m0 2 16 LEAF-FRAMED A64RA:ALLOCATE ;

\ Lowering a module that already reserves a frame: it has been through the pass
\ once, and a second frame inside the first is not a thing this pass builds.
: TWICE-LOWER-BODY ( IR-CTX:ctx -- )
   A64-MOD
   SPILL-BIND
   BUILD-FRAMED
   M-FREEZE {: m0:IR-BUILD:module :}
   CC m0 2 48 LEAF-FRAMED A64RA:ALLOCATE
   A64-BUILDER {: nb:IR-BUILD:builder :}
   CC m0 nb TXT TXT-N A64SPILL:REWRITE drop ;

\ Lowering a module whose walk decided no spill at all.
: NO-SPILL-LOWER-BODY ( IR-CTX:ctx -- )
   A64-MOD
   SPILL-BIND
   BUILD-PLAIN
   M-FREEZE {: m0:IR-BUILD:module :}
   CC m0 4 16 LEAF-FRAMED A64RA:ALLOCATE
   A64-BUILDER {: nb:IR-BUILD:builder :}
   CC m0 nb TXT TXT-N A64SPILL:REWRITE drop ;

\ ---- the frame rules, on modules that are wrong in one way -------------------
\ Each of these is a lowered shape with one thing changed, allocated and then
\ presented to the validator. The allocator itself has nothing to say about them
\ - it hands out no slot here, because nothing spills - so the refusal is the
\ validator's own judgement about the module in front of it.
: FRAME-REFUSE ( n -- )
   {: f:n :}
   M-FREEZE {: m:IR-BUILD:module :}
   CC m 4 f LEAF-FRAMED A64RA:ALLOCATE
   m 4 f LEAF-FRAMED A64RAV:ACCEPT ;

: FAR-SLOT-BODY ( IR-CTX:ctx -- )
   A64-MOD
   BUILD-FAR-SLOT
   16 FRAME-REFUSE ;

: SHARED-SLOT-BODY ( IR-CTX:ctx -- )
   A64-MOD
   BUILD-SHARED-SLOT
   16 FRAME-REFUSE ;

: EMPTY-SLOT-BODY ( IR-CTX:ctx -- )
   A64-MOD
   BUILD-EMPTY-SLOT
   16 FRAME-REFUSE ;

: WRONG-FRAME-BODY ( IR-CTX:ctx -- )
   A64-MOD
   BUILD-WRONG-FRAME
   16 FRAME-REFUSE ;

: LOOSE-ORDER-BODY ( IR-CTX:ctx -- )
   A64-MOD
   BUILD-LOOSE-ORDER
   16 FRAME-REFUSE ;

: EMPTY-POOL-BODY ( IR-CTX:ctx -- )
   A64-MOD
   BUILD-PLAIN
   0 M-ALLOCATE drop ;

\ Allocating a module the binding was not taken over.
: WRONG-MODULE-BODY ( IR-CTX:ctx -- )
   HIR-MOD
   BUILD-SQUARE
   A64-BUILDER {: ab:IR-BUILD:builder :}
   CC ab A64RA:BIND-DIALECT
   CC BB IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   CC m 4 LEAF-N A64RA:ALLOCATE ;

: NO-BIND-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   IR-BUILD:PLAN-BEGIN
   IR-BUILD:PLAN-DEFAULT
   c A64IR:NEW-BUILDER {: b:IR-BUILD:builder :}
   c 0 W-CTX !
   b 0 W-BLD !
   c b A64IR:REGISTER
   c b TXT TXT-N IR-BUILD:ADD-SOURCE 0 W-SRC !
   BUILD-PLAIN
   4 M-ALLOCATE drop ;

: TWICE-BIND-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   IR-BUILD:PLAN-BEGIN
   IR-BUILD:PLAN-DEFAULT
   c A64IR:NEW-BUILDER {: b:IR-BUILD:builder :}
   c b A64RA:BIND-DIALECT
   c b A64RA:BIND-DIALECT ;

: WRONG-DIALECT-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   IR-BUILD:PLAN-BEGIN
   IR-BUILD:PLAN-DEFAULT
   c HIR:NEW-BUILDER {: b:IR-BUILD:builder :}
   c b A64RA:BIND-DIALECT ;

\ These registers belong to one architecture; a context bound to another machine
\ has none of them. The module is built under the machine this dialect is for and
\ presented under one it is not.
: WRONG-TARGET-INNER ( IR-BUILD:module IR-CTX:ctx -- )
   {: m:IR-BUILD:module c:IR-CTX:ctx :}
   c m 4 LEAF-N A64RA:ALLOCATE ;

: WRONG-TARGET-BODY ( IR-CTX:ctx -- )
   A64-MOD
   BUILD-PLAIN
   M-FREEZE {: m:IR-BUILD:module :}
   m PBND [: WRONG-TARGET-INNER ;] IR-CTX:WITH-CONTEXT ;

\ The validator refuses an allocation it was handed the wrong module for, and one
\ made for a different set of registers.
: ACCEPT-WRONG-MODULE-BODY ( IR-CTX:ctx -- )
   HIR-MOD
   BUILD-SQUARE
   CC BB A64SEL:BIND-SOURCE
   CC BB IR-BUILD:FREEZE {: hm:IR-BUILD:module :}
   A64-BUILDER {: ab:IR-BUILD:builder :}
   CC ab A64RA:BIND-DIALECT
   CC hm ab TXT TXT-N  A64EFF:GPR-NONE LEAF  A64SEL:SELECT {: m:IR-BUILD:module :}
   CC m 4 LEAF-N A64RA:ALLOCATE
   hm 4 LEAF-N A64RAV:ACCEPT ;

: ACCEPT-WRONG-POOL-BODY ( IR-CTX:ctx -- )
   A64-MOD
   BUILD-PLAIN
   4 M-ALLOCATE {: m:IR-BUILD:module :}
   m 3 LEAF-N A64RAV:ACCEPT ;

\ The validator reads the declaration itself rather than anything the allocator
\ kept. Each of these three allocates under a contract that declares nothing -
\ so the arguments take the low registers in order and the returned value stays
\ where it was - and then asks for an acceptance under a contract that declares
\ something the assignment does not satisfy, over the same registers, so the
\ declaration is the only thing that differs.
: ACCEPT-WRONG-ARG-BODY ( IR-CTX:ctx -- )
   A64-MOD
   BUILD-KEEP
   4 M-ALLOCATE {: m:IR-BUILD:module :}
   m  4 POOL-N  A64EFF-CONV:REGISTER 1 0 SQ2  A64EFF:SEQ-NONE LEAF-DECL A64RAV:ACCEPT ;

: ACCEPT-WRONG-OUT-BODY ( IR-CTX:ctx -- )
   A64-MOD
   BUILD-KEEP
   4 M-ALLOCATE {: m:IR-BUILD:module :}
   m  4 POOL-N  A64EFF-CONV:REGISTER A64EFF:SEQ-NONE  1 SQ LEAF-DECL A64RAV:ACCEPT ;

: ACCEPT-OVER-ARG-BODY ( IR-CTX:ctx -- )
   A64-MOD
   BUILD-KEEP
   4 M-ALLOCATE {: m:IR-BUILD:module :}
   m  4 POOL-N  A64EFF-CONV:REGISTER 0 1 2 SQ3  A64EFF:SEQ-NONE LEAF-DECL A64RAV:ACCEPT ;

\ A claim nobody checked is not an answer, and an answer stops being one when a
\ later walk replaces the allocation it was about.
: UNCHECKED-BODY ( IR-CTX:ctx -- )
   A64-MOD
   BUILD-PLAIN
   4 M-ALLOCATE drop
   0 A64RAV:REG@ drop ;

: STALE-BODY ( IR-CTX:ctx -- )
   A64-MOD
   BUILD-PLAIN
   4 M-ALLOCATE {: m:IR-BUILD:module :}
   m 4 LEAF-N A64RAV:ACCEPT
   CC A64-MOD
   BUILD-PLAIN
   4 M-ALLOCATE drop
   0 A64RAV:REG@ drop ;

\ ---- registers no routine may hold state in ----------------------------------
\ x18 is platform-reserved, x30 is the link register and 31 is the zero register
\ or the stack pointer. None of them can enter a contract at all, so no pool the
\ allocator is given can contain one.
: RESERVED-CASES ( -- )
   s" the platform-reserved register cannot be declared destroyable" T-LABEL
   [: A64EFF:RESERVED-GPR A64EFF:GPR-REG drop ;] E-A64EFF-GPR TTHROWSQ
   s" the data-stack pointer cannot be declared destroyable" T-LABEL
   [: A64EFF:DSTACK-GPR A64EFF:GPR-REG drop ;] E-A64EFF-GPR TTHROWSQ
   s" the data-stack pointer cannot be declared as an argument place" T-LABEL
   [: A64EFF:DSTACK-GPR SQ A64EFF:SEQ-LEN drop ;] E-A64EFF-GPR TTHROWSQ
   s" the link register cannot be declared destroyable" T-LABEL
   [: A64EFF:LINK-GPR A64EFF:GPR-REG drop ;] E-A64EFF-GPR TTHROWSQ
   s" the zero register cannot be declared destroyable" T-LABEL
   [: A64EFF:ZERO-GPR A64EFF:GPR-REG drop ;] E-A64EFF-GPR TTHROWSQ ;

\ ---- refusal cases -----------------------------------------------------------
: LIVE-TIE ( -- )         WBND [: LIVE-TIE-BODY ;] IR-CTX:WITH-CONTEXT ;
: WRONG-CLASS ( -- )      WBND [: WRONG-CLASS-BODY ;] IR-CTX:WITH-CONTEXT ;
: EXTRA-LIVE-TIE ( -- )   WBND [: EXTRA-LIVE-TIE-BODY ;] IR-CTX:WITH-CONTEXT ;
: PAIR-SHARED ( -- )      WBND [: PAIR-SHARED-BODY ;] IR-CTX:WITH-CONTEXT ;
: PRESSURE ( -- )         WBND [: PRESSURE-BODY ;] IR-CTX:WITH-CONTEXT ;
: ARG-OUT-OF-POOL ( -- )  WBND [: ARG-OUT-OF-POOL-BODY ;] IR-CTX:WITH-CONTEXT ;
: CROSS ( -- )            WBND [: CROSS-BODY ;] IR-CTX:WITH-CONTEXT ;
: OVER-ARG ( -- )         WBND [: OVER-ARG-BODY ;] IR-CTX:WITH-CONTEXT ;
: OVER-OUT ( -- )         WBND [: OVER-OUT-BODY ;] IR-CTX:WITH-CONTEXT ;
: ACCEPT-WRONG-ARG ( -- )
   WBND [: ACCEPT-WRONG-ARG-BODY ;] IR-CTX:WITH-CONTEXT ;
: ACCEPT-WRONG-OUT ( -- )
   WBND [: ACCEPT-WRONG-OUT-BODY ;] IR-CTX:WITH-CONTEXT ;
: ACCEPT-OVER-ARG ( -- )
   WBND [: ACCEPT-OVER-ARG-BODY ;] IR-CTX:WITH-CONTEXT ;
: SMALL-FRAME ( -- )      WBND [: SMALL-FRAME-BODY ;] IR-CTX:WITH-CONTEXT ;
: TWICE-LOWER ( -- )      WBND [: TWICE-LOWER-BODY ;] IR-CTX:WITH-CONTEXT ;
: NO-SPILL-LOWER ( -- )   WBND [: NO-SPILL-LOWER-BODY ;] IR-CTX:WITH-CONTEXT ;
: FAR-SLOT ( -- )         WBND [: FAR-SLOT-BODY ;] IR-CTX:WITH-CONTEXT ;
: SHARED-SLOT ( -- )      WBND [: SHARED-SLOT-BODY ;] IR-CTX:WITH-CONTEXT ;
: EMPTY-SLOT ( -- )       WBND [: EMPTY-SLOT-BODY ;] IR-CTX:WITH-CONTEXT ;
: WRONG-FRAME ( -- )      WBND [: WRONG-FRAME-BODY ;] IR-CTX:WITH-CONTEXT ;
: LOOSE-ORDER ( -- )     WBND [: LOOSE-ORDER-BODY ;] IR-CTX:WITH-CONTEXT ;
: EMPTY-POOL ( -- )       WBND [: EMPTY-POOL-BODY ;] IR-CTX:WITH-CONTEXT ;
: WRONG-MODULE ( -- )     WBND [: WRONG-MODULE-BODY ;] IR-CTX:WITH-CONTEXT ;
: NO-BIND ( -- )          WBND [: NO-BIND-BODY ;] IR-CTX:WITH-CONTEXT ;
: TWICE-BIND ( -- )       WBND [: TWICE-BIND-BODY ;] IR-CTX:WITH-CONTEXT ;
: WRONG-DIALECT ( -- )    WBND [: WRONG-DIALECT-BODY ;] IR-CTX:WITH-CONTEXT ;
: WRONG-TARGET ( -- )     WBND [: WRONG-TARGET-BODY ;] IR-CTX:WITH-CONTEXT ;
: ACCEPT-WRONG-MODULE ( -- )
   WBND [: ACCEPT-WRONG-MODULE-BODY ;] IR-CTX:WITH-CONTEXT ;
: ACCEPT-WRONG-POOL ( -- )
   WBND [: ACCEPT-WRONG-POOL-BODY ;] IR-CTX:WITH-CONTEXT ;
: UNCHECKED ( -- )        WBND [: UNCHECKED-BODY ;] IR-CTX:WITH-CONTEXT ;
: STALE ( -- )            WBND [: STALE-BODY ;] IR-CTX:WITH-CONTEXT ;
: MB-EDGE-CLASH ( -- )    WBND [: MB-EDGE-CLASH-BODY ;] IR-CTX:WITH-CONTEXT ;
: MB-SPILL ( -- )         WBND [: MB-SPILL-BODY ;] IR-CTX:WITH-CONTEXT ;
: ROUND-FRAME ( -- )      WBND [: ROUND-FRAME-BODY ;] IR-CTX:WITH-CONTEXT ;
: MB-CARRIED ( -- )       WBND [: MB-CARRIED-BODY ;] IR-CTX:WITH-CONTEXT ;
: MB-MULTI-ARG ( -- )     WBND [: MB-MULTI-ARG-BODY ;] IR-CTX:WITH-CONTEXT ;

: DROP-BINDING ( -- )
   A64RA:RELEASE ;

: SHAPE-REFUSE-CASES ( -- )
   s" a value that is not a general register of this dialect is refused" T-LABEL
   [: WRONG-CLASS ;] E-A64RA-CLASS TTHROWSQ ;

: TIE-REFUSE-CASES ( -- )
   s" a move-wide overwrite whose kept value is read again is refused" T-LABEL
   [: LIVE-TIE ;] E-A64RA-TIE TTHROWSQ
   s" a tied operand of any form, read again, is refused the same way" T-LABEL
   [: EXTRA-LIVE-TIE ;] E-A64RA-TIE TTHROWSQ
   s" one value as two tied operands of one operation is refused" T-LABEL
   [: PAIR-SHARED ;] E-A64RA-TIE TTHROWSQ ;

: PRESSURE-REFUSE-CASES ( -- )
   s" more arguments live at once than the pool holds, and none may be put away" T-LABEL
   [: PRESSURE ;] E-A64RA-SPILL TTHROWSQ
   \ The refusal just above left no sealed walk, so there is no claim to read.
   s" a refused allocation leaves no claim behind" T-LABEL
   [: 0 A64RA:CLAIM@ drop ;] E-A64RA-STATE TTHROWSQ ;

\ ---- the frame the walk derives ----------------------------------------------
\ A contract's frame used to be a wall the walk was held to: a routine whose
\ author declared two slots and whose program needed three was refused with
\ E-A64RA-PRESSURE, though the chain compiles it perfectly well. Nobody was in a
\ position to declare that number - it is decided by the walk - so the
\ declaration is the walk's OUTPUT now and this is where that is pinned
\ (habu-derive-a-routine-84ed36b6).
\
\ THE BODY IS THE ONE THAT USED TO BE REFUSED, unchanged, and the frame it is
\ handed is still the sixteen bytes that used to be too small. Three values go to
\ slots, which wants twenty-four bytes below the entry; a frame is a multiple of
\ the stack alignment, so the routine has to declare thirty-two. Both numbers are
\ read, because the rounded one is what a caller declares and the unrounded one
\ is what the program actually reached, and a change that confused them would
\ leave one of the two right.
\
\ WHAT MAKES THIS MORE THAN A RESTATEMENT of the allocator's arithmetic: the same
\ two numbers are what src/compiler/native/spill.f sizes its reserve from and what
\ src/compiler/native/abi.f builds a declaration from, and the validator refuses
\ any difference between the two. Pinning them here is pinning the handshake.
: DERIVE-FRAME-CASES ( -- )
   s" a frame one slot short of what the spills need is derived, not refused" T-LABEL
   SMALL-FRAME
   s" four values went to slots" T-LABEL
   A64RA:SPILLS 4 T=
   s" the walk reached thirty-two bytes below the entry" T-LABEL
   A64RA:FRAME-USED 32 T=
   s" so the routine has to declare a frame of thirty-two" T-LABEL
   A64RA:FRAME 32 T= ;

: POOL-REFUSE-CASES ( -- )
   s" a routine that may destroy nothing allocates nothing" T-LABEL
   [: EMPTY-POOL ;] E-A64RA-POOL TTHROWSQ ;

\ The first of these is the contract's own refusal and not the allocator's: two
\ arguments declared into one register is a convention no caller could satisfy,
\ so no contract carrying it can be built and no allocation is ever reached.
: FIXED-REFUSE-CASES ( -- )
   s" two argument positions in one register cannot be declared at all" T-LABEL
   [: 0 0 SQ2 A64EFF:SEQ-LEN drop ;] E-A64EFF-SEQ TTHROWSQ
   s" a declared argument register the routine may not write is refused" T-LABEL
   [: ARG-OUT-OF-POOL ;] E-A64RA-FIXED TTHROWSQ
   s" more declared arguments than the routine has is refused" T-LABEL
   [: OVER-ARG ;] E-A64RA-FIXED TTHROWSQ
   s" more declared results than the routine returns is refused" T-LABEL
   [: OVER-OUT ;] E-A64RA-FIXED TTHROWSQ ;

: UNLOWERED ( -- )
   WBND [: UNLOWERED-BODY ;] IR-CTX:WITH-CONTEXT ;

: PLACE-REFUSE-CASES ( -- )
   s" data-stack places on a module that still carries block arguments are refused" T-LABEL
   [: UNLOWERED ;] E-A64RA-PLACE TTHROWSQ ;

\ Its own group for the reason CROSS-REFUSE-CASE below has one: every refusing
\ body abandons a context, and the live-arena registry gives those slots back
\ only when the enclosing context leaves.
: MB-CARRIED-REFUSE-CASE ( -- )
   s" a routine whose only held classes are loop-carried is refused" T-LABEL
   [: MB-CARRIED ;] E-A64RA-SPILL TTHROWSQ ;

\ Its own group: each refusing body above abandons a context, and the live-arena
\ registry gives those slots back only when the enclosing context leaves.
: CROSS-REFUSE-CASE ( -- )
   s" two returned values that need each other's register are refused" T-LABEL
   [: CROSS ;] E-A64RA-FIXED TTHROWSQ ;

\ The validator's own judgement about the same declaration.
: FIXED-ACCEPT-CASES ( -- )
   s" an assignment that ignores a declared argument register is refused" T-LABEL
   [: ACCEPT-WRONG-ARG ;] E-A64RAV-FIXED TTHROWSQ
   s" an assignment that ignores a declared result register is refused" T-LABEL
   [: ACCEPT-WRONG-OUT ;] E-A64RAV-FIXED TTHROWSQ
   s" accepting under a convention with more positions than the module is refused" T-LABEL
   [: ACCEPT-OVER-ARG ;] E-A64RAV-FIXED TTHROWSQ ;

: LOWER-TWICE-CASE ( -- )
   s" lowering a module that already reserves a frame is refused" T-LABEL
   [: TWICE-LOWER ;] E-A64SPILL-SHAPE TTHROWSQ ;

: LOWER-NONE-CASE ( -- )
   s" lowering a module whose walk decided no spill is refused" T-LABEL
   [: NO-SPILL-LOWER ;] E-A64SPILL-PLAN TTHROWSQ ;

: SLOT-REFUSE-CASES ( -- )
   s" a slot outside the declared frame is refused" T-LABEL
   [: FAR-SLOT ;] E-A64EFF-SLOT TTHROWSQ
   s" two values in one slot are refused" T-LABEL
   [: SHARED-SLOT ;] E-A64RAV-SHARE TTHROWSQ ;

: RELOAD-REFUSE-CASES ( -- )
   s" a reload of a slot nothing stored to is refused" T-LABEL
   [: EMPTY-SLOT ;] E-A64RAV-RELOAD TTHROWSQ
   s" a frame that is not the one the contract declares is refused" T-LABEL
   [: WRONG-FRAME ;] E-A64RAV-FRAME TTHROWSQ ;

: ORDER-REFUSE-CASES ( -- )
   s" a memory order the module mints and nothing reads is refused" T-LABEL
   [: LOOSE-ORDER ;] E-A64RAV-ORDER TTHROWSQ ;

: BIND-REFUSE-CASES ( -- )
   s" allocating without a binding is refused" T-LABEL
   [: NO-BIND ;] E-A64RA-BIND TTHROWSQ
   s" a second binding over a live one is refused" T-LABEL
   [: TWICE-BIND ;] E-A64RA-BIND TTHROWSQ
   DROP-BINDING ;

: MODULE-REFUSE-CASES ( -- )
   s" a frozen module the binding was not taken over is refused" T-LABEL
   [: WRONG-MODULE ;] E-A64RA-MODULE TTHROWSQ
   s" binding a builder of another dialect is refused" T-LABEL
   [: WRONG-DIALECT ;] E-A64RA-MODULE TTHROWSQ ;

: TARGET-REFUSE-CASES ( -- )
   s" allocating under a context bound to another machine is refused" T-LABEL
   [: WRONG-TARGET ;] E-A64RA-TARGET TTHROWSQ ;

: ACCEPT-REFUSE-CASES ( -- )
   s" accepting against a module the allocation was not made from is refused" T-LABEL
   [: ACCEPT-WRONG-MODULE ;] E-A64RAV-MODULE TTHROWSQ
   s" accepting under a different set of registers is refused" T-LABEL
   [: ACCEPT-WRONG-POOL ;] E-A64RAV-CONTRACT TTHROWSQ ;

\ ---- the refusals a routine of more than one block earns ---------------------
\ The first is the allocator's class invariant, stated over a class the edge rule
\ forces; the second is the frame it was given nothing of. The third is the
\ validator's alone: the allocator's edge rule reads single-successor terminators
\ only, so nothing in it looks at a two-way branch's destinations at all.
\
\ WHAT IS NOT REACHED FROM HERE, AND WHY IT IS STILL WRITTEN. Two clauses of the
\ validator's edge rule stay fail-closed. An edge whose operand count differs
\ from the destination's argument count is refused by the freeze verifier before
\ either the allocator or this file sees it, and the allocator refuses it again
\ under E-A64RA-EDGE, so VEDGE-OF's own count clause answers for neither.
\ VEDGE1 - a terminator's operand and the argument it fills given two different
\ registers - and OVERLAP-CK - two values live at one instant given one register -
\ are properties the allocator's class rule makes impossible, so only mutating
\ the allocator reaches them. Both are still written there, and both are what
\ turned the three mutations named above from wrong code into a red suite; a
\ fixture that faked them would measure this file's arithmetic and not the
\ validator's.
: MB-REFUSE-CASES ( -- )
   s" two values live at once forced into one class by an edge are refused" T-LABEL
   [: MB-EDGE-CLASH ;] E-A64RA-EDGE TTHROWSQ
   s" two operands of one operation in one register: no slot can help" T-LABEL
   [: MB-SPILL ;] E-A64RA-POOL TTHROWSQ ;

\ Where the rounding shows. A frame is a multiple of what the stack pointer moves
\ by, so a routine that reaches one slot below its entry still declares two slots
\ worth - and the declaration is the number src/compiler/native/abi.f builds from
\ the same count and src/compiler/native/spill.f sizes its reserve from, so the
\ two have to be this one and not the depth.
: ROUND-FRAME-CASES ( -- )
   s" a walk that reaches one slot declares the alignment above it" T-LABEL
   ROUND-FRAME
   s" one value went to a slot" T-LABEL
   A64RA:SPILLS 1 T=
   s" the walk reached eight bytes below the entry" T-LABEL
   A64RA:FRAME-USED 8 T=
   s" and the frame it has to declare rounds up to sixteen" T-LABEL
   A64RA:FRAME 16 T= ;

: MB-ACCEPT-REFUSE-CASES ( -- )
   s" a two-way branch into a block that takes an argument is refused" T-LABEL
   [: MB-MULTI-ARG ;] E-A64RAV-EDGE TTHROWSQ ;

: STATE-REFUSE-CASES ( -- )
   s" a claim no validator has accepted is not an answer" T-LABEL
   [: UNCHECKED ;] E-A64RAV-STATE TTHROWSQ
   s" an accepted answer stops answering when a later walk replaces it" T-LABEL
   [: STALE ;] E-A64RAV-STATE TTHROWSQ ;

\ ---- where the data-stack pointer stands, and the four ways to get it wrong ---
\ WHAT THESE MODULES ARE. `: DBL ( n -- n ) dup + ;` under the data-stack
\ convention, built straight into the machine dialect so that the one number the
\ placement chooses can be set to something the placement would never choose. The
\ routine takes one cell and leaves one, so the place the caller left the pointer
\ and the place it expects it back are the same place, 8: standing there costs no
\ adjustment at either end, and the one cell the routine touches is then one
\ BELOW the pointer, which is the -8 the two accesses carry.
\
\ AND THE FIRST ONE IS THE CANONICAL LOWERING, asserted to be ACCEPTED, because
\ every refusal below is a refusal of a module that differs from it in exactly
\ one number. Without it the four cases would only say "these are rejected" and
\ not "these are rejected and the right one is not".
: BUILD-DBL ( n n n -- )
   {: stand:n off:n leave:n :}
   s" DBL" 1 1 OPEN-FUN
   A64IR:SLOT-WIDTH stand - M-DTAKE {: t0:IR-ID:ir-value-id :}
   t0 off M-DLOAD {: v:IR-ID:ir-value-id t1:IR-ID:ir-value-id :}
   v v M-ADD {: w:IR-ID:ir-value-id :}
   w t1 off M-DSTORE {: t2:IR-ID:ir-value-id :}
   t2  leave stand -  M-DPUBLISH
   M-RET0
   CLOSE-FUN ;

: DBL-ACCEPT ( n n n -- )
   {: stand:n off:n leave:n :}
   stand off leave BUILD-DBL
   M-FREEZE {: m:IR-BUILD:module :}
   CC m  4 1 1 HABU-N  A64RA:ALLOCATE
   m  4 1 1 HABU-N  A64RAV:ACCEPT ;

\ The lowering the placement really makes: standing at 8, both adjustments zero,
\ both accesses one cell under the pointer.
: DBL-CANON-BODY ( IR-CTX:ctx -- bool )
   A64-MOD
   8 -8 8 DBL-ACCEPT
   A64RAV:ACCEPTED? ;

\ THE POINTER MOVED AND THE ACCESSES NOT MOVED WITH IT. The entry form says the
\ body stands at the base; every access still names the cell it named when the
\ body stood one above it. Nothing about the module is out of range and nothing
\ is missing: what is wrong is that two statements about one pointer disagree,
\ and the cell the load then names is under the caller's window altogether.
: DBL-MISPLACED-BODY ( IR-CTX:ctx -- )
   A64-MOD
   0 -8 8 DBL-ACCEPT ;

\ THE REQUIRED MOVE SKIPPED. The body stands at the base, so the results are
\ published from there and the pointer has to be moved up over them before the
\ routine returns - and this module leaves it where it stood. Every access is in
\ range and every cell is the cell it should be; the routine simply hands its
\ caller a stack whose top is one cell out.
: DBL-UNPUBLISHED-BODY ( IR-CTX:ctx -- )
   A64-MOD
   0 0 0 DBL-ACCEPT ;

\ THE NET-ZERO PAIR KEPT. This module is CORRECT: it stands at the base, moves
\ the pointer down at entry and back up at exit, and names every cell rightly -
\ it is what this chain emitted before the placement existed. What is wrong with
\ it is that the two moves cancel, so a place that needed neither was available
\ and two instructions are spent reaching one that needed both. The validator
\ re-derives the choice and refuses it, exactly as it refuses a store that writes
\ what the cell already holds.
: DBL-NET-ZERO-BODY ( IR-CTX:ctx -- )
   A64-MOD
   0 0 8 DBL-ACCEPT ;

\ THE PLACE OUT OF REACH. A body standing further above the base than the
\ unscaled field reaches could not address the base at all, so the place is
\ refused where it is derived rather than at whichever access first could not be
\ encoded.
: DBL-DEEP-BODY ( IR-CTX:ctx -- )
   A64-MOD
   A64EFF:SLOT-BACK A64IR:SLOT-WIDTH + -8 8 DBL-ACCEPT ;

: DBL-CANON ( -- bool )
   WBND [: DBL-CANON-BODY ;] IR-CTX:WITH-CONTEXT ;

: DBL-MISPLACED ( -- )
   WBND [: DBL-MISPLACED-BODY ;] IR-CTX:WITH-CONTEXT ;

: DBL-UNPUBLISHED ( -- )
   WBND [: DBL-UNPUBLISHED-BODY ;] IR-CTX:WITH-CONTEXT ;

: DBL-NET-ZERO ( -- )
   WBND [: DBL-NET-ZERO-BODY ;] IR-CTX:WITH-CONTEXT ;

: DBL-DEEP ( -- )
   WBND [: DBL-DEEP-BODY ;] IR-CTX:WITH-CONTEXT ;

: PLACE-ACCEPT-CASE ( -- )
   s" the lowering that stands where the fewest adjustments are needed is accepted"
   T-LABEL
   DBL-CANON TTRUE ;

: PLACE-MISPLACED-CASES ( -- )
   s" a pointer moved without its accesses is refused" T-LABEL
   [: DBL-MISPLACED ;] E-A64RAV-DSTACK TTHROWSQ
   s" and so is a routine that returns with the pointer where the body stood"
   T-LABEL
   [: DBL-UNPUBLISHED ;] E-A64RAV-DSTACK TTHROWSQ ;

: PLACE-CANON-CASES ( -- )
   s" a pair of moves that cancel is refused, place and all" T-LABEL
   [: DBL-NET-ZERO ;] E-A64RAV-DSTACK TTHROWSQ
   s" and so is a place further above the base than an access can reach back"
   T-LABEL
   [: DBL-DEEP ;] E-A64RAV-DSTACK TTHROWSQ ;

\ ---- and the place a BRANCH is taken from ------------------------------------
\ `: SELF ( n -- n ) SELF ;` is the smallest routine there is with a call in it,
\ and the smallest that can be entered at the wrong place. It hands its argument
\ straight on, so the cell the callee reads its argument out of is the cell this
\ routine's caller wrote, nothing is stored and nothing is loaded, and the four
\ places the routine requires - the caller's, the callee's argument base, the
\ callee's result base and the caller's again - are all one place. So it stands
\ there and every one of its adjustments is nothing: the routine is its frame,
\ its saved return address, one branch and its return.
\
\ WHAT MOVING THE SITE DOES. Add one cell to both of the site's distances and the
\ branch is taken with the pointer one cell above the callee's base. The callee
\ then reads its argument out of a cell this routine never wrote and leaves its
\ result in one this routine never publishes - and the module says so about
\ itself: the cells a branch publishes are the cells under where it is taken
\ from, and one of them is a cell no path of this routine has defined.
: BUILD-SELF ( n n -- )
   {: give:n back:n :}
   s" SELF" 1 1 OPEN-FUN
   A64EFF:SP-ALIGN M-RESERVE {: f0:IR-ID:ir-value-id :}
   f0 A64IR-OPCODE:LINKSAVE M-LINK {: f1:IR-ID:ir-value-id :}
   0 M-DTAKE {: t0:IR-ID:ir-value-id :}
   t0 give back M-CALL {: t1:IR-ID:ir-value-id :}
   t1 0 M-DPUBLISH
   f1 A64IR-OPCODE:LINKLOAD M-LINK {: f2:IR-ID:ir-value-id :}
   f2 A64EFF:SP-ALIGN M-RELEASE
   M-RET0
   CLOSE-FUN ;

: CALL-HABU-N ( n n n -- A64EFF:routine )
   {: n:n in:n out:n :}
   A64EFF-CONV:DSTACK in SLOTS-N  out SLOTS-N  n POOL-N
   A64EFF:FPR-NONE A64EFF:FPR-NONE A64EFF:FPR-NONE
   A64EFF-NZCV:UNTOUCHED A64EFF-LINK:PRESERVED A64EFF-CONTROL:RETURNS
   A64EFF:T-CALL A64EFF:SP-ALIGN 0 A64EFF:ROUTINE ;

: SELF-ACCEPT ( n n -- )
   {: give:n back:n :}
   give back BUILD-SELF
   M-FREEZE {: m:IR-BUILD:module :}
   CC m  4 1 1 CALL-HABU-N  A64RA:ALLOCATE
   m  4 1 1 CALL-HABU-N  A64RAV:ACCEPT ;

: SELF-CANON-BODY ( IR-CTX:ctx -- bool )
   A64-MOD
   0 0 SELF-ACCEPT
   A64RAV:ACCEPTED? ;

: SELF-HIGH-BODY ( IR-CTX:ctx -- )
   A64-MOD
   A64IR:SLOT-WIDTH A64IR:SLOT-WIDTH SELF-ACCEPT ;

: SELF-CANON ( -- bool )
   WBND [: SELF-CANON-BODY ;] IR-CTX:WITH-CONTEXT ;

: SELF-HIGH ( -- )
   WBND [: SELF-HIGH-BODY ;] IR-CTX:WITH-CONTEXT ;

: CALL-PLACE-ACCEPT-CASE ( -- )
   s" a call whose callee's base is where the routine already stands costs nothing"
   T-LABEL
   SELF-CANON TTRUE ;

: CALL-PLACE-REFUSE-CASE ( -- )
   s" a branch taken from anywhere but the callee's base is refused" T-LABEL
   [: SELF-HIGH ;] E-A64RAV-DRES TTHROWSQ ;

\ ---- one routine computing in both register files -----------------------------
\ WHAT THIS IS FOR. A register is a file and a number, so x0 and d0 are two
\ registers that are both number zero, and two values holding them are not
\ sharing anything. Everything above this point declares no floating register at
\ all, so nothing in this file ever asked the allocator for the second file or
\ watched the verifier judge an assignment that uses both. This one does: its
\ cell argument and its first double are alive at the same instant and are both
\ given register zero, which is the pattern a check that compared REGISTER
\ NUMBERS without their files would refuse, and its two doubles are alive at the
\ same instant and are given two different numbers, which is the pattern such a
\ check would let through if it compared nothing at all.
\
\ AND IT IS WHERE THE MUTATION LANDS. The one pattern this cannot build is two
\ values of two CLASSES in one file - this dialect has one class per file - so
\ that half of A64RAV:OVERLAP-CK is proved the way the class rule's own clauses
\ are, by mutating the compiler and watching the gate: see the note above
\ OVERLAP-CK in src/compiler/native/regalloc-verify.f.
: FPOOL-N ( n -- A64EFF:fprs )
   {: n:n :}
   A64EFF:FPR-NONE
   n 0 ?do i A64EFF:FPR-REG A64EFF:FPR-WITH loop ;

\ A leaf that may write both files: `n` general registers and `fn` floating ones.
: LEAF-FN ( n n -- A64EFF:routine )
   {: n:n fn:n :}
   A64EFF-CONV:REGISTER A64EFF:SEQ-NONE A64EFF:SEQ-NONE n POOL-N
   A64EFF:FPR-NONE A64EFF:FPR-NONE fn FPOOL-N
   A64EFF-NZCV:UNTOUCHED A64EFF-LINK:PRESERVED A64EFF-CONTROL:RETURNS
   A64EFF:TRAITS-NONE 0 0 A64EFF:ROUTINE ;

: M-FRESULT+ ( -- )
   CC BB  CC BB A64IR:FPR-TYPE  IR-BUILD:ADD-RESULT ;

: M-FMOVXD ( IR-ID:ir-value-id -- IR-ID:ir-value-id )
   {: x:IR-ID:ir-value-id :}
   A64IR-OPCODE:FMOVXD M-OPEN
   CC BB x IR-BUILD:ADD-OPERAND
   M-FRESULT+
   CLOSE-VALUE ;

: M-FADD ( IR-ID:ir-value-id IR-ID:ir-value-id -- IR-ID:ir-value-id )
   {: x:IR-ID:ir-value-id y:IR-ID:ir-value-id :}
   A64IR-OPCODE:FADD M-OPEN
   CC BB x IR-BUILD:ADD-OPERAND
   CC BB y IR-BUILD:ADD-OPERAND
   M-FRESULT+
   CLOSE-VALUE ;

: M-FMOVDX ( IR-ID:ir-value-id -- IR-ID:ir-value-id )
   {: d:IR-ID:ir-value-id :}
   A64IR-OPCODE:FMOVDX M-OPEN
   CC BB d IR-BUILD:ADD-OPERAND
   M-RESULT+
   CLOSE-VALUE ;

: M-FCVTZS ( IR-ID:ir-value-id -- IR-ID:ir-value-id )
   {: d:IR-ID:ir-value-id :}
   A64IR-OPCODE:FCVTZS M-OPEN
   CC BB d IR-BUILD:ADD-OPERAND
   M-RESULT+
   CLOSE-VALUE ;

\ The cell argument is read a second time AFTER the first double exists, which is
\ what makes the two alive together rather than one after the other.
: BUILD-TWO-FILES ( -- )
   s" BOTHF" 1 1 OPEN-FUN
   ARG+ {: a:IR-ID:ir-value-id :}
   a M-FMOVXD {: p:IR-ID:ir-value-id :}
   a M-FMOVXD {: q:IR-ID:ir-value-id :}
   p q M-FADD {: s:IR-ID:ir-value-id :}
   s M-FMOVDX M-RET
   CLOSE-FUN ;

: TWO-FILES-BODY ( IR-CTX:ctx -- n n n bool bool n n bool )
   A64-MOD
   BUILD-TWO-FILES
   M-FREEZE {: m:IR-BUILD:module :}
   CC m  2 2 LEAF-FN  A64RA:ALLOCATE
   m  2 2 LEAF-FN  A64RAV:ACCEPT
   0 A64RAV:REG@
   1 A64RAV:REG@
   2 A64RAV:REG@
   0 A64RAV:FLOATING?
   1 A64RAV:FLOATING?
   A64RAV:GPR-WRITTEN A64EFF:GPRS-N
   A64RAV:FPR-WRITTEN A64EFF:FPRS-N
   A64RAV:ACCEPTED? ;

: TWO-FILES-CASE ( -- )
   s" a cell and a double alive together hold register zero of each file" T-LABEL
   WBND [: TWO-FILES-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE 3 T= 1 T= TTRUE TFALSE 1 T= 0 T= 0 T= ;

\ ---- putting a DOUBLE away ---------------------------------------------------
\ The whole spill route again over a value of the floating file: allocate a
\ program that does not fit, lower the decisions into operations, and allocate
\ the module that holds them.
\
\ WHY A FIXTURE HAS TO DECLARE A SMALL FLOATING FILE FOR THIS TO BE REACHABLE AT
\ ALL. A routine contract hands out the WHOLE floating file
\ (src/compiler/native/abi.f), so a body the chain compiles runs short of D
\ registers only if it holds more than thirty-two doubles at once - which no row
\ of any pinned corpus does. Declaring two is what puts the wall where a fixture
\ can reach it, and it changes nothing about the pass under test: the plan and
\ the lowering read the pool they are given.
\
\ WHAT MAKES THIS MORE THAN "IT DID NOT THROW", AND IT IS THE BUILD ITSELF. The
\ operations the lowering writes carry the spilled value as an operand and its
\ reload as a result, and IR-BUILD checks both against the schema of the opcode
\ they are given. a64.str declares a GENERAL operand and a64.ldr a general
\ result, so putting a double away with the general pair does not build - the
\ file the eight bytes travel in is checked by the substrate rather than by a
\ name test the pass makes about itself. Point src/compiler/native/spill.f's
\ STORE-FORM at a64.str and this case is red where it stands.
\
\ THE ROUTINE STILL ANSWERS A CELL. The conversion at the end of the fixture
\ takes the sum into the general file and the return carries that, so nothing
\ here asserts where a double LEAVES a routine - only the frame round trip in
\ the middle, which is what the two forms are for.
: BUILD-FCHAIN ( -- )
   s" FCHAIN" 0 1 OPEN-FUN
   $11 M-MOVZ M-FMOVXD {: a:IR-ID:ir-value-id :}
   $22 M-MOVZ M-FMOVXD {: b:IR-ID:ir-value-id :}
   $33 M-MOVZ M-FMOVXD {: c:IR-ID:ir-value-id :}
   b c M-FADD {: s1:IR-ID:ir-value-id :}
   s1 a M-FADD M-FCVTZS M-RET
   CLOSE-FUN ;

\ LEAF-FN with a frame, because a routine that spills has to have somewhere to
\ spill to.
: FLEAF-FRAMED ( n n n -- A64EFF:routine )
   {: n:n fn:n size:n :}
   A64EFF-CONV:REGISTER A64EFF:SEQ-NONE A64EFF:SEQ-NONE n POOL-N
   A64EFF:FPR-NONE A64EFF:FPR-NONE fn FPOOL-N
   A64EFF-NZCV:UNTOUCHED A64EFF-LINK:PRESERVED A64EFF-CONTROL:RETURNS
   A64EFF:TRAITS-NONE size 0 A64EFF:ROUTINE ;

: FSPILL-CONTRACT ( -- A64EFF:routine )
   4 2 16 FLEAF-FRAMED ;

\ Three doubles are live where the third is written and two registers hold them,
\ so one has to go into the frame - and it is the first, which the last addition
\ reads furthest away.
: FSPILL-PLAN-BODY ( IR-CTX:ctx -- n n bool )
   A64-MOD
   SPILL-BIND
   BUILD-FCHAIN
   M-FREEZE {: m0:IR-BUILD:module :}
   CC m0 FSPILL-CONTRACT A64RA:ALLOCATE
   A64SPILL:RELEASE
   A64RA:SPILLS
   A64RA:PLAN-N
   0 A64RA:PLAN-STORE? ;

: FSPILL-PLAN-CASE ( -- )
   s" a double that does not fit is planned into the frame" T-LABEL
   WBND [: FSPILL-PLAN-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE 2 T= 1 T= ;

: FSPILL-LOWER-BODY ( IR-CTX:ctx -- n n bool )
   A64-MOD
   SPILL-BIND
   BUILD-FCHAIN
   M-FREEZE {: m0:IR-BUILD:module :}
   CC m0 FSPILL-CONTRACT A64RA:ALLOCATE
   A64-BUILDER {: nb:IR-BUILD:builder :}
   CC nb A64RA:BIND-DIALECT
   CC nb A64RAV:BIND-DIALECT
   CC m0 nb TXT TXT-N A64SPILL:REWRITE {: m1:IR-BUILD:module :}
   CC m1 FSPILL-CONTRACT A64RA:ALLOCATE
   m1 FSPILL-CONTRACT A64RAV:ACCEPT
   A64RA:SPILLS
   A64RA:VALUES
   A64RAV:ACCEPTED? ;

: FSPILL-LOWER-CASE ( -- )
   s" a lowered double spill allocates and is accepted" T-LABEL
   WBND [: FSPILL-LOWER-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE 13 T= 0 T= ;

\ ---- groups ------------------------------------------------------------------
: GROUP-PLACE-ACCEPT ( IR-CTX:ctx -- ) drop PLACE-ACCEPT-CASE ;
: GROUP-PLACE-MOVED ( IR-CTX:ctx -- ) drop PLACE-MISPLACED-CASES ;
: GROUP-PLACE-CANON ( IR-CTX:ctx -- ) drop PLACE-CANON-CASES ;
: GROUP-CALL-PLACE ( IR-CTX:ctx -- ) drop CALL-PLACE-ACCEPT-CASE ;
: GROUP-CALL-PLACE-BAD ( IR-CTX:ctx -- ) drop CALL-PLACE-REFUSE-CASE ;
: GROUP-SHAPE ( IR-CTX:ctx -- )     drop SHAPE-REFUSE-CASES ;
: GROUP-TIE ( IR-CTX:ctx -- )       drop TIE-REFUSE-CASES ;
: GROUP-PRESSURE ( IR-CTX:ctx -- )  drop PRESSURE-REFUSE-CASES ;
: GROUP-DERIVE ( IR-CTX:ctx -- )    drop DERIVE-FRAME-CASES ;
: GROUP-POOL ( IR-CTX:ctx -- )      drop POOL-REFUSE-CASES ;
: GROUP-FIXED ( IR-CTX:ctx -- )     drop FIXED-REFUSE-CASES ;
: GROUP-CROSS ( IR-CTX:ctx -- )     drop CROSS-REFUSE-CASE ;
: GROUP-PLACE ( IR-CTX:ctx -- )     drop PLACE-REFUSE-CASES ;
: GROUP-FIXED-ACCEPT ( IR-CTX:ctx -- ) drop FIXED-ACCEPT-CASES ;
: GROUP-LOWER ( IR-CTX:ctx -- )     drop LOWER-TWICE-CASE ;
: GROUP-NO-SPILL ( IR-CTX:ctx -- )  drop LOWER-NONE-CASE ;
: GROUP-SLOT ( IR-CTX:ctx -- )      drop SLOT-REFUSE-CASES ;
: GROUP-ORDER ( IR-CTX:ctx -- )     drop ORDER-REFUSE-CASES ;
: GROUP-RELOAD ( IR-CTX:ctx -- )    drop RELOAD-REFUSE-CASES ;
: GROUP-BIND ( IR-CTX:ctx -- )      drop BIND-REFUSE-CASES ;
: GROUP-MODULE ( IR-CTX:ctx -- )    drop MODULE-REFUSE-CASES ;
: GROUP-TARGET ( IR-CTX:ctx -- )    drop TARGET-REFUSE-CASES ;
: GROUP-ACCEPT ( IR-CTX:ctx -- )    drop ACCEPT-REFUSE-CASES ;
: GROUP-STATE ( IR-CTX:ctx -- )     drop STATE-REFUSE-CASES ;
: GROUP-MB ( IR-CTX:ctx -- )        drop MB-REFUSE-CASES ;
: GROUP-ROUND ( IR-CTX:ctx -- )     drop ROUND-FRAME-CASES ;
: GROUP-MB-CARRIED ( IR-CTX:ctx -- ) drop MB-CARRIED-REFUSE-CASE ;
: GROUP-MB-ACCEPT ( IR-CTX:ctx -- ) drop MB-ACCEPT-REFUSE-CASES ;

public

: RUN ( -- )
   T-RESET
   SQUARE-CASE
   MOVE-PLAN-CASE
   MOVE-LOWER-CASE
   DECL-SPILL-CASE
   DECL-DIFF-CASE
   DECL-HIGH-CASE
   PLAN-CASE
   TIE-SPILL-CASE
   DOUBLE-CASE
   LOWER-CASE
   DIFF-CASE
   SUM3-CASE
   SUM3-TIGHT-CASE
   SUM3-HIGH-CASE
   REUSE-CASE
   UNUSED-CASE
   WIDE-CASE
   PLAIN-CASE
   TWO-FUNS-CASE
   TWO-FILES-CASE
   FSPILL-PLAN-CASE
   FSPILL-LOWER-CASE
   INTERLEAVED-CASE
   TIED-EXTRA-CASE
   UNTIED-EXTRA-CASE
   PAIR-CASE
   MB-EDGE-CASE
   MB-LOOP-CASE
   MB-LOOP-HULL-CASE
   MB-TIE-CASE
   MB-COPY-CASE
   MB-LIVE-COPY-CASE
   MB-PLAN-CASE
   MB-LOWER-CASE
   MB-FIXED-CASE
   RESERVED-CASES
   WBND [: GROUP-PLACE-ACCEPT ;] IR-CTX:WITH-CONTEXT
   WBND [: GROUP-PLACE-MOVED ;] IR-CTX:WITH-CONTEXT
   WBND [: GROUP-PLACE-CANON ;] IR-CTX:WITH-CONTEXT
   WBND [: GROUP-CALL-PLACE ;] IR-CTX:WITH-CONTEXT
   WBND [: GROUP-CALL-PLACE-BAD ;] IR-CTX:WITH-CONTEXT
   WBND [: GROUP-SHAPE ;] IR-CTX:WITH-CONTEXT
   WBND [: GROUP-TIE ;] IR-CTX:WITH-CONTEXT
   WBND [: GROUP-PRESSURE ;] IR-CTX:WITH-CONTEXT
   WBND [: GROUP-DERIVE ;] IR-CTX:WITH-CONTEXT
   WBND [: GROUP-ROUND ;] IR-CTX:WITH-CONTEXT
   WBND [: GROUP-POOL ;] IR-CTX:WITH-CONTEXT
   WBND [: GROUP-FIXED ;] IR-CTX:WITH-CONTEXT
   WBND [: GROUP-CROSS ;] IR-CTX:WITH-CONTEXT
   WBND [: GROUP-PLACE ;] IR-CTX:WITH-CONTEXT
   WBND [: GROUP-FIXED-ACCEPT ;] IR-CTX:WITH-CONTEXT
   WBND [: GROUP-LOWER ;] IR-CTX:WITH-CONTEXT
   WBND [: GROUP-NO-SPILL ;] IR-CTX:WITH-CONTEXT
   WBND [: GROUP-SLOT ;] IR-CTX:WITH-CONTEXT
   WBND [: GROUP-ORDER ;] IR-CTX:WITH-CONTEXT
   WBND [: GROUP-RELOAD ;] IR-CTX:WITH-CONTEXT
   WBND [: GROUP-BIND ;] IR-CTX:WITH-CONTEXT
   WBND [: GROUP-MODULE ;] IR-CTX:WITH-CONTEXT
   WBND [: GROUP-TARGET ;] IR-CTX:WITH-CONTEXT
   WBND [: GROUP-ACCEPT ;] IR-CTX:WITH-CONTEXT
   WBND [: GROUP-STATE ;] IR-CTX:WITH-CONTEXT
   WBND [: GROUP-MB ;] IR-CTX:WITH-CONTEXT

   WBND [: GROUP-MB-CARRIED ;] IR-CTX:WITH-CONTEXT
   WBND [: GROUP-MB-ACCEPT ;] IR-CTX:WITH-CONTEXT
   T-REPORT ;

;package

A64RA-TEST:RUN
