\ native-regalloc.f - checked register-allocation tests.
\
\ Proves the contract of src/compiler/native/regalloc.f and its validator
\ src/compiler/native/regalloc-verify.f: a frozen straight-line A64IR module gets
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
   A64EFF:SEQ-NONE A64EFF:SEQ-NONE pool
   A64EFF:FPR-NONE A64EFF:FPR-NONE A64EFF:FPR-NONE
   A64EFF-NZCV:UNTOUCHED A64EFF-LINK:PRESERVED A64EFF-CONTROL:RETURNS
   A64EFF:TRAITS-NONE 0 0 A64EFF:ROUTINE ;

: LEAF-N ( n -- A64EFF:routine )
   POOL-N LEAF ;

\ The same leaf with a frame of its own: a routine that spills has to have
\ somewhere to spill to, and how deep that is, is the contract's declaration.
: LEAF-FRAMED ( n n -- A64EFF:routine )
   {: n:n size:n :}
   n POOL-N {: pool:A64EFF:gprs :}
   A64EFF:SEQ-NONE A64EFF:SEQ-NONE pool
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
: LEAF-DECL ( A64EFF:gprs A64EFF:placeseq A64EFF:placeseq -- A64EFF:routine )
   {: pool:A64EFF:gprs args:A64EFF:placeseq outs:A64EFF:placeseq :}
   args outs
   pool outs A64EFF:SEQ-SET A64EFF:GPR-WITHOUT
   A64EFF:FPR-NONE A64EFF:FPR-NONE A64EFF:FPR-NONE
   A64EFF-NZCV:UNTOUCHED A64EFF-LINK:PRESERVED A64EFF-CONTROL:RETURNS
   A64EFF:TRAITS-NONE 0 0 A64EFF:ROUTINE ;

\ The same with a frame, for a program that declares an interface AND spills.
: LEAF-DECL-FRAMED ( A64EFF:gprs A64EFF:placeseq A64EFF:placeseq -- A64EFF:routine )
   {: pool:A64EFF:gprs args:A64EFF:placeseq outs:A64EFF:placeseq :}
   args outs
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
   TTRUE 0 T= 0 T= 0 T= 2 T= ;

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
   1 T= 1 T= 0 T= 0 T= 0 T= 2 T= ;

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
   -1 T= -1 T= 1 T= 0 T= 2 T= ;

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
   CC m 4 POOL-N 2 0 SQ2 1 SQ LEAF-DECL A64RA:ALLOCATE
   m 4 POOL-N 2 0 SQ2 1 SQ LEAF-DECL A64RAV:ACCEPT
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
      0 1 2 SQ3  0 SQ LEAF-DECL A64RA:ALLOCATE
   m  4 4 POOL-FROM 0 1 2 SQ3 A64EFF:SEQ-SET A64EFF:GPR-WITH
      0 1 2 SQ3  0 SQ LEAF-DECL A64RAV:ACCEPT
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

\ Two functions in one module: whichever one an allocation was about, it was not
\ about both.
: BUILD-TWO-FUNS ( -- )
   s" ONE" 0 1 OPEN-FUN
   7 M-MOVZ M-RET
   CLOSE-FUN
   s" TWO" 0 1 OPEN-FUN
   9 M-MOVZ M-RET
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
   4 POOL-N  0 1 SQ2  1 SQ  LEAF-DECL ;

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
   1 T= 0 T= 1 T= ;

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

\ Which of two equally distant values loses its register. Both are read by the
\ same operation, so the cost rule cannot separate them and the tie rule does:
\ the first plan row names the value in the lower register.
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
   2 T= 0 T= 2 T= ;

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
   2 T= 0 T= 2 T= 1 T= ;

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

: TWO-FUNS-BODY ( IR-CTX:ctx -- )
   A64-MOD
   BUILD-TWO-FUNS
   REFUSE-SHAPE ;

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
   3 POOL-N  A64EFF:SEQ-NONE  1 SQ  LEAF-DECL-FRAMED ;

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
   CC m  2 POOL-N  A64EFF:SEQ-NONE  0 1 SQ2 LEAF-DECL A64RA:ALLOCATE ;

\ ---- fixed constraints an allocation cannot honour ---------------------------
\ A declared argument register the routine may not write. The pool is x0 to x3
\ and the convention says the first argument arrives in x5, which this routine
\ has promised to preserve: it could not be held there at all.
: ARG-OUT-OF-POOL-BODY ( IR-CTX:ctx -- )
   A64-MOD
   BUILD-KEEP
   M-FREEZE {: m:IR-BUILD:module :}
   CC m  4 POOL-N  5 0 SQ2  A64EFF:SEQ-NONE LEAF-DECL A64RA:ALLOCATE ;

\ A convention that names three argument positions for a routine that has two.
: OVER-ARG-BODY ( IR-CTX:ctx -- )
   A64-MOD
   BUILD-KEEP
   M-FREEZE {: m:IR-BUILD:module :}
   CC m  4 POOL-N  0 1 2 SQ3  A64EFF:SEQ-NONE LEAF-DECL A64RA:ALLOCATE ;

\ And one that names two returned values for a routine that returns one.
: OVER-OUT-BODY ( IR-CTX:ctx -- )
   A64-MOD
   BUILD-KEEP
   M-FREEZE {: m:IR-BUILD:module :}
   CC m  4 POOL-N  A64EFF:SEQ-NONE  0 1 SQ2 LEAF-DECL A64RA:ALLOCATE ;

\ ---- data-stack places the allocation cannot honour -------------------------
\ A convention declaring an argument in a data-stack slot describes a module the
\ selector already turned that place into a load in - so the block has no
\ argument for it. Handed a module that still carries its arguments as block
\ arguments, this allocation would leave them in registers no caller ever wrote
\ to, and it refuses instead. The second case is a side that names a register
\ place and a data-stack place at once, which nothing in the chain can pair with
\ a module's arguments.
: DSLOT-Q ( n -- A64EFF:placeseq )
   A64EFF:SEQ-NONE swap A64EFF:SEQ-WITH-SLOT ;

: UNLOWERED-BODY ( IR-CTX:ctx -- )
   A64-MOD
   BUILD-KEEP
   M-FREEZE {: m:IR-BUILD:module :}
   CC m  4 POOL-N  0 DSLOT-Q  A64EFF:SEQ-NONE LEAF-DECL A64RA:ALLOCATE ;

: MIXED-PLACE-BODY ( IR-CTX:ctx -- )
   A64-MOD
   BUILD-KEEP
   M-FREEZE {: m:IR-BUILD:module :}
   CC m  4 POOL-N  0 DSLOT-Q 1 A64EFF:SEQ-WITH  A64EFF:SEQ-NONE
   LEAF-DECL A64RA:ALLOCATE ;

\ Two registers cannot hold three arguments at once, and the routine declares no
\ frame, so there is nowhere to put the third: the pressure refusal that is left
\ is the frame running out of slots.
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
   m  4 POOL-N  1 0 SQ2  A64EFF:SEQ-NONE LEAF-DECL A64RAV:ACCEPT ;

: ACCEPT-WRONG-OUT-BODY ( IR-CTX:ctx -- )
   A64-MOD
   BUILD-KEEP
   4 M-ALLOCATE {: m:IR-BUILD:module :}
   m  4 POOL-N  A64EFF:SEQ-NONE  1 SQ LEAF-DECL A64RAV:ACCEPT ;

: ACCEPT-OVER-ARG-BODY ( IR-CTX:ctx -- )
   A64-MOD
   BUILD-KEEP
   4 M-ALLOCATE {: m:IR-BUILD:module :}
   m  4 POOL-N  0 1 2 SQ3  A64EFF:SEQ-NONE LEAF-DECL A64RAV:ACCEPT ;

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
: TWO-FUNS ( -- )         WBND [: TWO-FUNS-BODY ;] IR-CTX:WITH-CONTEXT ;
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

: DROP-BINDING ( -- )
   A64RA:RELEASE ;

: SHAPE-REFUSE-CASES ( -- )
   s" a value that is not a general register of this dialect is refused" T-LABEL
   [: WRONG-CLASS ;] E-A64RA-CLASS TTHROWSQ
   s" a module of more than one function is refused" T-LABEL
   [: TWO-FUNS ;] E-A64RA-SHAPE TTHROWSQ ;

: TIE-REFUSE-CASES ( -- )
   s" a move-wide overwrite whose kept value is read again is refused" T-LABEL
   [: LIVE-TIE ;] E-A64RA-TIE TTHROWSQ
   s" a tied operand of any form, read again, is refused the same way" T-LABEL
   [: EXTRA-LIVE-TIE ;] E-A64RA-TIE TTHROWSQ
   s" one value as two tied operands of one operation is refused" T-LABEL
   [: PAIR-SHARED ;] E-A64RA-TIE TTHROWSQ ;

: PRESSURE-REFUSE-CASES ( -- )
   s" more values live at once than a routine with no frame can put away" T-LABEL
   [: PRESSURE ;] E-A64RA-PRESSURE TTHROWSQ
   s" a frame one slot short of what the spills need is refused" T-LABEL
   [: SMALL-FRAME ;] E-A64RA-PRESSURE TTHROWSQ
   \ The refusal just above left no sealed walk, so there is no claim to read.
   s" a refused allocation leaves no claim behind" T-LABEL
   [: 0 A64RA:CLAIM@ drop ;] E-A64RA-STATE TTHROWSQ ;

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

: MIXED-PLACE ( -- )
   WBND [: MIXED-PLACE-BODY ;] IR-CTX:WITH-CONTEXT ;

: PLACE-REFUSE-CASES ( -- )
   s" data-stack places on a module that still carries block arguments are refused" T-LABEL
   [: UNLOWERED ;] E-A64RA-PLACE TTHROWSQ
   s" a side mixing register places with data-stack places is refused" T-LABEL
   [: MIXED-PLACE ;] E-A64RA-PLACE TTHROWSQ ;

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

: STATE-REFUSE-CASES ( -- )
   s" a claim no validator has accepted is not an answer" T-LABEL
   [: UNCHECKED ;] E-A64RAV-STATE TTHROWSQ
   s" an accepted answer stops answering when a later walk replaces it" T-LABEL
   [: STALE ;] E-A64RAV-STATE TTHROWSQ ;

\ ---- groups ------------------------------------------------------------------
: GROUP-SHAPE ( IR-CTX:ctx -- )     drop SHAPE-REFUSE-CASES ;
: GROUP-TIE ( IR-CTX:ctx -- )       drop TIE-REFUSE-CASES ;
: GROUP-PRESSURE ( IR-CTX:ctx -- )  drop PRESSURE-REFUSE-CASES ;
: GROUP-POOL ( IR-CTX:ctx -- )      drop POOL-REFUSE-CASES ;
: GROUP-FIXED ( IR-CTX:ctx -- )     drop FIXED-REFUSE-CASES ;
: GROUP-CROSS ( IR-CTX:ctx -- )     drop CROSS-REFUSE-CASE ;
: GROUP-PLACE ( IR-CTX:ctx -- )     drop PLACE-REFUSE-CASES ;
: GROUP-FIXED-ACCEPT ( IR-CTX:ctx -- ) drop FIXED-ACCEPT-CASES ;
: GROUP-LOWER ( IR-CTX:ctx -- )     drop LOWER-TWICE-CASE ;
: GROUP-NO-SPILL ( IR-CTX:ctx -- )  drop LOWER-NONE-CASE ;
: GROUP-SLOT ( IR-CTX:ctx -- )      drop SLOT-REFUSE-CASES ;
: GROUP-RELOAD ( IR-CTX:ctx -- )    drop RELOAD-REFUSE-CASES ;
: GROUP-BIND ( IR-CTX:ctx -- )      drop BIND-REFUSE-CASES ;
: GROUP-MODULE ( IR-CTX:ctx -- )    drop MODULE-REFUSE-CASES ;
: GROUP-TARGET ( IR-CTX:ctx -- )    drop TARGET-REFUSE-CASES ;
: GROUP-ACCEPT ( IR-CTX:ctx -- )    drop ACCEPT-REFUSE-CASES ;
: GROUP-STATE ( IR-CTX:ctx -- )     drop STATE-REFUSE-CASES ;

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
   INTERLEAVED-CASE
   TIED-EXTRA-CASE
   UNTIED-EXTRA-CASE
   PAIR-CASE
   RESERVED-CASES
   WBND [: GROUP-SHAPE ;] IR-CTX:WITH-CONTEXT
   WBND [: GROUP-TIE ;] IR-CTX:WITH-CONTEXT
   WBND [: GROUP-PRESSURE ;] IR-CTX:WITH-CONTEXT
   WBND [: GROUP-POOL ;] IR-CTX:WITH-CONTEXT
   WBND [: GROUP-FIXED ;] IR-CTX:WITH-CONTEXT
   WBND [: GROUP-CROSS ;] IR-CTX:WITH-CONTEXT
   WBND [: GROUP-PLACE ;] IR-CTX:WITH-CONTEXT
   WBND [: GROUP-FIXED-ACCEPT ;] IR-CTX:WITH-CONTEXT
   WBND [: GROUP-LOWER ;] IR-CTX:WITH-CONTEXT
   WBND [: GROUP-NO-SPILL ;] IR-CTX:WITH-CONTEXT
   WBND [: GROUP-SLOT ;] IR-CTX:WITH-CONTEXT
   WBND [: GROUP-RELOAD ;] IR-CTX:WITH-CONTEXT
   WBND [: GROUP-BIND ;] IR-CTX:WITH-CONTEXT
   WBND [: GROUP-MODULE ;] IR-CTX:WITH-CONTEXT
   WBND [: GROUP-TARGET ;] IR-CTX:WITH-CONTEXT
   WBND [: GROUP-ACCEPT ;] IR-CTX:WITH-CONTEXT
   WBND [: GROUP-STATE ;] IR-CTX:WITH-CONTEXT
   T-REPORT ;

;package

A64RA-TEST:RUN
