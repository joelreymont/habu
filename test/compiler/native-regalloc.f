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
\ function, an operation of a form outside the family, a move-wide overwrite
\ whose kept value is read again - and the allocator must still refuse them.
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
   A64EFF:GPR-NONE A64EFF:GPR-NONE pool
   A64EFF:FPR-NONE A64EFF:FPR-NONE A64EFF:FPR-NONE
   A64EFF-NZCV:UNTOUCHED A64EFF-LINK:PRESERVED A64EFF-CONTROL:RETURNS
   A64EFF:TRAITS-NONE 0 0 A64EFF:ROUTINE ;

: LEAF-N ( n -- A64EFF:routine )
   POOL-N LEAF ;

\ `n` registers starting at `base`. A pool that does not start at register zero
\ is what tells the allocatable set apart from the low registers: nothing may be
\ handed out because it happened to be free, only because the contract named it.
: POOL-FROM ( n n -- A64EFF:gprs )
   {: base:n n:n :}
   A64EFF:GPR-NONE
   n 0 ?do base i + A64EFF:GPR-REG A64EFF:GPR-WITH loop ;

: LEAF-FROM ( n n -- A64EFF:routine )
   POOL-FROM LEAF ;

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
   CC m ab TXT TXT-N A64SEL:SELECT ;

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

\ A seventh machine operation, defined into this dialect's own table. Nothing in
\ the substrate forbids it and the module verifies, so the allocator has to
\ refuse it by name rather than by never meeting it: an unmodelled form may tie
\ its registers the way the move-wide overwrite does.
: EXTRA-SCHEMA ( -- IR-ID:ir-symbol-id )
   CC BB s" a64.neg" IR-BUILD:INTERN-SYMBOL {: op:IR-ID:ir-symbol-id :}
   op IR-SCHEMA:BEGIN-OP
   CC BB A64IR:GPR-TYPE IR-SCHEMA:ADD-OPERAND
   CC BB A64IR:GPR-TYPE IR-SCHEMA:ADD-RESULT
   false 0 0 IR-SCHEMA:SET-CONTROL
   IR-SCHEMA:SET-PURE
   false IR-SCHEMA:SET-TRAP
   CTARGET-ARCH:AARCH64 CTARGET:F-BASE IR-SCHEMA:SET-TARGET
   CC BB s" a64.rule.neg" IR-BUILD:INTERN-SYMBOL IR-SCHEMA:SET-RULE
   CC BB s" a64.render.neg" IR-BUILD:INTERN-SYMBOL IR-SCHEMA:SET-RENDERER
   CC BB IR-BUILD:DEFINE-OP
   op ;

: BUILD-EXTRA ( -- )
   EXTRA-SCHEMA {: op:IR-ID:ir-symbol-id :}
   s" NEG" 0 1 OPEN-FUN
   7 M-MOVZ {: v:IR-ID:ir-value-id :}
   CC BB op IR-BUILD:BEGIN-OP
   CC BB  BODY-ST BODY-LN SPN  IR-BUILD:SET-OP-SPAN
   CC BB v IR-BUILD:ADD-OPERAND
   M-RESULT+
   CLOSE-VALUE M-RET
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

: EXTRA-OPCODE-BODY ( IR-CTX:ctx -- )
   A64-MOD
   BUILD-EXTRA
   REFUSE-SHAPE ;

\ Two registers cannot hold three arguments at once, and a routine that may
\ destroy nothing cannot hold one.
: PRESSURE-BODY ( IR-CTX:ctx -- )
   HIR-MOD
   BUILD-SUM3
   SELECTED {: m:IR-BUILD:module :}
   CC m 2 LEAF-N A64RA:ALLOCATE ;

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
   CC hm ab TXT TXT-N A64SEL:SELECT {: m:IR-BUILD:module :}
   CC m 4 LEAF-N A64RA:ALLOCATE
   hm 4 LEAF-N A64RAV:ACCEPT ;

: ACCEPT-WRONG-POOL-BODY ( IR-CTX:ctx -- )
   A64-MOD
   BUILD-PLAIN
   4 M-ALLOCATE {: m:IR-BUILD:module :}
   m 3 LEAF-N A64RAV:ACCEPT ;

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
   s" the link register cannot be declared destroyable" T-LABEL
   [: A64EFF:LINK-GPR A64EFF:GPR-REG drop ;] E-A64EFF-GPR TTHROWSQ
   s" the zero register cannot be declared destroyable" T-LABEL
   [: A64EFF:ZERO-GPR A64EFF:GPR-REG drop ;] E-A64EFF-GPR TTHROWSQ ;

\ ---- refusal cases -----------------------------------------------------------
: LIVE-TIE ( -- )         WBND [: LIVE-TIE-BODY ;] IR-CTX:WITH-CONTEXT ;
: WRONG-CLASS ( -- )      WBND [: WRONG-CLASS-BODY ;] IR-CTX:WITH-CONTEXT ;
: TWO-FUNS ( -- )         WBND [: TWO-FUNS-BODY ;] IR-CTX:WITH-CONTEXT ;
: EXTRA-OPCODE ( -- )     WBND [: EXTRA-OPCODE-BODY ;] IR-CTX:WITH-CONTEXT ;
: PRESSURE ( -- )         WBND [: PRESSURE-BODY ;] IR-CTX:WITH-CONTEXT ;
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
   [: TWO-FUNS ;] E-A64RA-SHAPE TTHROWSQ
   s" an operation of a form outside the dialect's family is refused" T-LABEL
   [: EXTRA-OPCODE ;] E-A64RA-OPCODE TTHROWSQ ;

: TIE-REFUSE-CASES ( -- )
   s" a move-wide overwrite whose kept value is read again is refused" T-LABEL
   [: LIVE-TIE ;] E-A64RA-TIE TTHROWSQ ;

: PRESSURE-REFUSE-CASES ( -- )
   s" more values live at once than the routine may destroy is refused" T-LABEL
   [: PRESSURE ;] E-A64RA-PRESSURE TTHROWSQ
   s" a routine that may destroy nothing allocates nothing" T-LABEL
   [: EMPTY-POOL ;] E-A64RA-PRESSURE TTHROWSQ
   \ The refusal just above left no sealed walk, so there is no claim to read.
   s" a refused allocation leaves no claim behind" T-LABEL
   [: 0 A64RA:CLAIM@ drop ;] E-A64RA-STATE TTHROWSQ ;

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
: GROUP-BIND ( IR-CTX:ctx -- )      drop BIND-REFUSE-CASES ;
: GROUP-MODULE ( IR-CTX:ctx -- )    drop MODULE-REFUSE-CASES ;
: GROUP-TARGET ( IR-CTX:ctx -- )    drop TARGET-REFUSE-CASES ;
: GROUP-ACCEPT ( IR-CTX:ctx -- )    drop ACCEPT-REFUSE-CASES ;
: GROUP-STATE ( IR-CTX:ctx -- )     drop STATE-REFUSE-CASES ;

public

: RUN ( -- )
   T-RESET
   SQUARE-CASE
   DIFF-CASE
   SUM3-CASE
   SUM3-TIGHT-CASE
   SUM3-HIGH-CASE
   REUSE-CASE
   UNUSED-CASE
   WIDE-CASE
   PLAIN-CASE
   INTERLEAVED-CASE
   RESERVED-CASES
   WBND [: GROUP-SHAPE ;] IR-CTX:WITH-CONTEXT
   WBND [: GROUP-TIE ;] IR-CTX:WITH-CONTEXT
   WBND [: GROUP-PRESSURE ;] IR-CTX:WITH-CONTEXT
   WBND [: GROUP-BIND ;] IR-CTX:WITH-CONTEXT
   WBND [: GROUP-MODULE ;] IR-CTX:WITH-CONTEXT
   WBND [: GROUP-TARGET ;] IR-CTX:WITH-CONTEXT
   WBND [: GROUP-ACCEPT ;] IR-CTX:WITH-CONTEXT
   WBND [: GROUP-STATE ;] IR-CTX:WITH-CONTEXT
   T-REPORT ;

;package

A64RA-TEST:RUN
