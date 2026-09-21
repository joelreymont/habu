\ x64-regalloc.f - the first x86-64 register allocation.
\
\ src/compiler/native/regalloc.f (A64RA) and regalloc-verify.f (A64RAV) are one
\ allocator and one validator for the compiler rather than one per machine. What
\ they used to spell as ARM64's - the three value types, the attribute keys, the
\ copy and the re-emittable opcode, how many instructions an address literal is,
\ the bytes one frame access moves and the architecture the allocation is for -
\ now arrives in the vocabulary the dialect builds
\ (src/compiler/native/dialect.f). This suite is what proves the vocabulary is
\ ENOUGH: the same two passes run here over a module in which no ARM64 name
\ appears, and the ARM64 suite next door runs unchanged over the other.
\
\ WHAT THESE CASES MEASURE THAT test/compiler/native-regalloc.f CANNOT:
\
\ - THE ARCHITECTURE. The allocator refused any contract whose arch was not
\   AARCH64. It now refuses any contract that is not the bound dialect's own
\   machine, and an x86-64 contract allocates - which is this whole suite.
\ - THE MISSING KEY. x86-64 has no write-back addressing, so its vocabulary
\   declares `x64.dwb` ABSENT: no transfer moves the data-stack pointer inside
\   its own encoding and no symbol names one. Every data-stack case below runs
\   through the reader that has to answer "no such transfer" without a symbol.
\ - THE COPY OPCODE. A copy is `x64.mov` here and `a64.mov` there, and the
\   allocator coalesces by the vocabulary's opcode. The two-address case pins
\   registers on both sides of a copy the selector inserted.
\ - ONE-LANE ADDRESSES. `mov r64, imm64` is one instruction, so this dialect
\   declares one address-carrier lane and the allocator does no lane arithmetic;
\   ARM64's four move-wides must stay contiguous through spill insertion.
\
\ A REGISTER CONVENTION, which is the shape the ARM64 cases allocate under too:
\ the contract names no place, so the module is the body the source selected to
\ and every value in it is one the allocator is free to place.
\
\ AND X64ABI:LEAF, WHICH IS THE DATA-STACK CONVENTION. Where the data-stack
\ pointer STANDS over a body is the SELECTOR's policy and not something either
\ later pass can derive from the module: A64SEL surveys the boundary transfers
\ and stands where the fewest need an adjustment, X64SEL takes every argument's
\ bytes at the entry and publishes every result's at the exit. The dialect is
\ what states which of the two it is - the vocabulary's `stand` field, `survey`
\ for a64ir and `entry` for x64ir (dialect.f NDIALECT:dstand) - and
\ regalloc-verify.f VDPLACE-CK measures a module against the policy its dialect
\ states rather than re-deriving one of them. The leaf case below is that fact
\ measured.
\
\ ONE FIXTURE PER CONTEXT, and the refusing case runs inside an enclosing one:
\ an abandoned context gives its registry slots back only when a live enclosing
\ context leaves normally (src/compiler/ir/context.f, the note on stale handles).

require lib/test.f
require src/compiler/native/select-x64.f
require src/compiler/native/regalloc.f
require src/compiler/native/regalloc-verify.f
require src/compiler/native/a64ir.f
require src/arch/x86-64/abi.f
require src/arch/x86-64/machine.f

package X64RA-TEST
private

\ ---- bindings ----------------------------------------------------------------
\ A linux x86-64 contract whose integer overflow wraps: the machine's own
\ behaviour, and the one this dialect selects arithmetic under.
: WBND ( -- CBIND:binding )
   CTARGET-ARCH:X86-64 CTARGET-ABI:SYSV-AMD64 CTARGET-ENDIAN:LITTLE
   CTARGET-PTR--WIDTH:BITS64
   CTARGET:F-BASE CTARGET:F-FP CTARGET:WITH CTARGET:CONTRACT
   CNUM-OVERFLOW:WRAP CNUM-FLOAT--MODEL:IEEE754 CNUM-CONTRACTION:FORBIDDEN
   CNUM-FAST--MATH:BIT-EXACT CNUM-COMPARE:IEEE754-UNORDERED CNUM:POLICY
   CBIND:BIND ;

\ The other machine, for the one case that builds a module of the other dialect:
\ a dialect may only make a builder under a contract its backend lowers for.
: ABND ( -- CBIND:binding )
   CTARGET-ARCH:AARCH64 CTARGET-ABI:AAPCS64-DARWIN CTARGET-ENDIAN:LITTLE
   CTARGET-PTR--WIDTH:BITS64
   CTARGET:F-BASE CTARGET:F-FP CTARGET:WITH CTARGET:CONTRACT
   CNUM-OVERFLOW:WRAP CNUM-FLOAT--MODEL:IEEE754 CNUM-CONTRACTION:FORBIDDEN
   CNUM-FAST--MATH:BIT-EXACT CNUM-COMPARE:IEEE754-UNORDERED CNUM:POLICY
   CBIND:BIND ;

\ ---- the fixture's source text -----------------------------------------------
\ One text stands behind every fixture, so each span a fixture attaches is a real
\ byte range in bytes the module has really registered.
create TXT
   58 c, 32 c, 76 c, 69 c, 65 c, 70 c, 32 c, 45 c,            \ ": LEAF -"
   32 c, 100 c, 117 c, 112 c, 32 c, 43 c, 32 c, 59 c,         \ " dup + ;"
16 constant TXT-N

2 constant NAME-ST                   \ the defined name inside TXT
4 constant NAME-LN
0 constant OPEN-ST                   \ the opening `:`
1 constant OPEN-LN
7 constant BODY-ST                   \ the body word
1 constant BODY-LN
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

: HIR-MOD ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   IR-BUILD:PLAN-BEGIN
   IR-BUILD:PLAN-DEFAULT
   c HIR:NEW-BUILDER {: b:IR-BUILD:builder :}
   c b HIR:REGISTER
   c 0 W-CTX !
   b 0 W-BLD !
   c b TXT TXT-N IR-BUILD:ADD-SOURCE 0 W-SRC ! ;

\ ---- staging one source operation --------------------------------------------
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

: RET1 ( IR-ID:ir-value-id -- )
   {: v:IR-ID:ir-value-id :}
   HIR-OPCODE:RETURN CLOSE-ST CLOSE-LN OPEN-OP
   CC BB v IR-BUILD:ADD-OPERAND
   CC BB IR-BUILD:END-OP drop ;

\ ---- staging the function ----------------------------------------------------
: SIGN ( n n -- IR-ID:ir-type-id )
   {: in:n out:n :}
   CELLT {: t:IR-ID:ir-type-id :}
   IR-TYPE:FN-BEGIN
   in 0 ?do t IR-TYPE:FN-PARAM loop
   out 0 ?do t IR-TYPE:FN-RESULT loop
   CC BB IR-BUILD:INTERN-CODE-REF ;

: OPEN-FUN ( n n -- )
   {: in:n out:n :}
   CC BB  CC BB s" LEAF" IR-BUILD:INTERN-SYMBOL  IR-BUILD:BEGIN-FUN
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

\ ---- the two shapes ----------------------------------------------------------
\ `: LEAF ( a b -- n ) - ;` - two arguments that die at the subtraction, which is
\ two-address and destroys the first of them. Nothing is copied.
: BUILD-DIFF ( -- )
   2 1 OPEN-FUN
   ARG+ {: x:IR-ID:ir-value-id :}
   ARG+ {: y:IR-ID:ir-value-id :}
   HIR-OPCODE:SUB x y BINOP RET1
   CLOSE-FUN ;

\ `: LEAF ( a -- n ) dup + ;` - one argument read twice by a form that destroys
\ its first operand, so selection copies it with `x64.mov` first. The copy and
\ its source are live at the same instant and cannot share a register.
: BUILD-SQUARE ( -- )
   1 1 OPEN-FUN
   ARG+ {: a:IR-ID:ir-value-id :}
   HIR-OPCODE:ADD a a BINOP RET1
   CLOSE-FUN ;

\ ---- running selection, allocation and validation ----------------------------
: X64-BUILDER ( -- IR-BUILD:builder )
   IR-BUILD:PLAN-BEGIN
   IR-BUILD:PLAN-DEFAULT
   CC X64IR:NEW-BUILDER ;

\ The contract every case here allocates under: a leaf that computes in this
\ machine's nine allocatable general registers (X64ABI:SCRATCH), returns to its
\ caller, reserves no frame and calls nothing. It names no place, so the
\ selector adds no entry and no exit and the module is the body alone. The
\ flags are declared CLOBBERED because every arithmetic form of this machine
\ writes them - there is no add here that does not.
: LEAF ( -- NEFF:routine )
   NEFF-CONV:REGISTER NEFF:SEQ-NONE NEFF:SEQ-NONE X64ABI:SCRATCH
   NEFF:FPR-NONE NEFF:FPR-NONE NEFF:FPR-NONE
   NEFF-NZCV:CLOBBERED NEFF-LINK:ABSENT NEFF-CONTROL:RETURNS
   NEFF:TRAITS-NONE 0 0 X64M:MACHINE NEFF:ROUTINE ;

\ Bind the source dialect to the module being read and the machine dialect to
\ the module about to be written, then select. Both the allocator and the
\ validator are bound with THIS dialect's vocabulary and THIS machine, which is
\ the whole point: neither pass names x86-64 anywhere in its own text.
: SELECTED ( -- IR-BUILD:module )
   CC BB X64SEL:BIND-SOURCE
   CC BB IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   X64-BUILDER {: xb:IR-BUILD:builder :}
   CC xb X64M:MACHINE  CC xb X64IR:VOCABULARY  A64RA:BIND-DIALECT
   CC xb  CC xb X64IR:VOCABULARY  A64RAV:BIND-DIALECT
   CC m xb LEAF X64SEL:SELECT ;

\ Allocate the selected module for the same contract it was selected under and
\ have the validator accept it. Every positive case goes through both, so no
\ case reads a claim the validator has not agreed with.
: ALLOCATED ( -- IR-BUILD:module )
   SELECTED {: m:IR-BUILD:module :}
   CC m LEAF A64RA:ALLOCATE
   m LEAF A64RAV:ACCEPT
   m ;

\ ---- and the same body under the data-stack convention -----------------------
\ The other contract of this machine: the interface is two caller cells in and
\ one out, so the selector writes the boundary and the validator has a stand to
\ measure. It is x86-64's own policy that is measured, because the vocabulary
\ x64ir builds states `entry` and the validator reads it there.
: DLEAF ( -- NEFF:routine )
   X64ABI:SCRATCH 2 1 X64ABI:LEAF ;

: DSTACK-SELECTED ( -- IR-BUILD:module )
   CC BB X64SEL:BIND-SOURCE
   CC BB IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   X64-BUILDER {: xb:IR-BUILD:builder :}
   CC xb X64M:MACHINE  CC xb X64IR:VOCABULARY  A64RA:BIND-DIALECT
   CC xb  CC xb X64IR:VOCABULARY  A64RAV:BIND-DIALECT
   CC m xb DLEAF X64SEL:SELECT ;

: DSTACK-ALLOCATED ( -- IR-BUILD:module )
   DSTACK-SELECTED {: m:IR-BUILD:module :}
   CC m DLEAF A64RA:ALLOCATE
   m DLEAF A64RAV:ACCEPT
   m ;

\ ---- the cases ---------------------------------------------------------------
: DIFF-BODY ( IR-CTX:ctx -- n n n n n bool )
   HIR-MOD
   BUILD-DIFF
   ALLOCATED drop
   A64RA:VALUES
   0 A64RAV:REG@
   1 A64RAV:REG@
   2 A64RAV:REG@
   A64RA:PLAN-N
   A64RAV:ACCEPTED? ;

\ The boundary is three memory tokens and a fourth the publish leaves, so the
\ values the allocator places are the two loaded arguments and the difference:
\ 1, 3 and 5. The other four are tokens and REG@ refuses them by class.
: DSTACK-BODY ( IR-CTX:ctx -- n n n n n bool )
   HIR-MOD
   BUILD-DIFF
   DSTACK-ALLOCATED drop
   A64RA:VALUES
   1 A64RAV:REG@
   3 A64RAV:REG@
   5 A64RAV:REG@
   A64RA:PLAN-N
   A64RAV:ACCEPTED? ;

: SQUARE-BODY ( IR-CTX:ctx -- n n n n n bool )
   HIR-MOD
   BUILD-SQUARE
   ALLOCATED drop
   A64RA:VALUES
   0 A64RAV:REG@
   1 A64RAV:REG@
   2 A64RAV:REG@
   A64RA:PLAN-N
   A64RAV:ACCEPTED? ;

\ ---- the vocabulary is one module's, and only that module's ------------------
\ Every name in a vocabulary is an ordinal of the module it was interned in, so
\ a vocabulary handed to another module's binding names nothing there. The
\ binding refuses before it copies a single name out.
: WRONG-VOCAB-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   IR-BUILD:PLAN-BEGIN
   IR-BUILD:PLAN-DEFAULT
   c A64IR:NEW-BUILDER {: b:IR-BUILD:builder :}
   c b A64IR:MACHINE  c b X64IR:VOCABULARY  A64RA:BIND-DIALECT ;

: WRONG-VOCAB ( -- )
   ABND [: WRONG-VOCAB-BODY ;] IR-CTX:WITH-CONTEXT ;

\ The key this dialect does not have, asked for anyway. x64ir declares the
\ write-back key ABSENT because x86-64 has no write-back addressing and interns
\ no symbol for one, and the reader refuses rather than answering with a number
\ that would read as a symbol here.
: ABSENT-KEY ( -- )
   NDIALECT-OPTKEY:ABSENT NDIALECT:KEY drop ;

: VOCAB-REFUSE-CASES ( -- )
   s" one dialect's vocabulary bound to another dialect's module is refused, because its names are that module's ordinals and name nothing here" T-LABEL
   [: WRONG-VOCAB ;] E-A64RA-MODULE TTHROWSQ

   s" a key the dialect declared absent is refused rather than answered: x64ir says `absent` for the write-back key every data-stack case above reads through" T-LABEL
   [: ABSENT-KEY ;] E-NDIALECT TTHROWSQ ;

\ A refusing case runs INSIDE an enclosing context: an abandoned context gives
\ its registry slots back only when a live enclosing context leaves normally.
: GROUP-VOCAB ( IR-CTX:ctx -- )       drop VOCAB-REFUSE-CASES ;

public

: RUN ( -- )
   T-RESET

   s" two arguments take two registers and the difference reuses the first: the allocator reads this module through the x86-64 vocabulary alone" T-LABEL
   WBND [: DIFF-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE 0 T= 0 T= 1 T= 0 T= 3 T=

   s" the copy a two-address form needs takes a register of its own: the allocator found it by the vocabulary's own copy opcode, x64.mov" T-LABEL
   WBND [: SQUARE-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE 0 T= 1 T= 1 T= 0 T= 3 T=

   s" the same leaf under the data-stack convention allocates and is accepted: the validator measures the stand against the `entry` policy x64ir states, where re-deriving A64SEL's survey refused this module with E-A64RAV-DSTACK" T-LABEL
   WBND [: DSTACK-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE 0 T= 0 T= 1 T= 0 T= 7 T=

   WBND [: GROUP-VOCAB ;] IR-CTX:WITH-CONTEXT

   T-REPORT ;

;package

X64RA-TEST:RUN
