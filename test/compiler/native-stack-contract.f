\ Frozen machine modules exercise the native stack boundary without executing
\ their loads, stores or recursive calls.
require lib/test.f
require src/compiler/native/regalloc-verify.f

package STACK-CONTRACT-TEST
using IR-BUILD
using A64IR
using A64EFF

1 TYPED-BUFFER CONTEXT IR-CTX:ctx
1 TYPED-BUFFER BUILDER IR-BUILD:builder
1 TYPED-BUFFER SOURCE-ID IR-ID:ir-source-id

: CC ( -- IR-CTX:ctx ) 0 CONTEXT @ ;
: BB ( -- IR-BUILD:builder ) 0 BUILDER @ ;


: BINDING ( -- CBIND:binding )
   CTARGET-ARCH:AARCH64 CTARGET-ABI:AAPCS64-DARWIN CTARGET-ENDIAN:LITTLE
   CTARGET-PTR--WIDTH:BITS64
   CTARGET:F-BASE CTARGET:F-FP CTARGET:WITH CTARGET:CONTRACT
   CNUM-OVERFLOW:WRAP CNUM-FLOAT--MODEL:IEEE754 CNUM-CONTRACTION:FORBIDDEN
   CNUM-FAST--MATH:BIT-EXACT CNUM-COMPARE:IEEE754-UNORDERED CNUM:POLICY
   CBIND:BIND ;


: SPAN ( -- IR-SOURCE:span ) BB 0 SOURCE-ID @ 0 1 ADD-SPAN ;


: MODULE-OPEN ( IR-CTX:ctx -- ) {: c:IR-CTX:ctx :}
   PLAN-BEGIN PLAN-DEFAULT
   c A64IR:NEW-BUILDER {: b:IR-BUILD:builder :}
   c 0 CONTEXT ! b 0 BUILDER !
   c b A64RA:BIND-DIALECT c b A64RAV:BIND-DIALECT
   c b REGISTER
   c b s" stack boundary" ADD-SOURCE 0 SOURCE-ID ! ;


: SIGNATURE ( n -- IR-ID:ir-type-id ) {: count:n :}
   CC BB GPR-TYPE {: t:IR-ID:ir-type-id :}
   IR-TYPE:FN-BEGIN
   count 0 ?do t IR-TYPE:FN-PARAM loop
   count 0 ?do t IR-TYPE:FN-RESULT loop
   CC BB INTERN-CODE-REF ;


: FUNCTION-OPEN ( n -- ) {: count:n :}
   CC BB CC BB s" BOUNDARY" INTERN-SYMBOL BEGIN-FUN
   CC BB count SIGNATURE SET-SIGNATURE
   CC BB IR--FUN-LINKAGE:DEFINED SET-LINKAGE
   CC BB IR--FUN-VISIBILITY:EXPORTED SET-VISIBILITY
   CC BB IR--FUN-CONVENTION:HABU SET-CONVENTION
   CC BB SPAN SET-FUN-SPAN
   CC BB BEGIN-BLOCK CC BB SPAN SET-BLOCK-SPAN ;


: OP-OPEN ( A64IR:opcode -- ) {: op:A64IR:opcode :}
   CC BB CC BB op OPCODE BEGIN-OP CC BB SPAN SET-OP-SPAN ;


: TOKEN+ ( -- ) CC BB CC BB MEM-TYPE ADD-RESULT ;
: VALUE+ ( -- ) CC BB CC BB GPR-TYPE ADD-RESULT ;


: OP-VALUE ( -- IR-ID:ir-value-id )
   CC BB END-OP {: op:IR-ID:ir-op-id :}
   CC BB op 0 OP-RESULT@ ;


: DISTANCE ( n -- ) {: bytes:n :}
   CC BB CC BB KEY-DBYTES CC BB bytes DBYTES-ATTR ADD-ATTR ;


: DATA-SLOT ( n -- ) {: bytes:n :}
   CC BB CC BB KEY-DSLOT CC BB bytes DSLOT-ATTR ADD-ATTR ;


: TAKE-DATA ( n -- IR-ID:ir-value-id )
   A64IR-OPCODE:DTAKE OP-OPEN TOKEN+ DISTANCE OP-VALUE ;


: LOAD-DATA ( IR-ID:ir-value-id n -- IR-ID:ir-value-id IR-ID:ir-value-id )
   {: tok:IR-ID:ir-value-id bytes:n :}
   A64IR-OPCODE:DLOAD OP-OPEN CC BB tok ADD-OPERAND
   VALUE+ TOKEN+ bytes DATA-SLOT
   CC BB END-OP {: op:IR-ID:ir-op-id :}
   CC BB op 0 OP-RESULT@ CC BB op 1 OP-RESULT@ ;


: DOUBLE-VALUE ( IR-ID:ir-value-id -- IR-ID:ir-value-id )
   {: value:IR-ID:ir-value-id :}
   A64IR-OPCODE:ADD OP-OPEN
   CC BB value ADD-OPERAND CC BB value ADD-OPERAND VALUE+ OP-VALUE ;


: STORE-DATA ( IR-ID:ir-value-id IR-ID:ir-value-id n -- IR-ID:ir-value-id )
   {: value:IR-ID:ir-value-id tok:IR-ID:ir-value-id bytes:n :}
   A64IR-OPCODE:DSTORE OP-OPEN
   CC BB value ADD-OPERAND CC BB tok ADD-OPERAND
   TOKEN+ bytes DATA-SLOT OP-VALUE ;


: PUBLISH-DATA ( IR-ID:ir-value-id n -- )
   {: tok:IR-ID:ir-value-id bytes:n :}
   A64IR-OPCODE:DPUBLISH OP-OPEN
   CC BB tok ADD-OPERAND bytes DISTANCE CC BB END-OP drop ;


: FUNCTION-CLOSE ( -- IR-BUILD:module )
   A64IR-OPCODE:RET OP-OPEN CC BB END-OP drop
   CC BB END-BLOCK drop CC BB END-FUN drop CC BB FREEZE ;


: CONTRACT ( A64EFF:placeseq A64EFF:placeseq bool -- A64EFF:routine )
   {: args:A64EFF:placeseq outs:A64EFF:placeseq calls:bool :}
   A64EFF-CONV:DSTACK args outs $F GPR-SET
   FPR-NONE FPR-NONE FPR-NONE
   A64EFF-NZCV:UNTOUCHED A64EFF-LINK:PRESERVED A64EFF-CONTROL:RETURNS
   calls if T-CALL SP-ALIGN else TRAITS-NONE 0 then
   0 ROUTINE ;


: ACCEPT-MODULE ( IR-BUILD:module A64EFF:placeseq A64EFF:placeseq bool -- bool )
   {: m:IR-BUILD:module args:A64EFF:placeseq outs:A64EFF:placeseq calls:bool :}
   CC m args outs calls CONTRACT A64RA:ALLOCATE
   m args outs calls CONTRACT A64RAV:ACCEPT
   A64RAV:ACCEPTED? ;


: ONE-SLOT ( n -- A64EFF:placeseq ) SEQ-NONE swap SEQ-WITH-SLOT ;


: SLOT-BODY ( n n IR-CTX:ctx -- bool )
   {: input:n output:n c:IR-CTX:ctx :}
   \ Only the declarations vary. The old verifier silently normalized a sparse
   \ declaration to slot zero and accepted this canonical machine body.
   c MODULE-OPEN 1 FUNCTION-OPEN
   0 TAKE-DATA SLOT-WIDTH negate LOAD-DATA
   {: value:IR-ID:ir-value-id tok:IR-ID:ir-value-id :}
   value DOUBLE-VALUE tok SLOT-WIDTH negate STORE-DATA 0 PUBLISH-DATA
   FUNCTION-CLOSE input ONE-SLOT output ONE-SLOT false ACCEPT-MODULE ;


: SLOT-CASE ( n n -- bool ) BINDING [: SLOT-BODY ;] IR-CTX:WITH-CONTEXT ;


: SPARSE-INPUT ( IR-CTX:ctx -- )
   drop [: 31 0 SLOT-CASE drop ;] E-A64RAV-DSTACK TTHROWSQ ;


: SPARSE-OUTPUT ( IR-CTX:ctx -- )
   drop [: 0 31 SLOT-CASE drop ;] E-A64RAV-DSTACK TTHROWSQ ;


\ The native boundary already normalizes a complete set to slot order. Preserve
\ that behavior here; this test does not introduce a permuted calling convention.
: PERMUTED-SLOTS ( -- A64EFF:placeseq ) 1 ONE-SLOT 0 SEQ-WITH-SLOT ;


: PERMUTED-BODY ( IR-CTX:ctx -- bool )
   MODULE-OPEN 2 FUNCTION-OPEN
   0 TAKE-DATA -16 LOAD-DATA {: a:IR-ID:ir-value-id t0:IR-ID:ir-value-id :}
   t0 -8 LOAD-DATA {: b:IR-ID:ir-value-id t1:IR-ID:ir-value-id :}
   a DOUBLE-VALUE {: aa:IR-ID:ir-value-id :}
   b DOUBLE-VALUE {: bb:IR-ID:ir-value-id :}
   aa t1 -16 STORE-DATA {: t2:IR-ID:ir-value-id :}
   bb t2 -8 STORE-DATA 0 PUBLISH-DATA
   FUNCTION-CLOSE PERMUTED-SLOTS PERMUTED-SLOTS false ACCEPT-MODULE ;


: FRAME-RESERVE ( -- IR-ID:ir-value-id )
   A64IR-OPCODE:RESERVE OP-OPEN TOKEN+
   CC BB CC BB KEY-FRAME CC BB SP-ALIGN FRAME-ATTR ADD-ATTR OP-VALUE ;


: FRAME-LINK ( IR-ID:ir-value-id A64IR:opcode -- IR-ID:ir-value-id )
   {: tok:IR-ID:ir-value-id op:A64IR:opcode :}
   op OP-OPEN CC BB tok ADD-OPERAND TOKEN+
   CC BB CC BB KEY-SLOT CC BB A64FRAME:LINK-SLOT SLOT-ATTR ADD-ATTR OP-VALUE ;


: FRAME-RELEASE ( IR-ID:ir-value-id -- )
   A64IR-OPCODE:RELEASE OP-OPEN CC BB rot ADD-OPERAND
   CC BB CC BB KEY-FRAME CC BB SP-ALIGN FRAME-ATTR ADD-ATTR CC BB END-OP drop ;


: SELF-CALL ( IR-ID:ir-value-id n -- IR-ID:ir-value-id )
   {: tok:IR-ID:ir-value-id bytes:n :}
   A64IR-OPCODE:CALL OP-OPEN CC BB tok ADD-OPERAND TOKEN+
   bytes DISTANCE
   CC BB CC BB KEY-DBACK CC BB bytes DBACK-ATTR ADD-ATTR OP-VALUE ;


\ Each call adds two requested positions. This exceeds the verifier's current
\ 256-position optimization buffer; safety must still hold when that fills.
257 constant MANY-CALLS


: CALLS-BODY ( n IR-CTX:ctx -- bool ) {: stand:n c:IR-CTX:ctx :}
   c MODULE-OPEN 1 FUNCTION-OPEN
   FRAME-RESERVE A64IR-OPCODE:LINKSAVE FRAME-LINK {: frame:IR-ID:ir-value-id :}
   SLOT-WIDTH stand - TAKE-DATA
   MANY-CALLS 0 ?do SLOT-WIDTH stand - SELF-CALL loop
   SLOT-WIDTH stand - PUBLISH-DATA
   frame A64IR-OPCODE:LINKLOAD FRAME-LINK FRAME-RELEASE
   FUNCTION-CLOSE 1 SEQ-DSTACK 1 SEQ-DSTACK true ACCEPT-MODULE ;


: CALLS-CASE ( n -- bool ) BINDING [: CALLS-BODY ;] IR-CTX:WITH-CONTEXT ;


: HIGH-STANDING ( IR-CTX:ctx -- )
   drop [: 16 CALLS-CASE drop ;] E-A64RAV-DSTACK TTHROWSQ ;

public

: RUN ( -- )
   T-RESET
   s" contiguous input and output are accepted" T-LABEL 0 0 SLOT-CASE TTRUE
   s" a complete permutation normalizes to native slot order" T-LABEL
   BINDING [: PERMUTED-BODY ;] IR-CTX:WITH-CONTEXT TTRUE
   s" sparse input is refused at the native boundary" T-LABEL
   BINDING [: SPARSE-INPUT ;] IR-CTX:WITH-CONTEXT
   s" sparse output is refused at the native boundary" T-LABEL
   BINDING [: SPARSE-OUTPUT ;] IR-CTX:WITH-CONTEXT
   s" a full placement buffer still permits valid standing" T-LABEL 8 CALLS-CASE TTRUE
   s" standing above entry is refused even with a full placement buffer" T-LABEL
   BINDING [: HIGH-STANDING ;] IR-CTX:WITH-CONTEXT
   T-REPORT ;

;using
;using
;using
;package

STACK-CONTRACT-TEST:RUN
