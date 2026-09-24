\ ir-structure-cases.f - Exercise compiler structures against the shared construction and guard vectors.

require lib/test.f
require lib/string.f
require test/compiler/ir-structure-schema.f

package COMPILER-STRUCT-CASES
using COMPILER-STRUCT-PROOF
private

variable CUR

\ ---- the rigged module -------------------------------------------------------
\ An AArch64 Darwin contract with the baseline instruction set and plain
\ floating point, which is what the habu calling convention needs.
: BND ( -- CBIND:binding )
   CTARGET-ARCH:AARCH64 CTARGET-ABI:AAPCS64-DARWIN CTARGET-ENDIAN:LITTLE
   CTARGET-PTR--WIDTH:BITS64
   CTARGET:F-BASE CTARGET:F-FP CTARGET:WITH CTARGET:CONTRACT
   CNUM-OVERFLOW:TRAP CNUM-FLOAT--MODEL:IEEE754 CNUM-CONTRACTION:FORBIDDEN
   CNUM-FAST--MATH:BIT-EXACT CNUM-COMPARE:IEEE754-UNORDERED CNUM:POLICY
   CBIND:BIND ;

\ Three opcodes. The variadic one lets the schema accept any operand and result
\ count, so a value-store step is decided by the SSA rule and the ceilings alone
\ - which is exactly what the model describes. The other two are the filler and
\ the terminator a block is built out of.
0 constant K-VAR
1 constant K-PLAIN
2 constant K-TERM

: OPC-SYM ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key n -- IR-ID:ir-symbol-id )
   {: c:IR-CTX:ctx sp:IR-ARENA:arena sr:IR-ARENA:arena key:IR-ID:ir-module-key k:n :}
   k K-VAR = if c sp sr key s" hir.var" IR-SYM:INTERN exit then
   k K-PLAIN = if c sp sr key s" hir.plain" IR-SYM:INTERN exit then
   c sp sr key s" hir.term" IR-SYM:INTERN ;

: I64 ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key -- IR-ID:ir-type-id )
   IR--TYPE-WIDTH:W64 IR--TYPE-SIGN:SIGNED IR-TYPE:INT ;

: A-SPAN ( IR-CTX:ctx IR-ARENA:arena IR-ID:ir-module-key -- IR-SOURCE:span )
   {: c:IR-CTX:ctx sa:IR-ARENA:arena key:IR-ID:ir-module-key :}
   sa  c sa key s" structure-source" IR-SOURCE:REGISTER  0 4 IR-SOURCE:SPAN ;

: SCH-SHAPE ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key n -- )
   {: c:IR-CTX:ctx tp:IR-ARENA:arena tr:IR-ARENA:arena key:IR-ID:ir-module-key k:n :}
   k K-VAR = if
      c tp tr key I64 IR-SCHEMA:ADD-OPERAND-TAIL
      c tp tr key I64 IR-SCHEMA:ADD-RESULT-TAIL
   then ;

: SCH-CTRL ( n -- )
   K-TERM = if true 0 0 IR-SCHEMA:SET-CONTROL exit then
   false 0 0 IR-SCHEMA:SET-CONTROL ;

: SCH-DEF ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key IR-ARENA:arena IR-ARENA:arena n -- )
   {: c:IR-CTX:ctx sp:IR-ARENA:arena sr:IR-ARENA:arena tp:IR-ARENA:arena tr:IR-ARENA:arena key:IR-ID:ir-module-key qp:IR-ARENA:arena qr:IR-ARENA:arena k:n :}
   c sp sr key k OPC-SYM IR-SCHEMA:BEGIN-OP
   c tp tr key k SCH-SHAPE
   k SCH-CTRL
   IR-SCHEMA:SET-PURE
   false IR-SCHEMA:SET-TRAP
   CTARGET-ARCH:AARCH64 CTARGET:F-BASE IR-SCHEMA:SET-TARGET
   c sp sr key s" rule.hir" IR-SYM:INTERN IR-SCHEMA:SET-RULE
   c sp sr key s" render.hir" IR-SYM:INTERN IR-SCHEMA:SET-RENDERER
   c qp qr key sr tr IR-SCHEMA:DEFINE ;

: SCH-ALL ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key IR-ARENA:arena IR-ARENA:arena -- )
   {: c:IR-CTX:ctx sp:IR-ARENA:arena sr:IR-ARENA:arena tp:IR-ARENA:arena tr:IR-ARENA:arena key:IR-ID:ir-module-key qp:IR-ARENA:arena qr:IR-ARENA:arena :}
   c sp sr tp tr key qp qr K-VAR SCH-DEF
   c sp sr tp tr key qp qr K-PLAIN SCH-DEF
   c sp sr tp tr key qp qr K-TERM SCH-DEF ;

\ One rigged module with the three committed ceilings the sequence asks for.
: RIG ( IR-CTX:ctx n n n -- IR-ID:ir-module-key IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena )
   {: c:IR-CTX:ctx ocap:n vcap:n pcap:n :}
   c IR-CTX:NEW-MODULE drop {: key:IR-ID:ir-module-key :}
   c key 16 256 IR-SYM:NEW {: sp:IR-ARENA:arena sr:IR-ARENA:arena :}
   c key 16 64 IR-TYPE:NEW {: tp:IR-ARENA:arena tr:IR-ARENA:arena :}
   c key 16 64 IR-ATTR:NEW {: ap:IR-ARENA:arena ar:IR-ARENA:arena :}
   c key 8 IR-SOURCE:NEW {: sa:IR-ARENA:arena :}
   c sr key  c sp sr key s" hir" IR-SYM:INTERN  1 0 8 64 IR-SCHEMA:NEW
   {: qp:IR-ARENA:arena qr:IR-ARENA:arena :}
   c key ocap vcap pcap IR-OP:NEW {: p:IR-ARENA:arena v:IR-ARENA:arena r:IR-ARENA:arena :}
   c key 8 32 64 IR-FUN:NEW {: fp:IR-ARENA:arena fr:IR-ARENA:arena br:IR-ARENA:arena :}
   c sp sr tp tr key qp qr SCH-ALL
   key sp sr tp tr ar sa qr p v r fp fr br ;

\ ---- driving one value-store step --------------------------------------------
\ A caught quotation cannot read the enclosing word's locals, so each step
\ carries everything it needs on the data stack and writes its answer to ANS,
\ which the runner preset: `catch` restores the DEPTH of both stacks and never
\ their contents, so no stack slot can carry a value out of a body that threw.
\ The cells the step was handed come back stale from the catch and are dropped.
variable ANS

: STAGE-OPERANDS ( IR-ID:ir-module-key n -- ) {: key:IR-ID:ir-module-key st:n :}
   st SS-OPN@ 0 ?do
      key st SS-OPB@ i + OPD@ IR-ID:PACK-VALUE IR-OP:ADD-OPERAND
   loop ;

: STAGE-RESULTS ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key n -- )
   {: c:IR-CTX:ctx tp:IR-ARENA:arena tr:IR-ARENA:arena key:IR-ID:ir-module-key st:n :}
   st SS-RSN@ 0 ?do
      c tp tr key I64 IR-OP:ADD-RESULT
   loop ;

: SSA-TRY ( IR-CTX:ctx IR-ID:ir-module-key IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena n -- IR-CTX:ctx IR-ID:ir-module-key IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena n )
   {: c:IR-CTX:ctx key:IR-ID:ir-module-key sp:IR-ARENA:arena sr:IR-ARENA:arena tp:IR-ARENA:arena tr:IR-ARENA:arena ar:IR-ARENA:arena sa:IR-ARENA:arena qr:IR-ARENA:arena p:IR-ARENA:arena v:IR-ARENA:arena r:IR-ARENA:arena st:n :}
   c sp sr key K-VAR OPC-SYM IR-OP:BEGIN-OP
   c sa key A-SPAN IR-OP:SET-SPAN
   key st STAGE-OPERANDS
   c tp tr key st STAGE-RESULTS
   c p v r key qr tr ar sa IR-OP:END-OP IR-ID:OP-LOCAL ANS !
   c key sp sr tp tr ar sa qr p v r st ;

: SSA-STEP ( IR-CTX:ctx IR-ID:ir-module-key IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena n -- n n )
   -1 ANS !
   [: SSA-TRY ;] catch {: rc:n :}
   2drop 2drop 2drop 2drop 2drop 2drop drop
   rc ANS @ ;

\ ---- driving one block step --------------------------------------------------

: FILLER-OP ( IR-CTX:ctx IR-ID:ir-module-key IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena n -- )
   {: c:IR-CTX:ctx key:IR-ID:ir-module-key sp:IR-ARENA:arena sr:IR-ARENA:arena tp:IR-ARENA:arena tr:IR-ARENA:arena ar:IR-ARENA:arena sa:IR-ARENA:arena qr:IR-ARENA:arena p:IR-ARENA:arena v:IR-ARENA:arena r:IR-ARENA:arena k:n :}
   c sp sr key k OPC-SYM IR-OP:BEGIN-OP
   c sa key A-SPAN IR-OP:SET-SPAN
   c p v r key qr tr ar sa IR-OP:END-OP drop ;

: BLK-TRY ( IR-CTX:ctx IR-ID:ir-module-key IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena -- IR-CTX:ctx IR-ID:ir-module-key IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena )
   {: c:IR-CTX:ctx key:IR-ID:ir-module-key sp:IR-ARENA:arena sr:IR-ARENA:arena tp:IR-ARENA:arena tr:IR-ARENA:arena ar:IR-ARENA:arena sa:IR-ARENA:arena qr:IR-ARENA:arena p:IR-ARENA:arena v:IR-ARENA:arena r:IR-ARENA:arena fr:IR-ARENA:arena br:IR-ARENA:arena :}
   c sa key A-SPAN IR-FUN:SET-BLOCK-SPAN
   c br fr key v r qr sa IR-FUN:END-BLOCK IR-ID:BLOCK-LOCAL ANS !
   c key sp sr tp tr ar sa qr p v r fr br ;

: BLK-END ( IR-CTX:ctx IR-ID:ir-module-key IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena -- n n )
   -1 ANS !
   [: BLK-TRY ;] catch {: rc:n :}
   2drop 2drop 2drop 2drop 2drop 2drop 2drop
   rc ANS @ ;

\ ---- one step's two assertions -----------------------------------------------

: STEP-ANSWER ( n n n n -- ) {: class:n ord:n rc:n got:n :}
   s" the builder reaches the answer the shared vector row records" T-LABEL
   rc class T=
   class 0 <> if exit then
   s" an accepted step answers the ordinal the shared vector row records" T-LABEL
   got ord T= ;

\ ---- one value-store sequence ------------------------------------------------

\ Every accepted step's operands were laid into the cell pool by
\ `IR-OP:WIN-STARTS` and `IR-OP:ROW-ADD`, and they come back out through
\ `IR-OP:OPERAND@`, which revalidates the whole row's tiling with `TILE-CK`
\ before it reads a single cell. The shared row already records those ordinals,
\ so asking the shipped store to hand them back drives the operand window
\ itself: a start that lands on the wrong cell, or an operand arm of the tiling
\ that stopped comparing, changes what comes back. Reading happens after the
\ last step so the row is read against the finished table.
: OPERAND-TRY ( IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key IR-ID:ir-op-id n -- IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key IR-ID:ir-op-id n )
   {: p:IR-ARENA:arena r:IR-ARENA:arena key:IR-ID:ir-module-key id:IR-ID:ir-op-id i:n :}
   p r key id i IR-OP:OPERAND@ IR-ID:VALUE-LOCAL ANS !
   p r key id i ;

: OPERAND-GET ( IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key IR-ID:ir-op-id n -- n n )
   -1 ANS !
   [: OPERAND-TRY ;] catch {: rc:n :}
   2drop 2drop drop
   rc ANS @ ;

: OPERAND-BACK ( IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key IR-ID:ir-op-id n n -- )
   {: p:IR-ARENA:arena r:IR-ARENA:arena key:IR-ID:ir-module-key id:IR-ID:ir-op-id i:n want:n :}
   p r key id i OPERAND-GET {: rc:n got:n :}
   s" reading an operand of an accepted operation back is accepted" T-LABEL
   rc 0 T=
   s" the operand window hands back the ordinal the shared row records" T-LABEL
   got want T= ;

: OPERANDS-BACK ( IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key n -- )
   {: p:IR-ARENA:arena r:IR-ARENA:arena key:IR-ID:ir-module-key st:n :}
   st SS-CLASS@ 0 <> if exit then
   key st SS-ORD@ IR-ID:PACK-OP {: id:IR-ID:ir-op-id :}
   st SS-OPN@ 0 ?do
      p r key id i st SS-OPB@ i + OPD@ OPERAND-BACK
   loop ;

: SSA-SEQ ( IR-CTX:ctx -- ) {: c:IR-CTX:ctx :}
   CUR @ {: s:n :}
   c s SCN-OCAP@ s SCN-VCAP@ s SCN-PCAP@ RIG
   {: key:IR-ID:ir-module-key sp:IR-ARENA:arena sr:IR-ARENA:arena tp:IR-ARENA:arena tr:IR-ARENA:arena ar:IR-ARENA:arena sa:IR-ARENA:arena qr:IR-ARENA:arena p:IR-ARENA:arena v:IR-ARENA:arena r:IR-ARENA:arena fp:IR-ARENA:arena fr:IR-ARENA:arena br:IR-ARENA:arena :}
   s SCN-LEN@ 0 ?do
      s SCN-BASE@ i + {: st:n :}
      c key sp sr tp tr ar sa qr p v r st SSA-STEP {: rc:n got:n :}
      st SS-CLASS@ st SS-ORD@ rc got STEP-ANSWER
   loop
   s SCN-LEN@ 0 ?do
      p r key s SCN-BASE@ i + OPERANDS-BACK
   loop
   s" the sequence ends holding the operation count the shared row records" T-LABEL
   r IR-OP:OPS s SCN-FINAL-A@ T=
   s" the sequence ends holding the value count the shared row records" T-LABEL
   v IR-OP:VALUES s SCN-FINAL-B@ T= ;

\ ---- one block sequence ------------------------------------------------------

: FILL-BEFORE ( IR-CTX:ctx IR-ID:ir-module-key IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena n -- )
   {: c:IR-CTX:ctx key:IR-ID:ir-module-key sp:IR-ARENA:arena sr:IR-ARENA:arena tp:IR-ARENA:arena tr:IR-ARENA:arena ar:IR-ARENA:arena sa:IR-ARENA:arena qr:IR-ARENA:arena p:IR-ARENA:arena v:IR-ARENA:arena r:IR-ARENA:arena st:n :}
   st BS-STRAY@ 0 ?do
      c key sp sr tp tr ar sa qr p v r K-PLAIN FILLER-OP
   loop ;

: FILL-INSIDE ( IR-CTX:ctx IR-ID:ir-module-key IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena n -- )
   {: c:IR-CTX:ctx key:IR-ID:ir-module-key sp:IR-ARENA:arena sr:IR-ARENA:arena tp:IR-ARENA:arena tr:IR-ARENA:arena ar:IR-ARENA:arena sa:IR-ARENA:arena qr:IR-ARENA:arena p:IR-ARENA:arena v:IR-ARENA:arena r:IR-ARENA:arena st:n :}
   st BS-OPN@ 0 ?do
      i st BS-TOFF@ = if K-TERM else K-PLAIN then {: k:n :}
      c key sp sr tp tr ar sa qr p v r k FILLER-OP
   loop ;

: BLK-SEQ ( IR-CTX:ctx -- ) {: c:IR-CTX:ctx :}
   CUR @ {: s:n :}
   c 32 32 128 RIG
   {: key:IR-ID:ir-module-key sp:IR-ARENA:arena sr:IR-ARENA:arena tp:IR-ARENA:arena tr:IR-ARENA:arena ar:IR-ARENA:arena sa:IR-ARENA:arena qr:IR-ARENA:arena p:IR-ARENA:arena v:IR-ARENA:arena r:IR-ARENA:arena fp:IR-ARENA:arena fr:IR-ARENA:arena br:IR-ARENA:arena :}
   br  c sp sr key s" main" IR-SYM:INTERN  IR-FUN:BEGIN-FUN
   s SCN-LEN@ 0 ?do
      s SCN-BASE@ i + {: st:n :}
      c key sp sr tp tr ar sa qr p v r st FILL-BEFORE
      r IR-FUN:BEGIN-BLOCK
      c key sp sr tp tr ar sa qr p v r st FILL-INSIDE
      c key sp sr tp tr ar sa qr p v r fr br BLK-END {: rc:n got:n :}
      st BS-CLASS@ st BS-ORD@ rc got STEP-ANSWER
   loop
   IR-FUN:ABANDON-FUN
   s" the sequence ends holding the block count the shared row records" T-LABEL
   br IR-FUN:BLOCKS s SCN-FINAL-A@ T=
   s" the sequence ends holding the operation count the shared row records" T-LABEL
   r IR-OP:OPS s SCN-FINAL-B@ T= ;

: SEQ-BODY ( IR-CTX:ctx -- ) {: c:IR-CTX:ctx :}
   CUR @ SCN-KIND@ KIND-SSA = if c SSA-SEQ exit then
   c BLK-SEQ ;

public

: VECTORS ( -- )
   SCENARIOS 0 ?do
      i CUR !
      BND [: SEQ-BODY ;] IR-CTX:WITH-CONTEXT
   loop ;

: HABU-SIDE ( -- )
   VECTORS ;

;using
;package
