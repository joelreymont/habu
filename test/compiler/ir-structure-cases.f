\ ir-structure-cases.f - the Habu half of the structure parity binding.
\
\ The module lives in `package COMPILER-STRUCT-CASES`. It takes the frozen rows
\ in `package COMPILER-STRUCT-PROOF` and asks the shipped compiler about them,
\ in four groups:
\
\   - the frozen guard bodies, read out of the production source as normalized
\     token runs. These are the bodies the model's hypotheses rest on: the
\     strictly-below operand rule, the four window tiling checks with the step
\     comparison they share, the append side that lays the four windows down in
\     order, and the terminator field the model finds to be derived.
\
\   - the call-closed guard rows. The reader first classifies every definition
\     in the owning file: a definition WRITES if it carries the arena push token
\     or calls a writer, and it GUARDS if it carries the row's guard token or
\     calls a guard. Both relations are closed under calls, because `END-OP`
\     reaches the strictly-below rule two calls deep and `OP@` reaches the
\     tiling rule three calls deep, and a one-level token scan of either body
\     would find no guard token at all and pass vacuously. Every row asserts
\     exactly that: the guard token occurs ZERO times in the body itself, and
\     the call-closed classification still finds the guard. A builder row must
\     also write rows and run every guard before its first write; a reader row
\     must write nothing.
\
\   - every build sequence, run through the real `IR-OP` and `IR-FUN` builder
\     words. The same rows become Rocq obligations in
\     `test/compiler/ir-structure-obligations.f`; this file never restates them.
\
\   - the coverage of the sequence table itself, so a role or a store that stops
\     being driven fails here rather than quietly shrinking what the gate asks.
\
\ Consumers: `test/compiler/ir-structure-manifest.f` (these four groups alone)
\ and `test/compiler/ir-structure-proof.f` (these four plus the Rocq half).

require lib/test.f
require lib/string.f
require test/compiler/ir-id-source.f
require test/compiler/ir-structure-schema.f

package COMPILER-STRUCT-CASES
using COMPILER-STRUCT-PROOF
private

512 constant DEF-MAX

create DEF-HEAD DEF-MAX cells allot
create DEF-LO DEF-MAX cells allot
create DEF-HI DEF-MAX cells allot
create DEF-WRITES DEF-MAX cells allot
create DEF-GUARDS DEF-MAX cells allot

variable DEF-N
variable HITS
variable FOUND
variable NEWLY
variable CUR

: TOK$ ( n -- ptr u8 n )
   COMPILER-ID-SRC:TOKEN$ ;

: TOK-IS? ( n ptr u8 n -- bool ) {: k:n a:ptr u:n :}
   k TOK$ a u STR= ;

\ How many times the exact token appears in the half-open span.
: SPAN-COUNT ( n n ptr u8 n -- n ) {: b:n e:n a:ptr u:n :}
   0 HITS !
   e b ?do
      i a u TOK-IS? if HITS @ 1+ HITS ! then
   loop
   HITS @ ;

\ ---- the frozen guard bodies -------------------------------------------------

: GUARD-ROW ( n -- ) {: row:n :}
   row GUARD-FILE$ COMPILER-ID-SRC:SCAN-FILE
   s" a frozen structure guard still has the body the model was proved against" T-LABEL
   row GUARD-WORD$ COMPILER-ID-SRC:BODY$ row GUARD-BODY$ T$= ;

public

: GUARDS ( -- )
   GUARD-COUNT 0 ?do i GUARD-ROW loop ;

private

\ ---- who writes, and who guards ----------------------------------------------

: DEF-NAME$ ( n -- ptr u8 n ) {: d:n :}
   d cells DEF-HEAD + @ COMPILER-ID-SRC:DEF-NAME-AT$ ;

: COLLECT-DEFS ( -- )
   0 DEF-N !
   COMPILER-ID-SRC:TOKENS 0 ?do
      i COMPILER-ID-SRC:DEF-HEAD? if
         DEF-N @ DEF-MAX >= if E-CIS-STRUCT throw then
         i DEF-HEAD DEF-N @ cells + !
         i COMPILER-ID-SRC:DEF-SPAN-AT {: b:n e:n :}
         b DEF-LO DEF-N @ cells + !
         e DEF-HI DEF-N @ cells + !
         0 DEF-WRITES DEF-N @ cells + !
         0 DEF-GUARDS DEF-N @ cells + !
         DEF-N @ 1+ DEF-N !
      then
   loop ;

: FLAG@ ( n ptr a -- n ) {: d:n base:ptr :}
   base d cells + @ ;

: FLAG! ( n ptr a -- ) {: d:n base:ptr :}
   1 base d cells + ! ;

: BODY-HAS? ( n ptr u8 n -- bool ) {: d:n a:ptr u:n :}
   d cells DEF-LO + @ d cells DEF-HI + @ a u SPAN-COUNT 0 > ;

: SEED-FLAGS ( ptr u8 n ptr a -- ) {: a:ptr u:n base:ptr :}
   DEF-N @ 0 ?do
      i a u BODY-HAS? if i base FLAG! then
   loop ;

\ Does this token name a definition that already carries the flag?
: FLAGGED-NAME? ( ptr u8 n ptr a -- bool ) {: a:ptr u:n base:ptr :}
   DEF-N @ 0 ?do
      i base FLAG@ 0 <> if
         i DEF-NAME$ a u STR= if true unloop exit then
      then
   loop
   false ;

: CALLS-FLAGGED? ( n ptr a -- bool ) {: d:n base:ptr :}
   d cells DEF-LO + @ d cells DEF-HI + @ {: b:n e:n :}
   e b ?do
      i TOK$ base FLAGGED-NAME? if true unloop exit then
   loop
   false ;

: FLAG-PASS ( ptr a -- n ) {: base:ptr :}
   0 NEWLY !
   DEF-N @ 0 ?do
      i base FLAG@ 0= if
         i base CALLS-FLAGGED? if
            i base FLAG!
            NEWLY @ 1+ NEWLY !
         then
      then
   loop
   NEWLY @ ;

\ Close the flag under calls. Each pass flags at least one more definition or
\ none at all, so the definition count bounds the number of passes.
: FLAG-CLOSE ( ptr a -- ) {: base:ptr :}
   DEF-N @ 1+ 0 ?do
      base FLAG-PASS 0= if unloop exit then
   loop ;

: CLASSIFY ( ptr u8 n -- ) {: g:ptr gu:n :}
   COLLECT-DEFS
   PUSH-TOKEN$ DEF-WRITES SEED-FLAGS
   g gu DEF-GUARDS SEED-FLAGS
   DEF-WRITES FLAG-CLOSE
   DEF-GUARDS FLAG-CLOSE ;

: WRITER-TOKEN? ( n -- bool ) {: k:n :}
   k PUSH-TOKEN$ TOK-IS? if true exit then
   k TOK$ DEF-WRITES FLAGGED-NAME? ;

: GUARD-TOKEN? ( n ptr u8 n -- bool ) {: k:n a:ptr u:n :}
   k a u TOK-IS? if true exit then
   k TOK$ DEF-GUARDS FLAGGED-NAME? ;

: FIRST-WRITER ( n n -- n ) {: b:n e:n :}
   e FOUND !
   e b ?do
      i WRITER-TOKEN? if
         FOUND @ e = if i FOUND ! then
      then
   loop
   FOUND @ ;

: WRITER-COUNT ( n n -- n ) {: b:n e:n :}
   0 HITS !
   e b ?do
      i WRITER-TOKEN? if HITS @ 1+ HITS ! then
   loop
   HITS @ ;

: GUARD-COUNT-IN ( n n ptr u8 n -- n ) {: b:n e:n a:ptr u:n :}
   0 HITS !
   e b ?do
      i a u GUARD-TOKEN? if HITS @ 1+ HITS ! then
   loop
   HITS @ ;

: LATE-GUARDS ( n n n ptr u8 n -- n ) {: b:n e:n first:n a:ptr u:n :}
   0 HITS !
   e first ?do
      i a u GUARD-TOKEN? if HITS @ 1+ HITS ! then
   loop
   HITS @ ;

: BUILDER-ROW ( n n n -- ) {: b:n e:n row:n :}
   s" a builder still writes its rows through the arena" T-LABEL
   b e WRITER-COUNT 0 > TTRUE
   s" every guard a builder reaches runs before its first arena push" T-LABEL
   b e  b e FIRST-WRITER  row ORDER-GUARD$ LATE-GUARDS 0 T= ;

: READER-ROW ( n n -- ) {: b:n e:n :}
   s" a reader revalidates and never writes a cell" T-LABEL
   b e WRITER-COUNT 0 T= ;

: ORDER-ROW ( n -- ) {: row:n :}
   row ORDER-FILE$ COMPILER-ID-SRC:SCAN-FILE
   row ORDER-GUARD$ CLASSIFY
   row ORDER-WORD$ COMPILER-ID-SRC:BODY-SPAN {: b:n e:n :}
   s" the guard token is nowhere in the body, so a one-level scan sees none" T-LABEL
   b e row ORDER-GUARD$ SPAN-COUNT 0 T=
   s" closing the guard relation under calls still reaches that guard" T-LABEL
   b e row ORDER-GUARD$ GUARD-COUNT-IN 0 > TTRUE
   row ORDER-WRITES? if b e row BUILDER-ROW else b e READER-ROW then ;

public

: CALL-CLOSED ( -- )
   ORDER-COUNT 0 ?do i ORDER-ROW loop ;

private

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
\ carries everything it needs on the data stack and hands its answer back in the
\ slot it was given. The slot keeps the -1 it went in with when the step throws.

: STAGE-OPERANDS ( IR-ID:ir-module-key n -- ) {: key:IR-ID:ir-module-key st:n :}
   st SS-OPN@ 0 ?do
      key st SS-OPB@ i + OPD@ IR-ID:PACK-VALUE IR-OP:ADD-OPERAND
   loop ;

: STAGE-RESULTS ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key n -- )
   {: c:IR-CTX:ctx tp:IR-ARENA:arena tr:IR-ARENA:arena key:IR-ID:ir-module-key st:n :}
   st SS-RSN@ 0 ?do
      c tp tr key I64 IR-OP:ADD-RESULT
   loop ;

: SSA-TRY ( IR-CTX:ctx IR-ID:ir-module-key IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena n n -- IR-CTX:ctx IR-ID:ir-module-key IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena n n )
   {: c:IR-CTX:ctx key:IR-ID:ir-module-key sp:IR-ARENA:arena sr:IR-ARENA:arena tp:IR-ARENA:arena tr:IR-ARENA:arena ar:IR-ARENA:arena sa:IR-ARENA:arena qr:IR-ARENA:arena p:IR-ARENA:arena v:IR-ARENA:arena r:IR-ARENA:arena st:n slot:n :}
   c sp sr key K-VAR OPC-SYM IR-OP:BEGIN-OP
   c sa key A-SPAN IR-OP:SET-SPAN
   key st STAGE-OPERANDS
   c tp tr key st STAGE-RESULTS
   c p v r key qr tr ar sa IR-OP:END-OP IR-ID:OP-LOCAL {: got:n :}
   c key sp sr tp tr ar sa qr p v r st got ;

: SSA-STEP ( IR-CTX:ctx IR-ID:ir-module-key IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena n -- n n )
   -1 [: SSA-TRY ;] catch {: rc:n :}
   {: c:IR-CTX:ctx key:IR-ID:ir-module-key sp:IR-ARENA:arena sr:IR-ARENA:arena tp:IR-ARENA:arena tr:IR-ARENA:arena ar:IR-ARENA:arena sa:IR-ARENA:arena qr:IR-ARENA:arena p:IR-ARENA:arena v:IR-ARENA:arena r:IR-ARENA:arena st:n got:n :}
   rc got ;

\ ---- driving one block step --------------------------------------------------

: FILLER-OP ( IR-CTX:ctx IR-ID:ir-module-key IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena n -- )
   {: c:IR-CTX:ctx key:IR-ID:ir-module-key sp:IR-ARENA:arena sr:IR-ARENA:arena tp:IR-ARENA:arena tr:IR-ARENA:arena ar:IR-ARENA:arena sa:IR-ARENA:arena qr:IR-ARENA:arena p:IR-ARENA:arena v:IR-ARENA:arena r:IR-ARENA:arena k:n :}
   c sp sr key k OPC-SYM IR-OP:BEGIN-OP
   c sa key A-SPAN IR-OP:SET-SPAN
   c p v r key qr tr ar sa IR-OP:END-OP drop ;

: BLK-TRY ( IR-CTX:ctx IR-ID:ir-module-key IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena n -- IR-CTX:ctx IR-ID:ir-module-key IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena n )
   {: c:IR-CTX:ctx key:IR-ID:ir-module-key sp:IR-ARENA:arena sr:IR-ARENA:arena tp:IR-ARENA:arena tr:IR-ARENA:arena ar:IR-ARENA:arena sa:IR-ARENA:arena qr:IR-ARENA:arena p:IR-ARENA:arena v:IR-ARENA:arena r:IR-ARENA:arena fr:IR-ARENA:arena br:IR-ARENA:arena slot:n :}
   c sa key A-SPAN IR-FUN:SET-BLOCK-SPAN
   c br fr key v r qr sa IR-FUN:END-BLOCK IR-ID:BLOCK-LOCAL {: got:n :}
   c key sp sr tp tr ar sa qr p v r fr br got ;

: BLK-END ( IR-CTX:ctx IR-ID:ir-module-key IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena -- n n )
   -1 [: BLK-TRY ;] catch {: rc:n :}
   {: c:IR-CTX:ctx key:IR-ID:ir-module-key sp:IR-ARENA:arena sr:IR-ARENA:arena tp:IR-ARENA:arena tr:IR-ARENA:arena ar:IR-ARENA:arena sa:IR-ARENA:arena qr:IR-ARENA:arena p:IR-ARENA:arena v:IR-ARENA:arena r:IR-ARENA:arena fr:IR-ARENA:arena br:IR-ARENA:arena got:n :}
   rc got ;

\ ---- one step's two assertions -----------------------------------------------

: STEP-ANSWER ( n n n n -- ) {: class:n ord:n rc:n got:n :}
   s" the builder reaches the answer the shared vector row records" T-LABEL
   rc class T=
   class 0 <> if exit then
   s" an accepted step answers the ordinal the shared vector row records" T-LABEL
   got ord T= ;

\ ---- one value-store sequence ------------------------------------------------

: SSA-SEQ ( IR-CTX:ctx -- ) {: c:IR-CTX:ctx :}
   CUR @ {: s:n :}
   c s SCN-OCAP@ s SCN-VCAP@ s SCN-PCAP@ RIG
   {: key:IR-ID:ir-module-key sp:IR-ARENA:arena sr:IR-ARENA:arena tp:IR-ARENA:arena tr:IR-ARENA:arena ar:IR-ARENA:arena sa:IR-ARENA:arena qr:IR-ARENA:arena p:IR-ARENA:arena v:IR-ARENA:arena r:IR-ARENA:arena fp:IR-ARENA:arena fr:IR-ARENA:arena br:IR-ARENA:arena :}
   s SCN-LEN@ 0 ?do
      s SCN-BASE@ i + {: st:n :}
      c key sp sr tp tr ar sa qr p v r st SSA-STEP {: rc:n got:n :}
      st SS-CLASS@ st SS-ORD@ rc got STEP-ANSWER
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

\ ---- the table cannot quietly lose a case ------------------------------------

: ROLE-COVERED? ( n -- bool ) {: role:n :}
   SCENARIOS 0 ?do
      i SCN-ROLE@ role = if true unloop exit then
   loop
   false ;

: KIND-COVERED? ( n -- bool ) {: kind:n :}
   SCENARIOS 0 ?do
      i SCN-KIND@ kind = if true unloop exit then
   loop
   false ;

public

\ The sequence table carries no digest, so this is its freeze. A role that stops
\ being covered or a store that stops being driven fails here rather than
\ quietly shrinking what the gate asks.
: COVERAGE ( -- )
   ROLE-COUNT 0 ?do
      s" every frozen sequence role is covered by a vector row" T-LABEL
      i ROLE-COVERED? TTRUE
   loop
   KIND-COUNT 0 ?do
      s" every store the model covers is driven by a vector row" T-LABEL
      i KIND-COVERED? TTRUE
   loop ;

: VECTORS ( -- )
   SCENARIOS 0 ?do
      i CUR !
      BND [: SEQ-BODY ;] IR-CTX:WITH-CONTEXT
   loop ;

: HABU-SIDE ( -- )
   GUARDS
   CALL-CLOSED
   COVERAGE
   VECTORS ;

;using
;package
