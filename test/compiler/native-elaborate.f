\ native-elaborate.f - checked straight-line elaborator tests.
\
\ Proves the section 7.2 contract of src/compiler/native/elaborate.f: a sealed
\ source tape holding one colon definition becomes a function of real HIR
\ operations in a module that freezes - which runs the whole structural
\ verifier - and every operation, operand and result reads back off the
\ published module.
\
\ THE MEASUREMENT THIS SUITE EXISTS FOR. `SQUARE dup *` must contain exactly
\ two operations, a multiply and a return, because `dup` is a compile-time
\ rename and costs nothing at all. The count is asserted, not described, and so
\ is the multiply's operand list: both operands are the same block argument,
\ which is what "the rename produced no operation and no value" means when it is
\ written down. `rot -` and `nip -` make the same measurement for the two deeper
\ renames, and they make it with a subtraction, whose operands cannot be
\ exchanged without changing the answer, so the order a rename puts values back
\ in is proved rather than described.
\
\ WHY NO FIXTURE HERE IS SPELLED `: NAME … ;`. The elaborator reads the tape a
\ real compilation produces, and the engine hands the checker's reader the
\ definition it RECONSTRUCTED - no opening `:`, no closing `;`. The frame is not
\ a pair of spellings on the tape; it is the recorded parser mode. So each
\ fixture's text is the name followed by the body, the name is the one token
\ marked interpreting, and every body token is marked compiling - exactly the
\ grid test/compiler/native-feed.f measures on a definition the engine really
\ compiled. test/compiler/native-chain.f then runs one produced tape all the way
\ through, so this suite may state that shape and that one proves it.
\
\ HOW A FIXTURE IS BUILT. Each one states its source text, and the shared source
\ fixture test/compiler/native-source-fixture.f lexes it onto a tape: one token
\ per word, spans that are real ranges in that text, and the parser mode each
\ token would really have been read in. That file is shared with the code
\ generator comparison harness, so both harnesses agree about what a token is.
\ The hostile fixtures below push their tokens by hand through the same writer,
\ because the thing under test is a token the lexer would never produce.
\
\ WHICH REFUSAL BELONGS TO WHOM. The elaborator names five refusals of its own -
\ the shape of a definition, the parser mode of a token, the declared arity, and
\ the two ends of the value vector. Every other refusal here is another
\ authority's and keeps that authority's name: a body word the dialect cannot
\ compile is E-HIR-UNMODELED, a token kind the subset does not model is
\ E-HIR-KIND, and a tape of another module is E-NTAPE-OWNER.

require lib/test.f
require src/compiler/native/elaborate.f
require test/compiler/native-source-fixture.f

package NELAB-TEST
private

\ ---- bindings ----------------------------------------------------------------
\ The AArch64 Darwin fixture binding the other compiler suites use.
: BND ( -- CBIND:binding )
   CTARGET-ARCH:AARCH64 CTARGET-ABI:AAPCS64-DARWIN CTARGET-ENDIAN:LITTLE
   CTARGET-PTR--WIDTH:BITS64
   CTARGET:F-BASE CTARGET:F-FP CTARGET:WITH CTARGET:CONTRACT
   CNUM-OVERFLOW:TRAP CNUM-FLOAT--MODEL:IEEE754 CNUM-CONTRACTION:FORBIDDEN
   CNUM-FAST--MATH:BIT-EXACT CNUM-COMPARE:IEEE754-UNORDERED CNUM:POLICY
   CBIND:BIND ;

\ ---- the shared source fixture -----------------------------------------------
\ The text buffer, the tape writer and the lexer live in
\ test/compiler/native-source-fixture.f. Importing its public words lets every
\ fixture below read the way it always did.
using NSRC

\ Everything a fixture compiles with, up to but not including the tokens: the
\ module with its dialect and word model, and a tape bound to the text already
\ in the fixture's buffer.
: RIG ( IR-CTX:ctx -- IR-BUILD:builder IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena )
   {: c:IR-CTX:ctx :}
   c HIR-BUILDER {: b:IR-BUILD:builder :}
   c b MODEL {: p:IR-ARENA:arena r:IR-ARENA:arena :}
   c b TAPE {: tp:IR-ARENA:arena :}
   b p r tp ;

\ The same rig with the text lexed onto the tape and the tape sealed.
: SEALED ( IR-CTX:ctx -- IR-BUILD:builder IR-ARENA:arena IR-ARENA:arena IR-ARENA:view )
   {: c:IR-CTX:ctx :}
   c RIG
   {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena tp:IR-ARENA:arena :}
   c LEX
   b p r  tp NTAPE:SEAL ;

\ ---- reading the published module --------------------------------------------
: F-BLK ( IR-BUILD:module IR-ID:ir-fun-id -- IR-ID:ir-block-id )
   {: m:IR-BUILD:module f:IR-ID:ir-fun-id :}
   m IR-BUILD:FFUN-ROWS m IR-BUILD:FBLOCK-ROWS m IR-BUILD:FKEY f 0
   IR-FUN:FBLOCK@ ;

: F-OPS ( IR-BUILD:module IR-ID:ir-block-id -- n )
   {: m:IR-BUILD:module blk:IR-ID:ir-block-id :}
   m IR-BUILD:FBLOCK-ROWS blk IR-FUN:FOP-COUNT ;

: F-OP ( IR-BUILD:module IR-ID:ir-block-id n -- IR-ID:ir-op-id )
   {: m:IR-BUILD:module blk:IR-ID:ir-block-id i:n :}
   m IR-BUILD:FBLOCK-ROWS m IR-BUILD:FOP-ROWS m IR-BUILD:FKEY blk i
   IR-FUN:FOP@ ;

: F-ARG ( IR-BUILD:module IR-ID:ir-block-id n -- IR-ID:ir-value-id )
   {: m:IR-BUILD:module blk:IR-ID:ir-block-id i:n :}
   m IR-BUILD:FBLOCK-ROWS m IR-BUILD:FVALUE-ROWS m IR-BUILD:FKEY blk i
   IR-FUN:FARG@ ;

: F-ARGS ( IR-BUILD:module IR-ID:ir-block-id -- n )
   {: m:IR-BUILD:module blk:IR-ID:ir-block-id :}
   m IR-BUILD:FBLOCK-ROWS blk IR-FUN:FARG-COUNT ;

: F-OPC? ( IR-BUILD:module IR-ID:ir-op-id ptr u8 n -- bool )
   {: m:IR-BUILD:module op:IR-ID:ir-op-id a u:n :} \ typed-local-lint: allow-bare-local - a keeps the ptr u8 byte-span role
   m IR-BUILD:FSYM-POOL m IR-BUILD:FSYM-ROWS
   m IR-BUILD:FOP-ROWS m IR-BUILD:FKEY op IR-OP:FOPCODE@
   a u IR-SYM:FEQ? ;

: F-IN ( IR-BUILD:module IR-ID:ir-op-id n -- IR-ID:ir-value-id )
   {: m:IR-BUILD:module op:IR-ID:ir-op-id i:n :}
   m IR-BUILD:FOP-POOL m IR-BUILD:FOP-ROWS m IR-BUILD:FKEY op i
   IR-OP:FOPERAND@ ;

: F-INS ( IR-BUILD:module IR-ID:ir-op-id -- n )
   {: m:IR-BUILD:module op:IR-ID:ir-op-id :}
   m IR-BUILD:FOP-ROWS op IR-OP:FOPERANDS ;

: F-OUT ( IR-BUILD:module IR-ID:ir-op-id n -- IR-ID:ir-value-id )
   {: m:IR-BUILD:module op:IR-ID:ir-op-id i:n :}
   m IR-BUILD:FOP-POOL m IR-BUILD:FOP-ROWS m IR-BUILD:FKEY op i
   IR-OP:FRESULT@ ;

: F-ATTR ( IR-BUILD:module IR-ID:ir-op-id n -- n )
   {: m:IR-BUILD:module op:IR-ID:ir-op-id i:n :}
   m IR-BUILD:FATTR-ROWS
   m IR-BUILD:FOP-POOL m IR-BUILD:FOP-ROWS m IR-BUILD:FKEY op i IR-OP:FATTR@
   IR-ATTR:FINT@ ;

: F-TOTAL ( IR-BUILD:module -- n )
   IR-BUILD:FOP-ROWS IR-OP:FOPS ;

: F-VALUES ( IR-BUILD:module -- n )
   IR-BUILD:FVALUE-ROWS IR-OP:FVALUES ;

: SAME? ( IR-ID:ir-value-id IR-ID:ir-value-id -- bool )
   {: x:IR-ID:ir-value-id y:IR-ID:ir-value-id :}
   x IR-ID:VALUE-LOCAL y IR-ID:VALUE-LOCAL = ;

\ How many blocks the function has, which block a terminator's successor names,
\ and how many successors it has. The control-flow cases below are about exactly
\ these three, so they are read off the published module and not described.
: F-BLKS ( IR-BUILD:module IR-ID:ir-fun-id -- n )
   {: m:IR-BUILD:module f:IR-ID:ir-fun-id :}
   m IR-BUILD:FFUN-ROWS f IR-FUN:FBLOCK-COUNT ;

: F-BLK-AT ( IR-BUILD:module IR-ID:ir-fun-id n -- IR-ID:ir-block-id )
   {: m:IR-BUILD:module f:IR-ID:ir-fun-id i:n :}
   m IR-BUILD:FFUN-ROWS m IR-BUILD:FBLOCK-ROWS m IR-BUILD:FKEY f i
   IR-FUN:FBLOCK@ ;

: F-TERM ( IR-BUILD:module IR-ID:ir-block-id -- IR-ID:ir-op-id )
   {: m:IR-BUILD:module blk:IR-ID:ir-block-id :}
   m IR-BUILD:FBLOCK-ROWS m IR-BUILD:FOP-ROWS m IR-BUILD:FKEY blk
   IR-FUN:FTERMINATOR@ ;

: F-SUCCS ( IR-BUILD:module IR-ID:ir-op-id -- n )
   {: m:IR-BUILD:module op:IR-ID:ir-op-id :}
   m IR-BUILD:FOP-ROWS op IR-OP:FSUCCESSORS ;

: F-SUCC ( IR-BUILD:module IR-ID:ir-op-id n -- n )
   {: m:IR-BUILD:module op:IR-ID:ir-op-id i:n :}
   m IR-BUILD:FOP-POOL m IR-BUILD:FOP-ROWS m IR-BUILD:FKEY op i
   IR-OP:FSUCCESSOR@ IR-ID:BLOCK-LOCAL ;

\ ---- a rename-heavy word: the op-count proof ---------------------------------
\ `dup` consumes the one input and puts it back twice, so the multiply's two
\ operands are the same block argument and no operation is staged for the rename.
\ Two operations, two values - the argument and the product - and nothing else.
: SQUARE-BODY ( IR-CTX:ctx -- n bool bool bool bool bool n n )
   {: c:IR-CTX:ctx :}
   s" SQUARE dup *" TEXT!
   c SEALED
   {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena v:IR-ARENA:view :}
   c b v p r 1 1 NELAB:COLON {: f:IR-ID:ir-fun-id :}
   c b IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   m f F-BLK {: blk:IR-ID:ir-block-id :}
   m blk F-OPS
   m blk 0 F-OP {: mul:IR-ID:ir-op-id :}
   m blk 1 F-OP {: ret:IR-ID:ir-op-id :}
   m mul s" hir.mul" F-OPC?
   m ret s" hir.return" F-OPC?
   m mul 0 F-IN  m blk 0 F-ARG SAME?
   m mul 1 F-IN  m blk 0 F-ARG SAME?
   m ret 0 F-IN  m mul 0 F-OUT SAME?
   m F-VALUES
   m F-TOTAL ;

: SQUARE-CASE ( -- )
   s" a rename-heavy word compiles to exactly the operations its values need" T-LABEL
   BND [: SQUARE-BODY ;] IR-CTX:WITH-CONTEXT
   2 T= 2 T= TTRUE TTRUE TTRUE TTRUE TTRUE 2 T= ;

\ ---- a literal and an operation ----------------------------------------------
\ Three operations: the constant carrying its value as the attribute the opcode's
\ schema requires, the addition of the argument and that constant, and the
\ return.
: INC-BODY ( IR-CTX:ctx -- n bool bool bool n bool bool )
   {: c:IR-CTX:ctx :}
   s" INC5 5 +" TEXT!
   c SEALED
   {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena v:IR-ARENA:view :}
   c b v p r 1 1 NELAB:COLON {: f:IR-ID:ir-fun-id :}
   c b IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   m f F-BLK {: blk:IR-ID:ir-block-id :}
   m blk F-OPS
   m blk 0 F-OP {: k:IR-ID:ir-op-id :}
   m blk 1 F-OP {: add:IR-ID:ir-op-id :}
   m k s" hir.const" F-OPC?
   m add s" hir.add" F-OPC?
   m  m blk 2 F-OP  s" hir.return" F-OPC?
   m k 0 F-ATTR
   m add 0 F-IN  m blk 0 F-ARG SAME?
   m add 1 F-IN  m k 0 F-OUT SAME? ;

: INC-CASE ( -- )
   s" a literal and an arithmetic word compile to a constant and an addition" T-LABEL
   BND [: INC-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE 5 T= TTRUE TTRUE TTRUE 3 T= ;

\ ---- a word that reads and writes memory -------------------------------------
\ The corpus's cell-bump body, `A ! A @ 1+ dup A !`, with A a `create`d data
\ word the model is told the address of. What this case measures is the two
\ things the printed operation list cannot show on its own.
\
\ THE OPERAND ORDER OF A STORE. Forth writes `value address !`, so the value is
\ the deeper of the two and therefore the store's FIRST operand and the address
\ its second. The two are both cells, so an elaborator that exchanged them would
\ build a module that verifies, allocates and emits - and writes the cell's
\ address into whatever the value happens to point at. Here each one is compared
\ against the value it has to be: the block argument, and the constant the data
\ word became.
\
\ THE ORDER ITSELF, LINK BY LINK. `hir.mem` is the FIRST operation of the entry
\ block, because a definition that touches memory mints its order where every
\ block of it can see the value - and every access after it takes the order the
\ access before it answered. The chain is asserted as identities - the
\ store's order operand is the mint's result, the load's is the store's result,
\ the second store's is the load's second result - so an access that took an
\ older order, or a fresh one, is a different VALUE here rather than a different
\ printed order.
4096 constant CELL-A-ADDR

: SEALED-DATA ( IR-CTX:ctx -- IR-BUILD:builder IR-ARENA:arena IR-ARENA:arena IR-ARENA:view )
   {: c:IR-CTX:ctx :}
   c HIR-BUILDER {: b:IR-BUILD:builder :}
   c b s" CELL-A" CELL-A-ADDR MODEL-DATA
   {: p:IR-ARENA:arena r:IR-ARENA:arena :}
   c b TAPE {: tp:IR-ARENA:arena :}
   c LEX
   b p r  tp NTAPE:SEAL ;

: BUMP-BODY ( IR-CTX:ctx -- n bool bool bool bool bool bool bool bool bool bool )
   {: c:IR-CTX:ctx :}
   s" BUMP CELL-A ! CELL-A @ 1+ dup CELL-A !" TEXT!
   c SEALED-DATA
   {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena v:IR-ARENA:view :}
   c b v p r 1 1 NELAB:COLON {: f:IR-ID:ir-fun-id :}
   c b IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   m f F-BLK {: blk:IR-ID:ir-block-id :}
   m blk 0 F-OP {: mem:IR-ID:ir-op-id :}
   m blk 1 F-OP {: a0:IR-ID:ir-op-id :}
   m blk 2 F-OP {: st0:IR-ID:ir-op-id :}
   m blk 3 F-OP {: a1:IR-ID:ir-op-id :}
   m blk 4 F-OP {: ld:IR-ID:ir-op-id :}
   m blk 7 F-OP {: a2:IR-ID:ir-op-id :}
   m blk 8 F-OP {: st1:IR-ID:ir-op-id :}
   m blk F-OPS
   m mem s" hir.mem" F-OPC?
   m st0 s" hir.store" F-OPC?
   m ld s" hir.load" F-OPC?
   m a2 s" hir.const" F-OPC?
   m st0 0 F-IN  m blk 0 F-ARG SAME?
   m st0 1 F-IN  m a0 0 F-OUT SAME?
   m st0 2 F-IN  m mem 0 F-OUT SAME?
   m ld 0 F-IN  m a1 0 F-OUT SAME?
   m ld 1 F-IN  m st0 0 F-OUT SAME?
   m st1 2 F-IN  m ld 1 F-OUT SAME? ;

: BUMP-CASE ( -- )
   s" a store and a load compile to one order, threaded link by link" T-LABEL
   BND [: BUMP-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE
   TTRUE TTRUE TTRUE TTRUE 10 T= ;

\ ---- a word with two outputs -------------------------------------------------
\ `over + swap` leaves the sum and the first input, in that order. Two
\ operations: three renames between them stage nothing, and the return hands
\ both outputs over bottom first.
: SUMA-BODY ( IR-CTX:ctx -- n n n bool bool )
   {: c:IR-CTX:ctx :}
   s" SUMA over + swap" TEXT!
   c SEALED
   {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena v:IR-ARENA:view :}
   c b v p r 2 2 NELAB:COLON {: f:IR-ID:ir-fun-id :}
   c b IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   m f F-BLK {: blk:IR-ID:ir-block-id :}
   m blk F-ARGS
   m blk F-OPS
   m blk 0 F-OP {: add:IR-ID:ir-op-id :}
   m blk 1 F-OP {: ret:IR-ID:ir-op-id :}
   m ret F-INS
   m ret 0 F-IN  m add 0 F-OUT SAME?
   m ret 1 F-IN  m blk 0 F-ARG SAME? ;

: SUMA-CASE ( -- )
   s" a word with two outputs returns both of them, bottom first" T-LABEL
   BND [: SUMA-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE 2 T= 2 T= 2 T= ;

\ ---- operand order -----------------------------------------------------------
\ Subtraction is not commutative, so the order the values enter the operand list
\ is observable: `swap -` subtracts the first input from the second.
: DIFF-BODY ( IR-CTX:ctx -- n bool bool bool )
   {: c:IR-CTX:ctx :}
   s" DIFF swap -" TEXT!
   c SEALED
   {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena v:IR-ARENA:view :}
   c b v p r 2 1 NELAB:COLON {: f:IR-ID:ir-fun-id :}
   c b IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   m f F-BLK {: blk:IR-ID:ir-block-id :}
   m blk F-OPS
   m blk 0 F-OP {: sub:IR-ID:ir-op-id :}
   m sub s" hir.sub" F-OPC?
   m sub 0 F-IN  m blk 1 F-ARG SAME?
   m sub 1 F-IN  m blk 0 F-ARG SAME? ;

: DIFF-CASE ( -- )
   s" a rename decides which value is which operand" T-LABEL
   BND [: DIFF-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE TTRUE 2 T= ;

\ ---- the three-value rotation ------------------------------------------------
\ `rot` ( a b c -- b c a ) is the rename where a wrong order is easiest to write
\ and hardest to see, so this fixture is built so that only the right one passes.
\ The body is `rot -`: after the rotation the vector holds b c a, the subtraction
\ takes the top two with the deeper one as its first operand, and subtraction is
\ not commutative - so its operands are c and then a, and the word returns b and
\ the difference. Every other rotation of three values puts a different pair of
\ block arguments into that operand list, so skewing any one pick index in the
\ declaration reds this case rather than computing the same answer another way.
\ And the rotation itself costs nothing: two operations, the subtraction and the
\ return, is the whole function.
: ROT3-BODY ( IR-CTX:ctx -- n bool bool bool n bool bool n n )
   {: c:IR-CTX:ctx :}
   s" ROT3 rot -" TEXT!
   c SEALED
   {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena v:IR-ARENA:view :}
   c b v p r 3 2 NELAB:COLON {: f:IR-ID:ir-fun-id :}
   c b IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   m f F-BLK {: blk:IR-ID:ir-block-id :}
   m blk F-OPS
   m blk 0 F-OP {: sub:IR-ID:ir-op-id :}
   m blk 1 F-OP {: ret:IR-ID:ir-op-id :}
   m sub s" hir.sub" F-OPC?
   m sub 0 F-IN  m blk 2 F-ARG SAME?
   m sub 1 F-IN  m blk 0 F-ARG SAME?
   m ret F-INS
   m ret 0 F-IN  m blk 1 F-ARG SAME?
   m ret 1 F-IN  m sub 0 F-OUT SAME?
   m F-VALUES
   m F-TOTAL ;

: ROT3-CASE ( -- )
   s" rot rotates three values and adds no operation at all" T-LABEL
   BND [: ROT3-BODY ;] IR-CTX:WITH-CONTEXT
   2 T= 4 T= TTRUE TTRUE 2 T= TTRUE TTRUE TTRUE 2 T= ;

\ ---- dropping the value underneath -------------------------------------------
\ `nip` ( a b -- b ) consumes two and puts back only the one that was on top, so
\ in `nip -` the middle input disappears and the subtraction is between the first
\ input and the third. Putting back the other consumed value instead would
\ subtract the second input, which is a different operand list, so the single
\ pick index is pinned here too. Two operations again: the rename adds none.
: NDIF-BODY ( IR-CTX:ctx -- n bool bool bool bool n n )
   {: c:IR-CTX:ctx :}
   s" NDIF nip -" TEXT!
   c SEALED
   {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena v:IR-ARENA:view :}
   c b v p r 3 1 NELAB:COLON {: f:IR-ID:ir-fun-id :}
   c b IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   m f F-BLK {: blk:IR-ID:ir-block-id :}
   m blk F-OPS
   m blk 0 F-OP {: sub:IR-ID:ir-op-id :}
   m blk 1 F-OP {: ret:IR-ID:ir-op-id :}
   m sub s" hir.sub" F-OPC?
   m sub 0 F-IN  m blk 0 F-ARG SAME?
   m sub 1 F-IN  m blk 2 F-ARG SAME?
   m ret 0 F-IN  m sub 0 F-OUT SAME?
   m F-VALUES
   m F-TOTAL ;

: NDIF-CASE ( -- )
   s" nip drops the value underneath and adds no operation at all" T-LABEL
   BND [: NDIF-BODY ;] IR-CTX:WITH-CONTEXT
   2 T= 4 T= TTRUE TTRUE TTRUE TTRUE 2 T= ;

\ ---- a definition whose body is empty ----------------------------------------
\ `: PASS ( n -- n ) ;` records ONE token, the name, and nothing else. There is
\ no closing token to walk to, so this is the case that proves the tape's end is
\ what ends the body: an elaborator still looking for a frame word would run off
\ the tape here instead of returning the argument it was given. One operation,
\ one value, and the return hands back the block argument itself.
: PASS-BODY ( IR-CTX:ctx -- n n bool n n )
   {: c:IR-CTX:ctx :}
   s" PASS" TEXT!
   c SEALED
   {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena v:IR-ARENA:view :}
   v NTAPE:TOKENS {: toks:n :}
   c b v p r 1 1 NELAB:COLON {: f:IR-ID:ir-fun-id :}
   c b IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   m f F-BLK {: blk:IR-ID:ir-block-id :}
   toks
   m blk F-OPS
   m  m blk 0 F-OP  0 F-IN  m blk 0 F-ARG SAME?
   m blk F-ARGS
   m F-VALUES ;

: PASS-CASE ( -- )
   s" a definition with an empty body returns its argument and ends at the tape" T-LABEL
   BND [: PASS-BODY ;] IR-CTX:WITH-CONTEXT
   1 T= 1 T= TTRUE 1 T= 1 T= ;

\ ---- the published function --------------------------------------------------
\ The definition became a function named as the source names it, with the
\ declared effect, the spans the tape recorded, and one entry block. The block's
\ span is the name's, which is where a produced tape puts the definition's
\ identity: `SQUARE` is the first six bytes of the text.
: FUN-BODY ( IR-CTX:ctx -- bool n n n n )
   {: c:IR-CTX:ctx :}
   s" SQUARE dup *" TEXT!
   c SEALED
   {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena v:IR-ARENA:view :}
   c b v p r 1 1 NELAB:COLON {: f:IR-ID:ir-fun-id :}
   c b IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   m IR-BUILD:FFUN-ROWS {: fv:IR-ARENA:view :}
   m IR-BUILD:FSYM-POOL m IR-BUILD:FSYM-ROWS
      fv m IR-BUILD:FKEY f IR-FUN:FSYMBOL@  s" SQUARE" IR-SYM:FEQ?
   fv f IR-FUN:FBLOCK-COUNT
   m f F-BLK {: blk:IR-ID:ir-block-id :}
   m blk F-ARGS
   m IR-BUILD:FBLOCK-ROWS m IR-BUILD:FKEY blk IR-FUN:FBLOCK-SPAN@
   IR--SOURCE-SPAN:UNMAKE {: sid:IR-ID:ir-source-id st:n ln:n :}
   st ln ;

: FUN-CASE ( -- )
   s" the definition publishes one function, named and spanned by its source" T-LABEL
   BND [: FUN-BODY ;] IR-CTX:WITH-CONTEXT
   6 T= 0 T= 1 T= 1 T= TTRUE ;

\ ---- refusals: what the elaborator will not compile --------------------------
\ A word the model never declared. To checked source this is the same event as a
\ declared boundary, and it carries the word model's own name for it. The
\ spelling is `xor`, not a stack word: the subset's five opcodes are a closed
\ family with no bitwise operation in it, so modeling `xor` would mean a new
\ opcode with an elaboration and a lowering behind it, while a new stack word is
\ only another rename row. That keeps this fixture testing an undeclared word
\ even as the rename vocabulary grows, which is how `rot` stopped being usable
\ here.
: UNDEC-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   s" BAD negate" TEXT!
   c SEALED
   {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena v:IR-ARENA:view :}
   c b v p r 1 1 NELAB:COLON drop ;

: UNDEC ( -- )
   BND [: UNDEC-BODY ;] IR-CTX:WITH-CONTEXT ;

\ An arithmetic word with one value under it.
: UNDER-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   s" BAD +" TEXT!
   c SEALED
   {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena v:IR-ARENA:view :}
   c b v p r 1 1 NELAB:COLON drop ;

: UNDER ( -- )
   BND [: UNDER-BODY ;] IR-CTX:WITH-CONTEXT ;

\ A rename that consumes more values than the vector holds. It stages no
\ operation, so this refusal can only come from the value vector itself.
: RENAME-UNDER-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   s" BAD over" TEXT!
   c SEALED
   {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena v:IR-ARENA:view :}
   c b v p r 1 1 NELAB:COLON drop ;

: RENAME-UNDER ( -- )
   BND [: RENAME-UNDER-BODY ;] IR-CTX:WITH-CONTEXT ;

\ A body deeper than the elaborator's value vector. The ceiling refuses; it does
\ not wrap, and it does not overwrite the vector.
: DEEP-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   s" DEEP" TEXT!
   65 0 ?do s"  7" TEXT+ loop
   c SEALED
   {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena v:IR-ARENA:view :}
   c b v p r 0 1 NELAB:COLON drop ;

: DEEP ( -- )
   BND [: DEEP-BODY ;] IR-CTX:WITH-CONTEXT ;

\ A body that leaves more values than the word declares.
: WIDE-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   s" BAD dup" TEXT!
   c SEALED
   {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena v:IR-ARENA:view :}
   c b v p r 1 1 NELAB:COLON drop ;

: WIDE ( -- )
   BND [: WIDE-BODY ;] IR-CTX:WITH-CONTEXT ;

\ A body that leaves fewer values than the word declares: `drop` empties the
\ vector, so the return has nothing to hand over. An arity the caller states and
\ a body that disagrees is refused at either end, not only when there are too
\ many values.
: NARROW-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   s" BAD drop" TEXT!
   c SEALED
   {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena v:IR-ARENA:view :}
   c b v p r 1 1 NELAB:COLON drop ;

: NARROW ( -- )
   BND [: NARROW-BODY ;] IR-CTX:WITH-CONTEXT ;

: NEGARITY-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   s" BAD dup *" TEXT!
   c SEALED
   {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena v:IR-ARENA:view :}
   c b v p r -1 1 NELAB:COLON drop ;

: NEGARITY ( -- )
   BND [: NEGARITY-BODY ;] IR-CTX:WITH-CONTEXT ;

\ A sealed tape with no tokens at all. There is no name, so there is no
\ definition, and the elaborator says so before it opens a function.
: EMPTY-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   s" BAD dup *" TEXT!
   c RIG
   {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena tp:IR-ARENA:arena :}
   c b  tp NTAPE:SEAL  p r 1 1 NELAB:COLON drop ;

: EMPTY ( -- )
   BND [: EMPTY-BODY ;] IR-CTX:WITH-CONTEXT ;

\ A tape of one module presented to another module's builder. Every identity the
\ tape holds carries its owning module, so the tape's own reader refuses the
\ foreign key before the elaborator has read a single token.
: FOREIGN-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   s" SQUARE dup *" TEXT!
   c SEALED
   {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena v:IR-ARENA:view :}
   c HIR-BUILDER {: b2:IR-BUILD:builder :}
   c b2 v p r 1 1 NELAB:COLON drop ;

: FOREIGN ( -- )
   BND [: FOREIGN-BODY ;] IR-CTX:WITH-CONTEXT ;

\ ---- refusals a lexer would never produce ------------------------------------
\ A string literal in the body: a token kind the straight-line subset does not
\ model, refused as such rather than resolved as a name.
: STRTOK-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   s" BAD x" TEXT!
   c RIG
   {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena tp:IR-ARENA:arena :}
   c 0 3 NTAPE-MODE:INTERPRETING NAME,
   c 4 1 NTAPE-MODE:COMPILING STR,
   c b  tp NTAPE:SEAL  p r 1 1 NELAB:COLON drop ;

: STRTOK ( -- )
   BND [: STRTOK-BODY ;] IR-CTX:WITH-CONTEXT ;

\ A body word the tape says was read while the parser was interpreting. Inside a
\ colon body the parser is compiling, so the tape and the definition disagree and
\ the elaborator refuses instead of choosing one of them.
: BADMODE-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   s" BAD dup *" TEXT!
   c RIG
   {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena tp:IR-ARENA:arena :}
   c 0 3 NTAPE-MODE:INTERPRETING NAME,
   c 4 3 NTAPE-MODE:INTERPRETING NAME,
   c 8 1 NTAPE-MODE:COMPILING NAME,
   c b  tp NTAPE:SEAL  p r 1 1 NELAB:COLON drop ;

: BADMODE ( -- )
   BND [: BADMODE-BODY ;] IR-CTX:WITH-CONTEXT ;

\ A second interpreting row deeper inside the body. This is the tape a producer
\ would make if it blended two definitions, and it is the case that says the
\ frame is the MODE and not the position: the elaborator does not stop at the
\ first row it could read as a name, it requires every later row to have been
\ read while compiling.
: SECOND-NAME-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   s" BAD dup * ONE" TEXT!
   c RIG
   {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena tp:IR-ARENA:arena :}
   c 0 3 NTAPE-MODE:INTERPRETING NAME,
   c 4 3 NTAPE-MODE:COMPILING NAME,
   c 8 1 NTAPE-MODE:COMPILING NAME,
   c 10 3 NTAPE-MODE:INTERPRETING NAME,
   c b  tp NTAPE:SEAL  p r 1 1 NELAB:COLON drop ;

: SECOND-NAME ( -- )
   BND [: SECOND-NAME-BODY ;] IR-CTX:WITH-CONTEXT ;

\ A name the tape says was read while compiling. `:` runs from the outer
\ interpreter and parses the name before switching, so this tape is not
\ describing a top-level definition.
: NAMEMODE-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   s" BAD dup *" TEXT!
   c RIG
   {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena tp:IR-ARENA:arena :}
   c 0 3 NTAPE-MODE:COMPILING NAME,
   c 4 3 NTAPE-MODE:COMPILING NAME,
   c 8 1 NTAPE-MODE:COMPILING NAME,
   c b  tp NTAPE:SEAL  p r 1 1 NELAB:COLON drop ;

: NAMEMODE ( -- )
   BND [: NAMEMODE-BODY ;] IR-CTX:WITH-CONTEXT ;

\ A definition whose name is an integer literal rather than a name.
: LITNAME-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   s" 12 dup *" TEXT!
   c RIG
   {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena tp:IR-ARENA:arena :}
   c 0 2 NTAPE-MODE:INTERPRETING 12 INT,
   c 3 3 NTAPE-MODE:COMPILING NAME,
   c 7 1 NTAPE-MODE:COMPILING NAME,
   c b  tp NTAPE:SEAL  p r 1 1 NELAB:COLON drop ;

: LITNAME ( -- )
   BND [: LITNAME-BODY ;] IR-CTX:WITH-CONTEXT ;

\ ---- cases -------------------------------------------------------------------
\ A refused elaboration leaves its context standing - the throw skips the
\ teardown - so its arenas and the operation stage its builder still holds are
\ only reclaimed when the enclosing context exits. Every refusal therefore gets
\ an enclosing context of its own, one case at a time, the way
\ test/compiler/ir-verify.f arranges the same problem.
: UNDEC-CASE ( -- )
   s" a body word the dialect never modeled is refused by name" T-LABEL
   [: UNDEC ;] E-HIR-UNMODELED TTHROWSQ ;

: STRTOK-CASE ( -- )
   s" a string literal in the body is refused as a kind the subset has no model for" T-LABEL
   [: STRTOK ;] E-HIR-KIND TTHROWSQ ;

: UNDER-CASE ( -- )
   s" an operation with too few values under it is refused" T-LABEL
   [: UNDER ;] E-NELAB-UNDER TTHROWSQ ;

: RENAME-UNDER-CASE ( -- )
   s" a rename with too few values under it is refused" T-LABEL
   [: RENAME-UNDER ;] E-NELAB-UNDER TTHROWSQ ;

: DEEP-CASE ( -- )
   s" a body deeper than the value vector is refused" T-LABEL
   [: DEEP ;] E-NELAB-CAP TTHROWSQ ;

: WIDE-CASE ( -- )
   s" a body leaving more values than the word declares is refused" T-LABEL
   [: WIDE ;] E-NELAB-ARITY TTHROWSQ ;

: NARROW-CASE ( -- )
   s" a body leaving fewer values than the word declares is refused" T-LABEL
   [: NARROW ;] E-NELAB-ARITY TTHROWSQ ;

: NEGARITY-CASE ( -- )
   s" a negative declared input count is refused" T-LABEL
   [: NEGARITY ;] E-NELAB-ARITY TTHROWSQ ;

: EMPTY-CASE ( -- )
   s" a tape with no tokens holds no definition and is refused" T-LABEL
   [: EMPTY ;] E-NELAB-SHAPE TTHROWSQ ;

: LITNAME-CASE ( -- )
   s" a definition named by an integer literal is refused" T-LABEL
   [: LITNAME ;] E-NELAB-SHAPE TTHROWSQ ;

: FOREIGN-CASE ( -- )
   s" a tape of another module is refused before a token is read" T-LABEL
   [: FOREIGN ;] E-NTAPE-OWNER TTHROWSQ ;

: BADMODE-CASE ( -- )
   s" a body word the tape read while interpreting is refused" T-LABEL
   [: BADMODE ;] E-NELAB-MODE TTHROWSQ ;

: SECOND-NAME-CASE ( -- )
   s" a second interpreting row inside the body is refused" T-LABEL
   [: SECOND-NAME ;] E-NELAB-MODE TTHROWSQ ;

: NAMEMODE-CASE ( -- )
   s" a defined name the tape read while compiling is refused" T-LABEL
   [: NAMEMODE ;] E-NELAB-MODE TTHROWSQ ;

\ ---- the three shapes a control word builds ----------------------------------
\ Each of these is a real corpus body, elaborated and then measured on the three
\ things a control construction can get wrong: how many blocks it made, which
\ block each edge goes to, and what the join takes as its arguments. A module
\ that reached FREEZE has already been through the whole structural verifier -
\ dominance, successor-argument counts and types, one terminator per block - so
\ what is left to assert is the wiring, and the wiring is asserted by ordinal.

\ `MAX2 2dup < if swap then drop`. Four blocks: the entry, the false arm's stub,
\ the true arm, and the join. The two-way branch hands nothing over and its two
\ successors are the stub and the true arm IN THAT ORDER - zero first - so a
\ swapped pair or a flipped polarity is a different pair of ordinals here. Both
\ arms reach the join with two arguments, because the stack was two deep when the
\ structure opened and `swap` changed which value is which and not how many.
: MAX2-BODY ( IR-CTX:ctx -- n bool n n n n n n n )
   {: c:IR-CTX:ctx :}
   s" MAX2 2dup < if swap then drop" TEXT!
   c SEALED
   {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena v:IR-ARENA:view :}
   c b v p r 2 1 NELAB:COLON {: f:IR-ID:ir-fun-id :}
   c b IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   m f F-BLKS
   m f 0 F-BLK-AT {: e:IR-ID:ir-block-id :}
   m f 1 F-BLK-AT {: st:IR-ID:ir-block-id :}
   m f 2 F-BLK-AT {: th:IR-ID:ir-block-id :}
   m f 3 F-BLK-AT {: jn:IR-ID:ir-block-id :}
   m e F-TERM {: t:IR-ID:ir-op-id :}
   m t s" hir.brz" F-OPC?
   m t F-SUCCS
   m t 0 F-SUCC
   m t 1 F-SUCC
   m  m st F-TERM  F-INS
   m  m th F-TERM  F-INS
   m jn F-ARGS
   m  m st F-TERM  0 F-SUCC ;

: MAX2-CASE ( -- )
   s" a two-way branch becomes four blocks wired to one join" T-LABEL
   BND [: MAX2-BODY ;] IR-CTX:WITH-CONTEXT
   3 T= 2 T= 2 T= 2 T= 2 T= 1 T= 2 T= TTRUE 4 T= ;

\ `COUNT-DOWN begin 1- dup 0 <= until`. Four blocks: the entry, the loop header,
\ the latch and the exit. The header is reached twice - once from the entry and
\ once from the latch - and takes one argument both times, which is what makes
\ the loop-carried value a block argument instead of a redefinition. `until`
\ leaves when the flag is true, so the ZERO successor is the latch and the other
\ is the exit: reversing them turns the loop inside out and the two ordinals say
\ so.
: COUNTDOWN-BODY ( IR-CTX:ctx -- n bool n n n n n n n )
   {: c:IR-CTX:ctx :}
   s" COUNT-DOWN begin 1- dup 0 <= until" TEXT!
   c SEALED
   {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena v:IR-ARENA:view :}
   c b v p r 1 1 NELAB:COLON {: f:IR-ID:ir-fun-id :}
   c b IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   m f F-BLKS
   m f 0 F-BLK-AT {: e:IR-ID:ir-block-id :}
   m f 1 F-BLK-AT {: hd:IR-ID:ir-block-id :}
   m f 2 F-BLK-AT {: la:IR-ID:ir-block-id :}
   m  m e F-TERM  s" hir.br" F-OPC?
   m  m e F-TERM  0 F-SUCC
   m hd F-ARGS
   m  m hd F-TERM  F-SUCCS
   m  m hd F-TERM  0 F-SUCC
   m  m hd F-TERM  1 F-SUCC
   m  m la F-TERM  0 F-SUCC
   m  m la F-TERM  F-INS ;

: COUNTDOWN-CASE ( -- )
   s" a begin-until loop becomes a header its latch branches back to" T-LABEL
   BND [: COUNTDOWN-BODY ;] IR-CTX:WITH-CONTEXT
   1 T= 1 T= 3 T= 2 T= 2 T= 1 T= 1 T= TTRUE 4 T= ;

\ `SUM-TO 0 swap 0 ?do i + loop`. Seven blocks: the entry, the skip stub the
\ entry test branches to when the loop runs no turns at all, the pre-header, the
\ header, the exit stub, the latch, and the join both exits meet in. The header
\ takes three arguments - the accumulator, the index and the limit - because all
\ three change on every turn, and the index is NOT on the value vector: Forth's
\ loop parameters are not on the data stack, so the body's `i` reads the header's
\ argument rather than something the body pushed.
: SUMTO-BODY ( IR-CTX:ctx -- n n n n n n n n n )
   {: c:IR-CTX:ctx :}
   s" SUM-TO 0 swap 0 ?do i + loop" TEXT!
   c SEALED
   {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena v:IR-ARENA:view :}
   c b v p r 1 1 NELAB:COLON {: f:IR-ID:ir-fun-id :}
   c b IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   m f F-BLKS
   m f 0 F-BLK-AT {: e:IR-ID:ir-block-id :}
   m f 1 F-BLK-AT {: sk:IR-ID:ir-block-id :}
   m f 2 F-BLK-AT {: pr:IR-ID:ir-block-id :}
   m f 3 F-BLK-AT {: hd:IR-ID:ir-block-id :}
   m f 4 F-BLK-AT {: xt:IR-ID:ir-block-id :}
   m f 5 F-BLK-AT {: la:IR-ID:ir-block-id :}
   m f 6 F-BLK-AT {: jn:IR-ID:ir-block-id :}
   m  m e F-TERM  0 F-SUCC
   m  m e F-TERM  1 F-SUCC
   m  m sk F-TERM  0 F-SUCC
   m  m pr F-TERM  0 F-SUCC
   m hd F-ARGS
   m  m la F-TERM  0 F-SUCC
   m  m xt F-TERM  0 F-SUCC
   m jn F-ARGS ;

: SUMTO-CASE ( -- )
   s" a counted loop becomes a header, a latch and one join for both exits" T-LABEL
   BND [: SUMTO-BODY ;] IR-CTX:WITH-CONTEXT
   1 T= 6 T= 3 T= 3 T= 3 T= 6 T= 2 T= 1 T= 7 T= ;

\ `WCOUNT begin dup 0 > while 1- repeat`. Five blocks: the entry, the loop
\ header, the stub the `while` leaves through, the body, and the block after the
\ loop. THE POLARITY IS THE WHOLE OF WHAT THIS CASE MEASURES, and it is the
\ opposite of `until`'s: `while` stays in the loop while its flag is TRUE, so the
\ ZERO successor is the stub out and the other is the body. Turning the two round
\ compiles a loop that runs exactly when it should not, and the two ordinals here
\ say which way they are wired. The stub carries the loop's one live value to the
\ block after the loop, which takes it as an argument, and the body branches back
\ to the header carrying the value the next turn reads.
: WCOUNT-BODY ( IR-CTX:ctx -- n n n n n n n n n n )
   {: c:IR-CTX:ctx :}
   s" WCOUNT begin dup 0 > while 1- repeat" TEXT!
   c SEALED
   {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena v:IR-ARENA:view :}
   c b v p r 1 1 NELAB:COLON {: f:IR-ID:ir-fun-id :}
   c b IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   m f F-BLKS
   m f 0 F-BLK-AT {: e:IR-ID:ir-block-id :}
   m f 1 F-BLK-AT {: hd:IR-ID:ir-block-id :}
   m f 2 F-BLK-AT {: st:IR-ID:ir-block-id :}
   m f 3 F-BLK-AT {: bd:IR-ID:ir-block-id :}
   m f 4 F-BLK-AT {: xt:IR-ID:ir-block-id :}
   m  m e F-TERM  0 F-SUCC
   m hd F-ARGS
   m  m hd F-TERM  F-SUCCS
   m  m hd F-TERM  0 F-SUCC
   m  m hd F-TERM  1 F-SUCC
   m  m st F-TERM  0 F-SUCC
   m  m st F-TERM  F-INS
   m  m bd F-TERM  0 F-SUCC
   m xt F-ARGS ;

: WCOUNT-CASE ( -- )
   s" a begin-while-repeat loop leaves through the while and goes round through the repeat" T-LABEL
   BND [: WCOUNT-BODY ;] IR-CTX:WITH-CONTEXT
   1 T= 1 T= 1 T= 4 T= 3 T= 2 T= 2 T= 1 T= 1 T= 5 T= ;

\ `PICK2 2dup > if drop else nip then`. Five blocks: the entry, the stub the
\ `if`'s false path leaves through, the first arm, the second arm, and the join.
\ THE STUB LANDS IN THE SECOND ARM AND NOT IN THE JOIN, which is the one thing an
\ `else` changes about the shape an `if` builds, and the ordinal says so.
\
\ AND THE JOIN IS ONE VALUE WIDE WHERE THE STRUCTURE OPENED TWO DEEP. With one
\ arm the join is also reached by the `if`'s own false stub, so an arm has to
\ leave the stack as it found it; with two, both edges into the join come from
\ arms, so a structure whose arms each CONSUME a value and leave one - which is
\ every `max` ever written - is an ordinary structure. The second arm takes the
\ two values the stub handed it and the join takes the one both arms left.
: PICK2-BODY ( IR-CTX:ctx -- n n n n n n n n n )
   {: c:IR-CTX:ctx :}
   s" PICK2 2dup > if drop else nip then" TEXT!
   c SEALED
   {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena v:IR-ARENA:view :}
   c b v p r 2 1 NELAB:COLON {: f:IR-ID:ir-fun-id :}
   c b IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   m f F-BLKS
   m f 0 F-BLK-AT {: e:IR-ID:ir-block-id :}
   m f 1 F-BLK-AT {: st:IR-ID:ir-block-id :}
   m f 2 F-BLK-AT {: a1:IR-ID:ir-block-id :}
   m f 3 F-BLK-AT {: a2:IR-ID:ir-block-id :}
   m f 4 F-BLK-AT {: jn:IR-ID:ir-block-id :}
   m  m e F-TERM  0 F-SUCC
   m  m e F-TERM  1 F-SUCC
   m  m st F-TERM  0 F-SUCC
   m  m st F-TERM  F-INS
   m a2 F-ARGS
   m  m a1 F-TERM  0 F-SUCC
   m  m a2 F-TERM  0 F-SUCC
   m jn F-ARGS ;

: PICK2-CASE ( -- )
   s" an else sends the false path into a second arm and both arms into one join" T-LABEL
   BND [: PICK2-BODY ;] IR-CTX:WITH-CONTEXT
   1 T= 4 T= 4 T= 2 T= 2 T= 3 T= 2 T= 1 T= 5 T= ;

\ `TWOW begin dup 0 > while dup 7 <> while 1- repeat`. TWO `while`s in one loop,
\ which is ordinary Forth and the reason the block after the loop is named
\ against the `begin` rather than against a `while`: both of them read the one
\ answer and both of their stubs branch to it, so the block after the loop has
\ two paths into it and still takes one set of arguments. Seven blocks: the
\ entry, the header, the first stub, the block between the two tests, the second
\ stub, the body, and the block after the loop.
: TWOW-BODY ( IR-CTX:ctx -- n n n n n n n n )
   {: c:IR-CTX:ctx :}
   s" TWOW begin dup 0 > while dup 7 <> while 1- repeat" TEXT!
   c SEALED
   {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena v:IR-ARENA:view :}
   c b v p r 1 1 NELAB:COLON {: f:IR-ID:ir-fun-id :}
   c b IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   m f F-BLKS
   m f 1 F-BLK-AT {: hd:IR-ID:ir-block-id :}
   m f 2 F-BLK-AT {: s1:IR-ID:ir-block-id :}
   m f 3 F-BLK-AT {: mid:IR-ID:ir-block-id :}
   m f 4 F-BLK-AT {: s2:IR-ID:ir-block-id :}
   m f 5 F-BLK-AT {: bd:IR-ID:ir-block-id :}
   m f 6 F-BLK-AT {: xt:IR-ID:ir-block-id :}
   m  m hd F-TERM  0 F-SUCC
   m  m mid F-TERM  0 F-SUCC
   m  m mid F-TERM  1 F-SUCC
   m  m s1 F-TERM  0 F-SUCC
   m  m s2 F-TERM  0 F-SUCC
   m  m bd F-TERM  0 F-SUCC
   m xt F-ARGS ;

: TWOW-CASE ( -- )
   s" two whiles in one loop leave through one block after it" T-LABEL
   BND [: TWOW-BODY ;] IR-CTX:WITH-CONTEXT
   1 T= 1 T= 6 T= 6 T= 5 T= 4 T= 2 T= 7 T= ;

\ ---- what a broken control structure is refused as ---------------------------
\ A closer with no opener, a closer that does not match the opener it meets, an
\ opener left open at the end of the body, and an arm that leaves the stack a
\ different depth from the one it started at. Each is a different way of writing
\ a control structure that does not close, and each has to be refused by name
\ rather than compiled into some other program.
: ORPHAN-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   s" ORPHAN 1 then" TEXT!
   c SEALED
   {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena v:IR-ARENA:view :}
   c b v p r 0 1 NELAB:COLON drop ;

: ORPHAN ( -- )
   BND [: ORPHAN-BODY ;] IR-CTX:WITH-CONTEXT ;

: CROSSED-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   s" CROSSED begin 0 then" TEXT!
   c SEALED
   {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena v:IR-ARENA:view :}
   c b v p r 0 1 NELAB:COLON drop ;

: CROSSED ( -- )
   BND [: CROSSED-BODY ;] IR-CTX:WITH-CONTEXT ;

: UNCLOSED-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   s" UNCLOSED 0 if" TEXT!
   c SEALED
   {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena v:IR-ARENA:view :}
   c b v p r 0 1 NELAB:COLON drop ;

: UNCLOSED ( -- )
   BND [: UNCLOSED-BODY ;] IR-CTX:WITH-CONTEXT ;

: LOPSIDED-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   s" LOPSIDED dup if 1 then" TEXT!
   c SEALED
   {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena v:IR-ARENA:view :}
   c b v p r 1 1 NELAB:COLON drop ;

: LOPSIDED ( -- )
   BND [: LOPSIDED-BODY ;] IR-CTX:WITH-CONTEXT ;

: STRAY-INDEX-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   s" STRAY i" TEXT!
   c SEALED
   {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena v:IR-ARENA:view :}
   c b v p r 0 1 NELAB:COLON drop ;

: STRAY-INDEX ( -- )
   BND [: STRAY-INDEX-BODY ;] IR-CTX:WITH-CONTEXT ;

\ The seven ways the two mid-structure words can be written wrong. Each one is a
\ program a Forth compiler would happily assemble into something, and each one is
\ a different broken invariant of the block construction: a `while` outside any
\ loop has no exit to name, a `repeat` over a loop no `while` left opens a block
\ nothing branches to, an `until` over a loop a `while` DID leave strands the
\ values that `while` handed over, a loop body that does not leave the stack as
\ the header takes it hands the back edge the wrong number, two `while`s that
\ disagree about how deep they left hand one block two different widths, an
\ `else` outside any `if` has no first arm to end, and two `else`s over one `if`
\ would open the second arm twice.
: STRAYW-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   s" STRAYW dup while drop" TEXT!
   c SEALED
   {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena v:IR-ARENA:view :}
   c b v p r 1 1 NELAB:COLON drop ;

: STRAYW ( -- )
   BND [: STRAYW-BODY ;] IR-CTX:WITH-CONTEXT ;

: NOWHILE-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   s" NOWHILE begin 1- repeat" TEXT!
   c SEALED
   {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena v:IR-ARENA:view :}
   c b v p r 1 1 NELAB:COLON drop ;

: NOWHILE ( -- )
   BND [: NOWHILE-BODY ;] IR-CTX:WITH-CONTEXT ;

: WUNTIL-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   s" WUNTIL begin dup 0 > while dup 0 <= until" TEXT!
   c SEALED
   {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena v:IR-ARENA:view :}
   c b v p r 1 1 NELAB:COLON drop ;

: WUNTIL ( -- )
   BND [: WUNTIL-BODY ;] IR-CTX:WITH-CONTEXT ;

: WDRIFT-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   s" WDRIFT begin dup 0 > while 1- 7 repeat" TEXT!
   c SEALED
   {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena v:IR-ARENA:view :}
   c b v p r 1 1 NELAB:COLON drop ;

: WDRIFT ( -- )
   BND [: WDRIFT-BODY ;] IR-CTX:WITH-CONTEXT ;

: WWIDTH-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   s" WWIDTH begin dup 0 > while 7 over 0 <> while drop 1- repeat" TEXT!
   c SEALED
   {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena v:IR-ARENA:view :}
   c b v p r 1 1 NELAB:COLON drop ;

: WWIDTH ( -- )
   BND [: WWIDTH-BODY ;] IR-CTX:WITH-CONTEXT ;

: STRAYE-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   s" STRAYE 1 else 2 then" TEXT!
   c SEALED
   {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena v:IR-ARENA:view :}
   c b v p r 0 1 NELAB:COLON drop ;

: STRAYE ( -- )
   BND [: STRAYE-BODY ;] IR-CTX:WITH-CONTEXT ;

: TWOE-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   s" TWOE dup if 1 else 2 else 3 then" TEXT!
   c SEALED
   {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena v:IR-ARENA:view :}
   c b v p r 1 1 NELAB:COLON drop ;

: TWOE ( -- )
   BND [: TWOE-BODY ;] IR-CTX:WITH-CONTEXT ;

\ Two arms that leave different depths hand the same join different numbers of
\ values, which is the same refusal a one-armed `if` gets for changing the depth
\ at all - and it is the check that says the join's width is derived from the
\ first arm and then ENFORCED on the second, rather than taken from whichever
\ arm happened to close last.
: ELOPSIDED-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   s" ELOPSIDED dup if 1 else 2 3 then" TEXT!
   c SEALED
   {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena v:IR-ARENA:view :}
   c b v p r 1 1 NELAB:COLON drop ;

: ELOPSIDED ( -- )
   BND [: ELOPSIDED-BODY ;] IR-CTX:WITH-CONTEXT ;

\ `exit` ends the block it stands in, so an `else` after one would close a block
\ that is not open. It is refused by name; dot habu-let-exit-stand-d74f14ec
\ carries the capability.
: EXITELSE-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   s" EXITELSE dup if drop 1 exit else 2 then" TEXT!
   c SEALED
   {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena v:IR-ARENA:view :}
   c b v p r 1 1 NELAB:COLON drop ;

: EXITELSE ( -- )
   BND [: EXITELSE-BODY ;] IR-CTX:WITH-CONTEXT ;

\ A refused elaboration leaves its context standing, so each of these gets an
\ enclosing context of its own in RUN, one case at a time, exactly as every
\ other refusal in this suite does.
: ORPHAN-CASE ( -- )
   s" a closer with no opener is refused" T-LABEL
   [: ORPHAN ;] E-NELAB-CTRL TTHROWSQ ;

: CROSSED-CASE ( -- )
   s" a closer that does not match its opener is refused" T-LABEL
   [: CROSSED ;] E-NELAB-CTRL TTHROWSQ ;

: UNCLOSED-CASE ( -- )
   s" a body that ends with a structure still open is refused" T-LABEL
   [: UNCLOSED ;] E-NELAB-CTRL TTHROWSQ ;

: LOPSIDED-CASE ( -- )
   s" an arm that changes the stack depth is refused" T-LABEL
   [: LOPSIDED ;] E-NELAB-JOIN TTHROWSQ ;

: STRAY-INDEX-CASE ( -- )
   s" a loop index outside any counted loop is refused" T-LABEL
   [: STRAY-INDEX ;] E-NELAB-CTRL TTHROWSQ ;

: STRAYW-CASE ( -- )
   s" a while outside any loop is refused" T-LABEL
   [: STRAYW ;] E-NELAB-CTRL TTHROWSQ ;

: NOWHILE-CASE ( -- )
   s" a repeat closing a loop no while left is refused" T-LABEL
   [: NOWHILE ;] E-NELAB-CTRL TTHROWSQ ;

: WUNTIL-CASE ( -- )
   s" an until closing a loop a while left is refused" T-LABEL
   [: WUNTIL ;] E-NELAB-CTRL TTHROWSQ ;

: WDRIFT-CASE ( -- )
   s" a while loop whose body does not leave the stack as it found it is refused" T-LABEL
   [: WDRIFT ;] E-NELAB-JOIN TTHROWSQ ;

: WWIDTH-CASE ( -- )
   s" two whiles that leave one loop at different depths are refused" T-LABEL
   [: WWIDTH ;] E-NELAB-JOIN TTHROWSQ ;

: STRAYE-CASE ( -- )
   s" an else outside any if is refused" T-LABEL
   [: STRAYE ;] E-NELAB-CTRL TTHROWSQ ;

: TWOE-CASE ( -- )
   s" a second else over one if is refused" T-LABEL
   [: TWOE ;] E-NELAB-CTRL TTHROWSQ ;

: ELOPSIDED-CASE ( -- )
   s" two arms that leave different depths are refused" T-LABEL
   [: ELOPSIDED ;] E-NELAB-JOIN TTHROWSQ ;

: EXITELSE-CASE ( -- )
   s" an else after an exit is refused" T-LABEL
   [: EXITELSE ;] E-NELAB-CTRL TTHROWSQ ;

\ ---- a typed locals frame ----------------------------------------------------
\ The corpus's LERP, written as the tape carries it: `{:`, one `name:type` token
\ per local, `:}`, then a body that reads the names. A local is a named SSA
\ VALUE, so the whole of what this case has to prove is which value each name
\ ended up meaning - and every one of those answers is an operand identity read
\ off the published module, not a count.
\
\ THE BINDING ORDER IS THE POINT. `{: a b t :}` over a stack holding a, b, t
\ binds a to the DEEPEST value, so `a` is block argument zero and `t` is block
\ argument two. `b a -` therefore subtracts argument zero from argument one, and
\ a subtraction's operands cannot be exchanged without changing the answer - so
\ a frame that bound the names the other way round reddens here rather than
\ computing the same number by another route. `t *` then reads argument two, and
\ the second `a` reads argument zero again, which is what says a name may be
\ read more than once and still be one value.
\
\ AND THE DIVISION'S OPERANDS. `100 /` divides the product by the hundred, in
\ that order, so operand zero is the multiply's result and operand one is the
\ constant's. Swapping them is a different program and this says so.
\
\ Six operations for nine body tokens: the group stages nothing at all - it is
\ five of those tokens - and neither does either mention of a local.
: LERP-BODY ( IR-CTX:ctx -- n bool bool bool bool bool bool bool bool bool bool n )
   {: c:IR-CTX:ctx :}
   s" LERP {: a:n b:n t:n :} b a - t * 100 / a +" TEXT!
   c SEALED
   {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena v:IR-ARENA:view :}
   c b v p r 3 1 NELAB:COLON {: f:IR-ID:ir-fun-id :}
   c b IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   m f F-BLK {: blk:IR-ID:ir-block-id :}
   m blk F-OPS
   m blk 0 F-OP {: sb:IR-ID:ir-op-id :}
   m blk 1 F-OP {: ml:IR-ID:ir-op-id :}
   m blk 2 F-OP {: kn:IR-ID:ir-op-id :}
   m blk 3 F-OP {: dv:IR-ID:ir-op-id :}
   m blk 4 F-OP {: ad:IR-ID:ir-op-id :}
   m sb s" hir.sub" F-OPC?
   m dv s" hir.div" F-OPC?
   m sb 0 F-IN  m blk 1 F-ARG SAME?
   m sb 1 F-IN  m blk 0 F-ARG SAME?
   m ml 0 F-IN  m sb 0 F-OUT SAME?
   m ml 1 F-IN  m blk 2 F-ARG SAME?
   m dv 0 F-IN  m ml 0 F-OUT SAME?
   m dv 1 F-IN  m kn 0 F-OUT SAME?
   m ad 0 F-IN  m dv 0 F-OUT SAME?
   m ad 1 F-IN  m blk 0 F-ARG SAME?
   m F-VALUES ;

: LERP-CASE ( -- )
   s" a typed locals frame binds the first name to the deepest value" T-LABEL
   BND [: LERP-BODY ;] IR-CTX:WITH-CONTEXT
   8 T=
   TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE
   TTRUE TTRUE
   6 T= ;

\ ---- what a locals frame refuses ---------------------------------------------
\ Each of the five below is a shape this elaborator has no rule for, and each one
\ is refused by name rather than compiled into something else. Rebinding a local
\ and taking its address need no case here: `to` and `^` are not words of the
\ dialect at all, so they are already refused as E-HIR-UNMODELED by the case
\ above them in this file.
: TWO-GROUPS-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   s" TWOG {: a:n :} {: b:n :} a" TEXT!
   c SEALED
   {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena v:IR-ARENA:view :}
   c b v p r 2 1 NELAB:COLON drop ;

: TWO-GROUPS ( -- )
   BND [: TWO-GROUPS-BODY ;] IR-CTX:WITH-CONTEXT ;

: OPEN-GROUP-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   s" OPENG {: a:n" TEXT!
   c SEALED
   {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena v:IR-ARENA:view :}
   c b v p r 1 1 NELAB:COLON drop ;

: OPEN-GROUP ( -- )
   BND [: OPEN-GROUP-BODY ;] IR-CTX:WITH-CONTEXT ;

: SHADOW-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   s" SHADOW {: dup:n :} 0" TEXT!
   c SEALED
   {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena v:IR-ARENA:view :}
   c b v p r 1 1 NELAB:COLON drop ;

: SHADOW ( -- )
   BND [: SHADOW-BODY ;] IR-CTX:WITH-CONTEXT ;

: TWICE-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   s" TWICE {: a:n a:n :} a" TEXT!
   c SEALED
   {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena v:IR-ARENA:view :}
   c b v p r 2 1 NELAB:COLON drop ;

: TWICE ( -- )
   BND [: TWICE-BODY ;] IR-CTX:WITH-CONTEXT ;

\ A group inside a branch would bind names on a path that does not reach the
\ rest of the body, and this elaborator has no scoping rule for that.
: NESTED-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   s" NESTG dup if {: a:n :} a then" TEXT!
   c SEALED
   {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena v:IR-ARENA:view :}
   c b v p r 1 1 NELAB:COLON drop ;

: NESTED ( -- )
   BND [: NESTED-BODY ;] IR-CTX:WITH-CONTEXT ;

: TWO-GROUPS-CASE ( -- )
   s" a second locals group in one definition is refused" T-LABEL
   [: TWO-GROUPS ;] E-NELAB-LOCAL TTHROWSQ ;

: OPEN-GROUP-CASE ( -- )
   s" a locals group the body never closes is refused" T-LABEL
   [: OPEN-GROUP ;] E-NELAB-LOCAL TTHROWSQ ;

: SHADOW-CASE ( -- )
   s" a local named after a word the dialect models is refused" T-LABEL
   [: SHADOW ;] E-NELAB-LOCAL TTHROWSQ ;

: TWICE-CASE ( -- )
   s" the same local declared twice is refused" T-LABEL
   [: TWICE ;] E-NELAB-LOCAL TTHROWSQ ;

: NESTED-CASE ( -- )
   s" a locals group inside a control structure is refused" T-LABEL
   [: NESTED ;] E-NELAB-LOCAL TTHROWSQ ;

public

: RUN ( -- )
   T-RESET
   MAX2-CASE
   LERP-CASE
   BND [: drop TWO-GROUPS-CASE ;] IR-CTX:WITH-CONTEXT
   BND [: drop OPEN-GROUP-CASE ;] IR-CTX:WITH-CONTEXT
   BND [: drop SHADOW-CASE ;] IR-CTX:WITH-CONTEXT
   BND [: drop TWICE-CASE ;] IR-CTX:WITH-CONTEXT
   BND [: drop NESTED-CASE ;] IR-CTX:WITH-CONTEXT
   COUNTDOWN-CASE
   SUMTO-CASE
   WCOUNT-CASE
   PICK2-CASE
   TWOW-CASE
   BND [: drop ORPHAN-CASE ;] IR-CTX:WITH-CONTEXT
   BND [: drop CROSSED-CASE ;] IR-CTX:WITH-CONTEXT
   BND [: drop UNCLOSED-CASE ;] IR-CTX:WITH-CONTEXT
   BND [: drop LOPSIDED-CASE ;] IR-CTX:WITH-CONTEXT
   BND [: drop STRAY-INDEX-CASE ;] IR-CTX:WITH-CONTEXT
   BND [: drop STRAYW-CASE ;] IR-CTX:WITH-CONTEXT
   BND [: drop NOWHILE-CASE ;] IR-CTX:WITH-CONTEXT
   BND [: drop WUNTIL-CASE ;] IR-CTX:WITH-CONTEXT
   BND [: drop WDRIFT-CASE ;] IR-CTX:WITH-CONTEXT
   BND [: drop WWIDTH-CASE ;] IR-CTX:WITH-CONTEXT
   BND [: drop STRAYE-CASE ;] IR-CTX:WITH-CONTEXT
   BND [: drop TWOE-CASE ;] IR-CTX:WITH-CONTEXT
   BND [: drop ELOPSIDED-CASE ;] IR-CTX:WITH-CONTEXT
   BND [: drop EXITELSE-CASE ;] IR-CTX:WITH-CONTEXT
   SQUARE-CASE
   INC-CASE
   BUMP-CASE
   SUMA-CASE
   DIFF-CASE
   ROT3-CASE
   NDIF-CASE
   PASS-CASE
   FUN-CASE
   BND [: drop UNDEC-CASE ;] IR-CTX:WITH-CONTEXT
   BND [: drop STRTOK-CASE ;] IR-CTX:WITH-CONTEXT
   BND [: drop UNDER-CASE ;] IR-CTX:WITH-CONTEXT
   BND [: drop RENAME-UNDER-CASE ;] IR-CTX:WITH-CONTEXT
   BND [: drop DEEP-CASE ;] IR-CTX:WITH-CONTEXT
   BND [: drop WIDE-CASE ;] IR-CTX:WITH-CONTEXT
   BND [: drop NARROW-CASE ;] IR-CTX:WITH-CONTEXT
   BND [: drop NEGARITY-CASE ;] IR-CTX:WITH-CONTEXT
   BND [: drop EMPTY-CASE ;] IR-CTX:WITH-CONTEXT
   BND [: drop LITNAME-CASE ;] IR-CTX:WITH-CONTEXT
   BND [: drop FOREIGN-CASE ;] IR-CTX:WITH-CONTEXT
   BND [: drop BADMODE-CASE ;] IR-CTX:WITH-CONTEXT
   BND [: drop SECOND-NAME-CASE ;] IR-CTX:WITH-CONTEXT
   BND [: drop NAMEMODE-CASE ;] IR-CTX:WITH-CONTEXT
   T-REPORT ;

;package

NELAB-TEST:RUN
