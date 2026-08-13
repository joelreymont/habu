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

require lib/errors.f
require lib/test.f
require src/compiler/native/elaborate.f
require test/compiler/native-source-fixture.f

\ ---- the three targets the `is` fixtures name --------------------------------
\ THEY ARE GLOBAL, AND THAT IS PART OF THE FIXTURE. The chain resolves an `is`
\ target the way the engine resolves a name in the body being compiled - the
\ open package's two wordlists, then the global one - and no package is open
\ when this suite's cases run. A package-private defer would answer absent and
\ the positive case would then be refused for the wrong reason, proving nothing
\ about the operand role it exists to measure. NELB-DATA's first data cell holds
\ the defer magic exactly, which is what a real defer's trailer starts with.
create NELB-DATA  $4842444546455201 ,  0 ,
: NELB-PLAIN ( n -- n ) 2 * ;
defer NELB-HOOK ( n -- n )

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

\ ---- naming an operation by what it IS ---------------------------------------
\ A fixture that reads `block op 7` pins the arithmetic of the operation list,
\ not the semantics of the body, so any legal transform that changes the count
\ reads as an IR invariant violation. That mis-directed a whole lane once: the
\ literal memo collapsed two address constants, index 8 stopped existing, and
\ IR-FUN:FOP@ threw the generic out-of-range code from the TEST while the module
\ froze cleanly. The three readers below let a case say which operation it means
\ - the first store, the only load, how many constants there are - so the suite
\ constrains what the body computes and stays silent about how many operations
\ express it.
: F-OPC-N ( IR-BUILD:module IR-ID:ir-block-id ptr u8 n -- n )
   {: m:IR-BUILD:module blk:IR-ID:ir-block-id a u:n :} \ typed-local-lint: allow-bare-local - a keeps the ptr u8 byte-span role
   0
   m blk F-OPS 0 ?do
      m  m blk i F-OP  a u F-OPC? if 1+ then
   loop ;

\ The k-th operation of this block carrying this opcode, counting from zero. A
\ case that asks for one that is not there gets a refusal rather than a wrong
\ operation, because "the second store" not existing is exactly the kind of
\ change a fixture is here to catch.
: F-OPC-AT ( IR-BUILD:module IR-ID:ir-block-id ptr u8 n n -- IR-ID:ir-op-id )
   {: m:IR-BUILD:module blk:IR-ID:ir-block-id a u:n k:n :} \ typed-local-lint: allow-bare-local - a keeps the ptr u8 byte-span role
   k
   m blk F-OPS 0 ?do
      m  m blk i F-OP  a u F-OPC? if
         dup 0= if drop m blk i F-OP unloop exit then
         1-
      then
   loop
   drop E-NELAB-UNDER throw ;

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

: F-OUTS ( IR-BUILD:module IR-ID:ir-op-id -- n )
   {: m:IR-BUILD:module op:IR-ID:ir-op-id :}
   m IR-BUILD:FOP-ROWS op IR-OP:FRESULTS ;

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

\ Is the value this operand names defined by an operation of this opcode? Says
\ "the address a store writes through is a constant" without saying WHICH
\ constant operation computed it, so it reads the same whether or not two equal
\ literals in the block were folded into one value.
: F-FROM? ( IR-BUILD:module IR-ID:ir-block-id IR-ID:ir-value-id ptr u8 n -- bool )
   {: m:IR-BUILD:module blk:IR-ID:ir-block-id v:IR-ID:ir-value-id a u:n :} \ typed-local-lint: allow-bare-local - a keeps the ptr u8 byte-span role
   false
   m blk F-OPS 0 ?do
      m blk i F-OP {: op:IR-ID:ir-op-id :}
      m op F-OUTS 0 > if
         m op 0 F-OUT v SAME? if
            drop m op a u F-OPC? leave
         then
      then
   loop ;

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
\ word the model names and the engine answers for. What this case measures is the two
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
\ THE DATA WORD IS A REAL ONE NOW. The model is no longer told an address; it
\ asks the engine what the spelling denotes, so the case has to create the word
\ before it declares it. It is created through the same front end any definition
\ goes through, at the scope this case runs in, which is the scope the model
\ resolves it in.
TRUSTED: EV ( ptr u8 n -- ) evaluate ;

: CELL-A! ( -- )
   s" CELL-A" 0 search-wl 0<> if exit then
   s" create CELL-A 1 cells allot" EV ;

: SEALED-DATA ( IR-CTX:ctx -- IR-BUILD:builder IR-ARENA:arena IR-ARENA:arena IR-ARENA:view )
   {: c:IR-CTX:ctx :}
   CELL-A!
   c HIR-BUILDER {: b:IR-BUILD:builder :}
   c b s" CELL-A" MODEL-DATA
   {: p:IR-ARENA:arena r:IR-ARENA:arena :}
   c b TAPE {: tp:IR-ARENA:arena :}
   c LEX
   b p r  tp NTAPE:SEAL ;

: BUMP-BODY ( IR-CTX:ctx -- n n n n bool bool bool bool bool bool )
   {: c:IR-CTX:ctx :}
   s" BUMP CELL-A ! CELL-A @ 1+ dup CELL-A !" TEXT!
   c SEALED-DATA
   {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena v:IR-ARENA:view :}
   c b v p r 1 1 NELAB:COLON {: f:IR-ID:ir-fun-id :}
   c b IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   m f F-BLK {: blk:IR-ID:ir-block-id :}
   m blk s" hir.mem" 0 F-OPC-AT {: mem:IR-ID:ir-op-id :}
   m blk s" hir.store" 0 F-OPC-AT {: st0:IR-ID:ir-op-id :}
   m blk s" hir.load" 0 F-OPC-AT {: ld:IR-ID:ir-op-id :}
   m blk s" hir.store" 1 F-OPC-AT {: st1:IR-ID:ir-op-id :}
   m blk s" hir.mem" F-OPC-N
   m blk s" hir.store" F-OPC-N
   m blk s" hir.load" F-OPC-N
   m blk s" hir.add" F-OPC-N
   m st0 0 F-IN  m blk 0 F-ARG SAME?
   m st0 2 F-IN  m mem 0 F-OUT SAME?
   m ld 1 F-IN  m st0 0 F-OUT SAME?
   m st1 2 F-IN  m ld 1 F-OUT SAME?
   m blk  m st0 1 F-IN  s" hir.const" F-FROM?
   m blk  m ld 0 F-IN   s" hir.const" F-FROM? ;

: BUMP-CASE ( -- )
   s" a store and a load compile to one order, threaded link by link" T-LABEL
   BND [: BUMP-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE
   1 T= 1 T= 2 T= 1 T= ;

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

\ ---- the two words this suite is refused by -----------------------------------
\ WHY AN OUT-OF-SCOPE SPELLING IS THE FIXTURE. A body word the dialect models no
\ operation for is not refused for that alone: src/compiler/native/elaborate.f
\ RESOLVE-SCAN puts every unmodelled name to the running engine, and a word the
\ engine can name and the checker can size becomes a CALL. `negate` and `mod`,
\ which these fixtures used to be written with, compile that way now, and so does
\ a named CONSTANT: dot habu-export-the-checker-2bbc831c gave the checker's
\ stored effects a per-cell width, so `-- a` sizes at one cell. That capability
\ is asserted where both halves of it are live, in test/compiler/native-migrate.f
\ CONST-CALL-CASE, which migrates a body naming a constant and then runs it.
\
\ What is left, and what these two are, is a spelling that resolves NOWHERE the
\ chain looks. src/compiler/native/dict.f walks the open package's two wordlists
\ and then the global one; these constants are PUBLIC WORDS OF THIS PACKAGE and
\ the bodies spell them BARE, and every case here elaborates with no package
\ open. So the spelling is a real published word that the chain's resolver cannot
\ reach from where the body is compiled. It answers absent, and the elaborator
\ refuses the body with the dialect's own code, E-HIR-UNMODELED, naming that
\ token.
\
\ THE REASON IS THE RESOLVER'S OWN AND IT IS PERMANENT. A bare name is looked up
\ in the scope the definition is compiled in, and that is the engine's rule, not
\ a gap: no capability landing later makes a package's public word answer to its
\ bare spelling from outside. Which is what this fixture wanted from the width
\ refusal it used to be written with, and did not get - that one closed.
\
\ TWO OF THEM, SPELLED DIFFERENTLY AND VALUED DIFFERENTLY, because one case below
\ refuses two bodies and demands two different answers back.
public
5 constant K5
7 constant K7
private

\ ---- refusals: what the elaborator will not compile --------------------------
\ A word the model never declared. To checked source this is the same event as a
\ declared boundary, and it carries the word model's own name for it.
: UNDEC-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   s" BAD K7" TEXT!
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
\ A CHARACTER literal in the body: the token kind the straight-line subset still
\ does not model, refused as such rather than resolved as a name. It took over
\ this role from the string literal, which the subset now compiles.
: CHARTOK-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   s" BAD x" TEXT!
   c RIG
   {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena tp:IR-ARENA:arena :}
   c 0 3 NTAPE-MODE:INTERPRETING NAME,
   c 4 1 NTAPE-MODE:COMPILING $78 CHAR,
   c b  tp NTAPE:SEAL  p r 1 1 NELAB:COLON drop ;

\ And the same tape with a STRING literal in it, which the subset does compile:
\ the body leaves an address and a length, so the definition declares two
\ outputs and no inputs. Nothing is asserted about the code here - that is
\ test/compiler/native-string.f's work, through the production entry - only that
\ the elaborator has a rule for the kind at all.
: STRTOK-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   s" BAD x" TEXT!
   c RIG
   {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena tp:IR-ARENA:arena :}
   c 0 3 NTAPE-MODE:INTERPRETING NAME,
   c 4 1 NTAPE-MODE:COMPILING STR,
   c b  tp NTAPE:SEAL  p r 0 2 NELAB:COLON drop ;

: CHARTOK ( -- )
   BND [: CHARTOK-BODY ;] IR-CTX:WITH-CONTEXT ;

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

: CHARTOK-CASE ( -- )
   s" a character literal in the body is refused as a kind the subset has no model for" T-LABEL
   [: CHARTOK ;] E-HIR-KIND TTHROWSQ ;

: STRTOK-CASE ( -- )
   s" a string literal in the body is a kind the subset does model" T-LABEL
   BND [: STRTOK-BODY ;] IR-CTX:WITH-CONTEXT
   NELAB:REFUSED-ROW -1 T= ;

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

\ ---- which token the refusal was about ---------------------------------------
\ The refusals above say WHAT the chain would not compile; these say WHICH token
\ it was standing on, which is the part a caller can act on. The word model's two
\ refusals carry no token in the code itself, so the elaborator writes the token
\ down as the refusal leaves it, and these cases read that record back through
\ the same public entry every other case here uses.
\
\ WHAT IS ACTUALLY BEING PROVED, because "the record says K5" on its own would
\ pass just as well against a fixed answer. Three bodies refused for three
\ different spellings each get their own spelling back, one of them at a row that
\ is not the first, and one of them a name only the fixture has ever written - so
\ the answer is read off the tape rather than looked up in the dialect's table.
\
\ AND THE HOSTILE ONE, which is what the record is really for. A record left over
\ from an earlier refusal would be indistinguishable from a right answer at the
\ moment a caller reads it, so the cases below run a SECOND elaboration after a
\ refusal and demand the first one's word be gone: gone when the second refusal
\ names a different token, and gone when the second definition compiles and names
\ nothing at all.

\ `BAD ` followed by k copies of one letter: a body of exactly one token, whose
\ spelling is as long as the case wants to make it.
: LONG-TEXT ( n -- )
   {: k:n :}
   s" BAD " TEXT!
   k 0 ?do s" a" TEXT+ loop ;

\ That token's spelling, taken off the fixture's own text rather than written out
\ a second time here: four bytes of `BAD ` and then the whole of the body.
: BODY$ ( -- ptr u8 n )
   TEXT$ {: a u:n :} \ typed-local-lint: allow-bare-local - a keeps the ptr u8 byte-span role
   a 4 + u 4 - ;

\ A body word the chain cannot compile - the other of the two constants above, so
\ that the case which refuses both of them has two different answers to tell
\ apart.
: KTOK-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   s" BAD K5" TEXT!
   c SEALED
   {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena v:IR-ARENA:view :}
   c b v p r 1 1 NELAB:COLON drop ;

: KTOK ( -- )
   BND [: KTOK-BODY ;] IR-CTX:WITH-CONTEXT ;

\ A name no dialect will ever model, three tokens into the body. The two tokens
\ before it are modeled and leave the vector exactly as deep as it started, so
\ the elaboration really does reach the fourth row - which is what makes the row
\ the record answers a measurement rather than a constant.
: LATE-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   s" BAD dup * WIDGET" TEXT!
   c SEALED
   {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena v:IR-ARENA:view :}
   c b v p r 1 1 NELAB:COLON drop ;

: LATE ( -- )
   BND [: LATE-BODY ;] IR-CTX:WITH-CONTEXT ;

\ An unmodeled word spelled in exactly as many bytes as the record holds, and one
\ spelled in one byte more. The lengths are asked of the elaborator rather than
\ written down here, so the pair stays at the ceiling if the ceiling moves.
: FITNAME-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   NELAB:REFUSED-CAP LONG-TEXT
   c SEALED
   {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena v:IR-ARENA:view :}
   c b v p r 1 1 NELAB:COLON drop ;

: FITNAME ( -- )
   BND [: FITNAME-BODY ;] IR-CTX:WITH-CONTEXT ;

: OVERNAME-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   NELAB:REFUSED-CAP 1+ LONG-TEXT
   c SEALED
   {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena v:IR-ARENA:view :}
   c b v p r 1 1 NELAB:COLON drop ;

: OVERNAME ( -- )
   BND [: OVERNAME-BODY ;] IR-CTX:WITH-CONTEXT ;

\ A body that compiles. It measures nothing - the SQUARE case above already
\ measures what this body becomes - because what it is here for is the state it
\ leaves behind, and a fixture that answered a stackful of values would put that
\ state behind a row of assertions about something else.
: COMPILES-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   s" SQUARE dup *" TEXT!
   c SEALED
   {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena v:IR-ARENA:view :}
   c b v p r 1 1 NELAB:COLON drop ;

\ Each refusal, asserted. A case that runs two of them wraps each one in an
\ enclosing context of its own for the reason the section above gives: the throw
\ skips the elaboration's own teardown, so the arenas it left standing are only
\ reclaimed when something outside it exits normally.
: K-THROWS ( -- )
   [: KTOK ;] E-HIR-UNMODELED TTHROWSQ ;

: CHAR-THROWS ( -- )
   [: CHARTOK ;] E-HIR-KIND TTHROWSQ ;

: FIT-THROWS ( -- )
   [: FITNAME ;] E-HIR-UNMODELED TTHROWSQ ;

: OVER-THROWS ( -- )
   [: OVERNAME ;] E-HIR-UNMODELED TTHROWSQ ;

: UNDER-THROWS ( -- )
   [: UNDER ;] E-NELAB-UNDER TTHROWSQ ;

: REFUSED-WORD-CASE ( -- )
   s" a body word the dialect cannot compile is named by the record, with its row and its kind" T-LABEL
   K-THROWS
   NELAB:REFUSED$ s" K5" T$=
   NELAB:REFUSED-ROW 1 T=
   NTAPE-KIND:NAME NELAB:REFUSED-KIND? TTRUE ;

: REFUSED-OTHER-CASE ( -- )
   s" a different body word gives a different answer, so the record reads the body" T-LABEL
   [: UNDEC ;] E-HIR-UNMODELED TTHROWSQ
   NELAB:REFUSED$ s" K7" T$= ;

: REFUSED-LATE-CASE ( -- )
   s" a name only this fixture ever wrote is answered, at the row it stands on" T-LABEL
   [: LATE ;] E-HIR-UNMODELED TTHROWSQ
   NELAB:REFUSED$ s" WIDGET" T$=
   NELAB:REFUSED-ROW 3 T= ;

: REFUSED-KIND-CASE ( -- )
   s" a token kind the subset does not model is answered as that kind, not as a name" T-LABEL
   CHAR-THROWS
   NTAPE-KIND:CHAR-LITERAL NELAB:REFUSED-KIND? TTRUE
   NTAPE-KIND:NAME NELAB:REFUSED-KIND? TFALSE
   NELAB:REFUSED$ s" x" T$=
   NELAB:REFUSED-ROW 1 T= ;

: REFUSED-STALE-CASE ( -- )
   s" a later refusal never answers an earlier refusal's word" T-LABEL
   BND [: drop K-THROWS ;] IR-CTX:WITH-CONTEXT
   NELAB:REFUSED$ s" K5" T$=
   BND [: drop CHAR-THROWS ;] IR-CTX:WITH-CONTEXT
   NELAB:REFUSED$ s" K5" T$<>
   NELAB:REFUSED$ s" x" T$=
   NTAPE-KIND:NAME NELAB:REFUSED-KIND? TFALSE ;

: REFUSED-CLEARED-CASE ( -- )
   s" and a definition that compiles leaves no word for a caller to read" T-LABEL
   BND [: drop K-THROWS ;] IR-CTX:WITH-CONTEXT
   NELAB:REFUSED$ s" K5" T$=
   BND [: COMPILES-BODY ;] IR-CTX:WITH-CONTEXT
   NELAB:REFUSED-ROW -1 T=
   NELAB:REFUSED$ nip 0 T=
   NTAPE-KIND:NAME NELAB:REFUSED-KIND? TFALSE ;

\ A refusal that is not about one token's spelling. `+` is a word the dialect
\ models perfectly well; what is wrong is the body around it, so the record has
\ nothing to name and says so - even directly after a refusal that DID name a
\ word, which is the same staleness demand as the two cases above made of a
\ definition that compiled.
: REFUSED-NONE-CASE ( -- )
   s" a refusal that is not about a token leaves no token named, however recent the last one was" T-LABEL
   BND [: drop K-THROWS ;] IR-CTX:WITH-CONTEXT
   NELAB:REFUSED$ s" K5" T$=
   BND [: drop UNDER-THROWS ;] IR-CTX:WITH-CONTEXT
   NELAB:REFUSED-ROW -1 T=
   NELAB:REFUSED$ nip 0 T= ;

\ The clear a driver calls itself. What it is FOR cannot be shown from here - it
\ matters when a definition is refused before any elaboration begins, which needs
\ the engine and lives in test/compiler/native-migrate.f - so what this case
\ states is the one thing this suite owns: the word really does throw a record
\ away, so a driver that calls it before each attempt starts from nothing.
: REFUSED-RESET-CASE ( -- )
   s" a caller can throw the record away itself" T-LABEL
   BND [: drop K-THROWS ;] IR-CTX:WITH-CONTEXT
   NELAB:REFUSED$ s" K5" T$=
   NELAB:REFUSED-RESET
   NELAB:REFUSED-ROW -1 T=
   NELAB:REFUSED$ nip 0 T=
   NTAPE-KIND:NAME NELAB:REFUSED-KIND? TFALSE ;

: REFUSED-CEILING-CASE ( -- )
   s" a spelling that fills the record is answered whole, and one byte more is answered as nothing" T-LABEL
   BND [: drop FIT-THROWS ;] IR-CTX:WITH-CONTEXT
   NELAB:REFUSED$ BODY$ T$=
   NELAB:REFUSED$ nip NELAB:REFUSED-CAP T=
   BND [: drop OVER-THROWS ;] IR-CTX:WITH-CONTEXT
   NELAB:REFUSED$ nip 0 T=
   NELAB:REFUSED-ROW 1 T=
   NTAPE-KIND:NAME NELAB:REFUSED-KIND? TTRUE ;

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

\ ---- where the literal memo may cross a block boundary and where it may not ---
\ `LITMEMO dup 1 < if 1- then 1-`. The number one is written three times and the
\ four blocks answer for it differently, which is the whole of the memo's
\ dominance rule read off a published module.
\
\ THE ARM KEEPS THE ENTRY'S CONSTANT. The arm is opened by OPEN-PLAIN, whose one
\ predecessor is the two-way branch above it, so the entry block dominates the
\ arm and the constant it defined may be read there by name. The arm therefore
\ stages NO constant of its own, and the subtraction's second operand is asserted
\ to be the very value the entry block's constant defined - identity, not a count,
\ because "the arm has one fewer operation" would also be true of an arm that
\ computed one again and threw it away.
\
\ THE JOIN DOES NOT. The third `1-` is past `then`, in a block every arm reaches,
\ so no block a walk has just left dominates it and the memo is cleared there.
\ That block stages its own constant, and this is the assertion that fails if the
\ carrying is ever widened from OPEN-PLAIN to every opener.
\
\ THE STUB STAGES NOTHING. A stub and the arm are siblings, so a constant defined
\ in a stub would dominate neither the arm nor the join; STUB-H scopes the memo
\ for exactly that reason. Today a stub only crosses edge values and stages no
\ constant at all, and this is where that stays a measured fact.
\
\ IT IS THE `1-` SHAPE ON PURPOSE. The entry's constant comes off the tape and
\ the arm's is a constant-and-operation word's own, so the two reach the memo by
\ the two different routes into EMIT-LIT and are proved to meet in it. That is
\ also the shape test/compiler/native-chain.f RSPILL-CASE is built on.
: LITMEMO-BODY ( IR-CTX:ctx -- n n n n n bool )
   {: c:IR-CTX:ctx :}
   s" LITMEMO dup 1 < if 1- then 1-" TEXT!
   c SEALED
   {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena v:IR-ARENA:view :}
   c b v p r 1 1 NELAB:COLON {: f:IR-ID:ir-fun-id :}
   c b IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   m f 0 F-BLK-AT {: e:IR-ID:ir-block-id :}
   m f 1 F-BLK-AT {: st:IR-ID:ir-block-id :}
   m f 2 F-BLK-AT {: arm:IR-ID:ir-block-id :}
   m f 3 F-BLK-AT {: jn:IR-ID:ir-block-id :}
   m e s" hir.const" 0 F-OPC-AT {: ec:IR-ID:ir-op-id :}
   m arm s" hir.sub" 0 F-OPC-AT {: sb:IR-ID:ir-op-id :}
   m f F-BLKS
   m e s" hir.const" F-OPC-N
   m st s" hir.const" F-OPC-N
   m arm s" hir.const" F-OPC-N
   m jn s" hir.const" F-OPC-N
   m sb 1 F-IN  m ec 0 F-OUT SAME? ;

: LITMEMO-CASE ( -- )
   s" the literal memo crosses into the arm and stops at the join" T-LABEL
   BND [: LITMEMO-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE 1 T= 0 T= 0 T= 1 T= 4 T= ;

\ ---- the memo tells an ADDRESS from a number that equals it -------------------
\ THE ONE CASE A MEMO KEYED ON THE NUMBER ALONE GETS WRONG. `CELL-A` pushes the
\ address of a `create`d word and an integer literal may hold any value at all,
\ including that one. Both reach EMIT-LIT, both carry the identical sixty-four
\ bits, and they are NOT the same literal: one is an address the relocation pass
\ has to find and rewrite when the region it names moves, the other is a number
\ that must survive untouched. A memo keyed on the value folds them into one
\ operation and gives whichever kind that operation ended up carrying to both
\ references - silently, because reuse is invisible by construction.
\
\ THE FIXTURE MAKES THE COLLISION REAL RATHER THAN ARRANGING IT. The number in
\ the tape is not a number chosen to look like an address: it IS the address, read
\ back out of the engine through the same NDICT:FIXED-VALUE the word model asks,
\ and formatted into the source text. So the two tokens cannot drift apart, and
\ the case cannot pass by the two values merely differing.
\
\ WHAT IS ASSERTED. Two `hir.const` operations in the block, not one - and the
\ two carry DIFFERENT `hir.addr` attributes, which is what makes the count mean
\ what it says. Asserting the count alone would pass for two constants that both
\ claimed to be addresses.
create LK-NUM 32 allot
variable LK-N

: LK-DIGITS ( n -- ptr u8 n ) {: v:n :}
   32 LK-N !
   v 0= if
      LK-N @ 1- LK-N !  48 LK-NUM LK-N @ + c!
      LK-NUM LK-N @ + 1 exit
   then
   v begin dup 0 > while
      dup 10 mod 48 +
      LK-N @ 1- LK-N !  LK-NUM LK-N @ + c!
      10 /
   repeat drop
   LK-NUM LK-N @ +  32 LK-N @ - ;

\ The address the engine gives the word this case is about, asked the way the
\ word model asks it, so the tape's literal and the model's fixed value are one
\ number by construction.
: LK-ADDR ( -- n )
   CELL-A!
   s" CELL-A" NDICT:FIXED-VALUE ;

: LK-TEXT! ( -- )
   s" LITKIND CELL-A " TEXT!
   LK-ADDR LK-DIGITS NSRC:TEXT+ ;

\ The `hir.addr` attribute of one `hir.const`, read off the frozen module at the
\ ordinal the schema declares it at - which test/compiler/native-hir.f pins, so
\ the two files cannot disagree about which ordinal carries which key.
: F-ADDR-OF ( IR-BUILD:module IR-ID:ir-op-id -- n )
   {: m:IR-BUILD:module op:IR-ID:ir-op-id :}
   m IR-BUILD:FATTR-ROWS
   m IR-BUILD:FOP-POOL m IR-BUILD:FOP-ROWS m IR-BUILD:FKEY op 1 IR-OP:FATTR@
   IR-ATTR:FINT@ ;

: LITKIND-BODY ( IR-CTX:ctx -- n n n bool )
   {: c:IR-CTX:ctx :}
   LK-TEXT!
   c SEALED-DATA
   {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena v:IR-ARENA:view :}
   c b v p r 0 2 NELAB:COLON {: f:IR-ID:ir-fun-id :}
   c b IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   m f F-BLK {: blk:IR-ID:ir-block-id :}
   m blk s" hir.const" 0 F-OPC-AT {: c0:IR-ID:ir-op-id :}
   m blk s" hir.const" 1 F-OPC-AT {: c1:IR-ID:ir-op-id :}
   m blk s" hir.const" F-OPC-N
   m c0 F-ADDR-OF
   m c1 F-ADDR-OF
   m c0 0 F-OUT  m c1 0 F-OUT  SAME? ;

: LITKIND-CASE ( -- )
   s" an address and an integer equal to it are two literals" T-LABEL
   BND [: LITKIND-BODY ;] IR-CTX:WITH-CONTEXT
   TFALSE
   HIR:ADDR-NONE T=
   HIR:ADDR-DATA T=
   2 T= ;

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

\ `SUM-TO-D 0 swap 0 do i + loop` - the same loop written with `do`. FIVE blocks
\ where `?do` has seven, and the two missing ones are exactly the guard's: the
\ block that tests `limit - start` and the stub it takes when they are equal.
\ Everything else is the same shape at the same widths - a header taking the
\ accumulator, the index and the limit, an exit stub, a latch back to the header,
\ and one join - which is what "`?do` is `do` with a zero-trip guard" means once
\ it is blocks. The entry block therefore ends on ONE successor here and on a
\ two-way branch there, and that single ordinal is what says no test was built.
\
\ IT IS PINNED BESIDE SUM-TO ON PURPOSE. The two cases share every number but
\ the block count and the entry's terminator, so a change that gave `do` a guard,
\ or took `?do`'s away, moves one of them and not the other.
: SUMTO-DO-BODY ( IR-CTX:ctx -- n n n n n n n n )
   {: c:IR-CTX:ctx :}
   s" SUM-TO-D 0 swap 0 do i + loop" TEXT!
   c SEALED
   {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena v:IR-ARENA:view :}
   c b v p r 1 1 NELAB:COLON {: f:IR-ID:ir-fun-id :}
   c b IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   m f F-BLKS
   m f 0 F-BLK-AT {: e:IR-ID:ir-block-id :}
   m f 1 F-BLK-AT {: hd:IR-ID:ir-block-id :}
   m f 2 F-BLK-AT {: xt:IR-ID:ir-block-id :}
   m f 3 F-BLK-AT {: la:IR-ID:ir-block-id :}
   m f 4 F-BLK-AT {: jn:IR-ID:ir-block-id :}
   m  m e F-TERM  F-SUCCS
   m  m e F-TERM  0 F-SUCC
   m hd F-ARGS
   m  m hd F-TERM  0 F-SUCC
   m  m hd F-TERM  1 F-SUCC
   m  m xt F-TERM  0 F-SUCC
   m  m la F-TERM  0 F-SUCC ;

: SUMTO-DO-CASE ( -- )
   s" a plain do builds the same loop without the guard block and its stub" T-LABEL
   BND [: SUMTO-DO-BODY ;] IR-CTX:WITH-CONTEXT
   1 T= 4 T= 3 T= 2 T= 3 T= 1 T= 1 T= 5 T= ;

\ `FOREVER begin 1- again`. TWO blocks: the entry and the loop header - and the
\ absence of a third is the whole of what `again` is. `until` closes the same
\ loop with a latch and an exit and builds four (COUNTDOWN above); `again` has no
\ test, so it has no two-way branch and no latch stub, and it has no exit edge,
\ so no block after the loop is opened. The header's terminator therefore names
\ ONE successor and that successor is the header itself: the back edge is the
\ block's own last operation rather than a stub branching to it.
\
\ AND THE FUNCTION HAS NO BLOCK CONTROL LEAVES THROUGH, which is pinned here as
\ the count: two blocks, each with a successor. That is the shape
\ src/compiler/native/regalloc.f calls NO-RET, and it is what makes a
\ `begin … again` word a routine with no return convention at all rather than one
\ whose return nothing branches to.
: FOREVER-BODY ( IR-CTX:ctx -- n n n n n )
   {: c:IR-CTX:ctx :}
   s" FOREVER begin 1- again" TEXT!
   c SEALED
   {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena v:IR-ARENA:view :}
   c b v p r 1 1 NELAB:COLON {: f:IR-ID:ir-fun-id :}
   c b IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   m f F-BLKS
   m f 0 F-BLK-AT {: e:IR-ID:ir-block-id :}
   m f 1 F-BLK-AT {: hd:IR-ID:ir-block-id :}
   m  m e F-TERM  0 F-SUCC
   m hd F-ARGS
   m  m hd F-TERM  F-SUCCS
   m  m hd F-TERM  0 F-SUCC ;

: FOREVER-CASE ( -- )
   s" a begin-again loop is a header branching to itself and no block after it" T-LABEL
   BND [: FOREVER-BODY ;] IR-CTX:WITH-CONTEXT
   1 T= 1 T= 1 T= 1 T= 2 T= ;

\ `SUMLV 0 swap 0 ?do i 2 = if leave then i + loop`. TEN blocks: the seven a
\ `?do` already builds (SUMTO above), plus the three the `if` adds - the stub its
\ false edge leaves through, the arm the `leave` ends, and the block `then`
\ opens. The `leave`'s own block IS that arm: it opens no block of its own,
\ because the block it branches to is one the loop was always going to have.
\
\ THE ORDINAL IT BRANCHES TO IS THE LOOP'S JOIN, AND THAT IS WHAT THIS CASE
\ MEASURES. Block 9 is the block after the loop, and here THREE edges reach it
\ where two reach it in SUMTO: the skip stub the guard takes when the loop runs
\ no turns, the exit stub at `loop` (block 7, whose successor is pinned below),
\ and the `leave` in block 5. A `leave` wired to the latch, to the header, or to
\ the `if`'s own join would show a different ordinal on the arm's edge while the
\ exit stub's stayed put, and the join's argument count says what it carries is
\ the loop's live vector rather than the arm's.
: SUMLV-BODY ( IR-CTX:ctx -- n n n n n )
   {: c:IR-CTX:ctx :}
   s" SUMLV 0 swap 0 ?do i 2 = if leave then i + loop" TEXT!
   c SEALED
   {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena v:IR-ARENA:view :}
   c b v p r 1 1 NELAB:COLON {: f:IR-ID:ir-fun-id :}
   c b IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   m f F-BLKS
   m f 5 F-BLK-AT {: arm:IR-ID:ir-block-id :}
   m f 9 F-BLK-AT {: jn:IR-ID:ir-block-id :}
   m  m arm F-TERM  F-SUCCS
   m  m arm F-TERM  0 F-SUCC
   m jn F-ARGS
   m f 7 F-BLK-AT {: xt:IR-ID:ir-block-id :}
   m  m xt F-TERM  0 F-SUCC ;

: SUMLV-CASE ( -- )
   s" a leave branches out of the loop to the block loop's own exit reaches" T-LABEL
   BND [: SUMLV-BODY ;] IR-CTX:WITH-CONTEXT
   9 T= 1 T= 9 T= 1 T= 10 T= ;

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

\ ---- what a plain `do` still refuses -----------------------------------------
\ `do` takes the same pair `?do` takes, so it refuses the same two things about
\ it, and the four cases below are what says the new opener kept them.

\ Only one value under the pair the loop opens with.
: DOUNDER-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   s" DOUNDER 1 do 2 loop" TEXT!
   c SEALED
   {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena v:IR-ARENA:view :}
   c b v p r 0 1 NELAB:COLON drop ;

: DOUNDER ( -- )
   BND [: DOUNDER-BODY ;] IR-CTX:WITH-CONTEXT ;

\ A double as the limit. It is made with `s>f` rather than written as a float
\ literal because this suite's lexer has no float literals - a run of digits is
\ an integer and everything else is a name - so `1.0` would arrive as an
\ unmodelled NAME and the case would be measuring the lexer.
\
\ THE PAIR IS THE PIN ON DO-PAIR BEING ONE SEAM: `?do` would refuse this at its
\ own subtraction whatever this file said, so the `do` half is what the shared
\ rule buys, and both are here because a rule that held for one opener and not
\ the other is exactly the bug worth catching.
: DODBL-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   s" DODBL 0 s>f 0 do 2 + loop" TEXT!
   c SEALED
   {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena v:IR-ARENA:view :}
   c b v p r 0 1 NELAB:COLON drop ;

: DODBL ( -- )
   BND [: DODBL-BODY ;] IR-CTX:WITH-CONTEXT ;

: QDODBL-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   s" QDODBL 0 s>f 0 ?do 2 + loop" TEXT!
   c SEALED
   {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena v:IR-ARENA:view :}
   c b v p r 0 1 NELAB:COLON drop ;

: QDODBL ( -- )
   BND [: QDODBL-BODY ;] IR-CTX:WITH-CONTEXT ;

\ A `do` nothing closes. It is refused by CROSS-SCAN, which counts loop openers
\ and closers before either of the two block walks runs - and that is the site
\ this whole leaf turns on: before `do` was modelled it was the `loop` of a
\ perfectly good plain-do body that arrived here with nothing open, so the
\ refusal named a balanced program.
: DOOPEN-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   s" DOOPEN 3 0 do 1" TEXT!
   c SEALED
   {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena v:IR-ARENA:view :}
   c b v p r 0 1 NELAB:COLON drop ;

: DOOPEN ( -- )
   BND [: DOOPEN-BODY ;] IR-CTX:WITH-CONTEXT ;

\ A `do` closed by `until`. The frame a counted loop pushes is not a `begin`'s,
\ whichever of the two words opened it.
: DOUNTIL-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   s" DOUNTIL 3 0 do 1 until" TEXT!
   c SEALED
   {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena v:IR-ARENA:view :}
   c b v p r 0 1 NELAB:COLON drop ;

: DOUNTIL ( -- )
   BND [: DOUNTIL-BODY ;] IR-CTX:WITH-CONTEXT ;

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

: DOUNDER-CASE ( -- )
   s" a do with only one value under it is refused" T-LABEL
   [: DOUNDER ;] E-NELAB-UNDER TTHROWSQ ;

: DODBL-CASE ( -- )
   s" a do whose limit is a double is refused" T-LABEL
   [: DODBL ;] E-NELAB-TYPE TTHROWSQ ;

: QDODBL-CASE ( -- )
   s" a ?do whose limit is a double is refused at the same seam" T-LABEL
   [: QDODBL ;] E-NELAB-TYPE TTHROWSQ ;

: DOOPEN-CASE ( -- )
   s" a do nothing closes is refused" T-LABEL
   [: DOOPEN ;] E-NELAB-CTRL TTHROWSQ ;

: DOUNTIL-CASE ( -- )
   s" a do closed by until is refused" T-LABEL
   [: DOUNTIL ;] E-NELAB-CTRL TTHROWSQ ;

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

\ ---- a second group names what the first one's names computed ----------------
\ THE SHAPE THE TREE WRITES CONSTANTLY: bind the arguments, compute, name the
\ result, compute again. `b a - {: d:n :} d d *` squares the difference, and the
\ two names come from two groups.
\
\ WHAT THE MODULE HAS TO SHOW, AND IT IS NOT "it compiled". The subtraction's
\ operands are the two entry arguments the right way round - a frame that bound
\ the second group's name to the wrong value would still compile - and the
\ multiply reads the SUBTRACTION's result twice, which is the whole claim: `d`
\ is one value, defined by the operation that stood before the group, and read
\ where the body names it. Three operations for eleven body tokens - the
\ subtraction, the multiply and the return: neither group stages anything, and
\ neither does a mention of a name. Four values, which is the two entry
\ arguments and the two results; a group that had staged a move would show one
\ more of each.
: TWO-GROUPS-BODY ( IR-CTX:ctx -- n bool bool bool bool n )
   {: c:IR-CTX:ctx :}
   s" TWOG {: a:n b:n :} b a - {: d:n :} d d *" TEXT!
   c SEALED
   {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena v:IR-ARENA:view :}
   c b v p r 2 1 NELAB:COLON {: f:IR-ID:ir-fun-id :}
   c b IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   m f F-BLK {: blk:IR-ID:ir-block-id :}
   m blk F-OPS
   m blk 0 F-OP {: sb:IR-ID:ir-op-id :}
   m blk 1 F-OP {: ml:IR-ID:ir-op-id :}
   m sb 0 F-IN  m blk 1 F-ARG SAME?
   m sb 1 F-IN  m blk 0 F-ARG SAME?
   m ml 0 F-IN  m sb 0 F-OUT SAME?
   m ml 1 F-IN  m sb 0 F-OUT SAME?
   m F-VALUES ;

: TWO-GROUPS-CASE ( -- )
   s" a second locals group names what the first group's names computed" T-LABEL
   BND [: TWO-GROUPS-BODY ;] IR-CTX:WITH-CONTEXT
   4 T=
   TTRUE TTRUE TTRUE TTRUE
   3 T= ;

\ ---- a local named after a word the dialect models ---------------------------
\ THE NAME IS THE PROGRAM'S, AND THAT IS THE ENGINE'S ANSWER RATHER THAN A
\ CHOICE. `: T ( n -- n ) {: i:n :} 0 3 0 ?do i + loop ;` answers 15 for 5 -
\ three turns of the LOCAL - while the same body without the declaration answers
\ 3, the loop INDEX; `{: dup:n :} dup dup +` doubles; `{: if:n :} if if +`
\ doubles. test/compiler/native-migrate.f holds each of those answers against
\ the engine's own compilation of the same text. What is measured HERE is the
\ module, because an answer cannot say which of two readings produced it when
\ both readings are legal programs.
\
\ THE FOUR BODIES ASK FOUR DIFFERENT READERS. A primitive's name asks the walk;
\ the loop index's name asks the block builder as well, since a mention read as
\ the index would take the header's argument instead of the body's; `of` is read
\ by the tag-dispatch pre-pass and `is` by the deferred-word pre-pass, and
\ neither of those two passes builds anything the walk would refuse - a mention
\ they claimed would silently become an operand of a form that is not there.
: PRIMLOC-BODY ( IR-CTX:ctx -- n bool bool bool bool n )
   {: c:IR-CTX:ctx :}
   s" PRIML {: dup:n :} dup dup +" TEXT!
   c SEALED
   {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena v:IR-ARENA:view :}
   c b v p r 1 1 NELAB:COLON {: f:IR-ID:ir-fun-id :}
   c b IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   m f F-BLK {: blk:IR-ID:ir-block-id :}
   m blk F-OPS
   m blk 0 F-OP {: ad:IR-ID:ir-op-id :}
   m ad s" hir.add" F-OPC?
   m  m blk 1 F-OP  s" hir.return" F-OPC?
   m ad 0 F-IN  m blk 0 F-ARG SAME?
   m ad 1 F-IN  m blk 0 F-ARG SAME?
   m F-VALUES ;

: PRIMLOC-CASE ( -- )
   s" a local named after a primitive is that local, and stages no operation of its own" T-LABEL
   BND [: PRIMLOC-BODY ;] IR-CTX:WITH-CONTEXT
   2 T= TTRUE TTRUE TTRUE TTRUE 2 T= ;

\ `IDXL {: i:n :} 0 3 0 ?do i + loop`. The block shape is SUM-TO's, which this
\ file already measures: seven blocks, and a header taking the accumulator, the
\ index and the limit - three arguments, because those are the three values a
\ turn of the loop changes.
\
\ WHICH VALUE THE ADDITION READS IS THE WHOLE CASE, and the two readings are
\ told apart by exactly that. The accumulator is the header's first argument in
\ both readings. The second operand is the header's SECOND argument if `i` is
\ the loop index, and the DEFINITION'S OWN ENTRY ARGUMENT if `i` is the local -
\ a value the entry block defines and therefore dominates the loop with, so it
\ needs no argument of the header at all. Both are asserted, one true and one
\ false, because the count alone says nothing: SUM-TO's header has three
\ arguments too.
: IDXLOC-BODY ( IR-CTX:ctx -- n n bool bool bool )
   {: c:IR-CTX:ctx :}
   s" IDXL {: i:n :} 0 3 0 ?do i + loop" TEXT!
   c SEALED
   {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena v:IR-ARENA:view :}
   c b v p r 1 1 NELAB:COLON {: f:IR-ID:ir-fun-id :}
   c b IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   m f F-BLKS
   m f 0 F-BLK-AT {: e:IR-ID:ir-block-id :}
   m f 3 F-BLK-AT {: hd:IR-ID:ir-block-id :}
   m hd F-ARGS
   m hd s" hir.add" 0 F-OPC-AT {: ad:IR-ID:ir-op-id :}
   m ad 0 F-IN  m hd 0 F-ARG SAME?
   m ad 1 F-IN  m e 0 F-ARG SAME?
   m ad 1 F-IN  m hd 1 F-ARG SAME? ;

: IDXLOC-CASE ( -- )
   s" a local named after the loop index is the local the loop body adds, not the index" T-LABEL
   BND [: IDXLOC-BODY ;] IR-CTX:WITH-CONTEXT
   TFALSE TTRUE TTRUE 3 T= 7 T= ;

: OFLOC-BODY ( IR-CTX:ctx -- n bool bool bool )
   {: c:IR-CTX:ctx :}
   s" OFL {: of:n :} of of +" TEXT!
   c SEALED
   {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena v:IR-ARENA:view :}
   c b v p r 1 1 NELAB:COLON {: f:IR-ID:ir-fun-id :}
   c b IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   m f F-BLK {: blk:IR-ID:ir-block-id :}
   m blk F-OPS
   m blk 0 F-OP {: ad:IR-ID:ir-op-id :}
   m ad s" hir.add" F-OPC?
   m ad 0 F-IN  m blk 0 F-ARG SAME?
   m ad 1 F-IN  m blk 0 F-ARG SAME? ;

: OFLOC-CASE ( -- )
   s" a local named after an arm keyword opens no arm" T-LABEL
   BND [: OFLOC-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE TTRUE 2 T= ;

: ISLOC-BODY ( IR-CTX:ctx -- n bool bool bool )
   {: c:IR-CTX:ctx :}
   s" ISL {: is:n :} is is +" TEXT!
   c SEALED
   {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena v:IR-ARENA:view :}
   c b v p r 1 1 NELAB:COLON {: f:IR-ID:ir-fun-id :}
   c b IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   m f F-BLK {: blk:IR-ID:ir-block-id :}
   m blk F-OPS
   m blk 0 F-OP {: ad:IR-ID:ir-op-id :}
   m ad s" hir.add" F-OPC?
   m ad 0 F-IN  m blk 0 F-ARG SAME?
   m ad 1 F-IN  m blk 0 F-ARG SAME? ;

: ISLOC-CASE ( -- )
   s" a local named after the deferred-word keyword claims no operand" T-LABEL
   BND [: ISLOC-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE TTRUE 2 T= ;

\ ---- what a locals frame refuses ---------------------------------------------
\ Each body below is a shape this elaborator has no rule for, or one ceiling it
\ will not overrun, and each one is refused by name rather than compiled into
\ something else. Rebinding a local and taking its address need no case here:
\ `to` and `^` are not words of the dialect at all, so they are already refused
\ as E-HIR-UNMODELED by the case above them in this file.
: NESTED-GROUP-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   s" NESTG {: a:n {: b:n :} :} a" TEXT!
   c SEALED
   {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena v:IR-ARENA:view :}
   c b v p r 2 1 NELAB:COLON drop ;

: NESTED-GROUP ( -- )
   BND [: NESTED-GROUP-BODY ;] IR-CTX:WITH-CONTEXT ;

\ A closer with no opener. The pre-pass is looking for an opener and passes over
\ it, so the row the WALK meets is one no group claims - which is the whole
\ reason the walk holds its arrival against the row the pre-pass recorded for
\ the group it is binding next. The first body has a group for that closer to be
\ mistaken for and reaches the row check; the second has none and reaches the
\ groups-exhausted check. The two are BACKSTOPS FOR EACH OTHER and that is
\ measured rather than assumed: deleting either one alone still refuses both
\ bodies, and deleting both lets the lone closer compile.
: ORPHAN-CLOSE-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   s" ORPHC :} {: a:n :} a" TEXT!
   c SEALED
   {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena v:IR-ARENA:view :}
   c b v p r 1 1 NELAB:COLON drop ;

: ORPHAN-CLOSE ( -- )
   BND [: ORPHAN-CLOSE-BODY ;] IR-CTX:WITH-CONTEXT ;

: LONE-CLOSE-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   s" LONEC :} 0" TEXT!
   c SEALED
   {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena v:IR-ARENA:view :}
   c b v p r 0 1 NELAB:COLON drop ;

: LONE-CLOSE ( -- )
   BND [: LONE-CLOSE-BODY ;] IR-CTX:WITH-CONTEXT ;

\ More groups than the tables hold. A group that declares no name is legal
\ source - the engine parses `{: :}` and binds nothing (measured) - so the group
\ ceiling is reachable without reaching the name ceiling, and it is a table
\ bound rather than a shape rule: seventeen groups is one past LMAX.
: GROUP-CAP-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   s" GCAP {: :} {: :} {: :} {: :} {: :} {: :} {: :} {: :} {: :} {: :} {: :} {: :} {: :} {: :} {: :} {: :} {: :} 0" TEXT!
   c SEALED
   {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena v:IR-ARENA:view :}
   c b v p r 0 1 NELAB:COLON drop ;

: GROUP-CAP ( -- )
   BND [: GROUP-CAP-BODY ;] IR-CTX:WITH-CONTEXT ;

: OPEN-GROUP-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   s" OPENG {: a:n" TEXT!
   c SEALED
   {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena v:IR-ARENA:view :}
   c b v p r 1 1 NELAB:COLON drop ;

: OPEN-GROUP ( -- )
   BND [: OPEN-GROUP-BODY ;] IR-CTX:WITH-CONTEXT ;

\ The one name a local may not take. Every pass that reads a body token for what
\ it MEANS asks LOCAL-OF before it asks the word model - so a declared name means
\ the local everywhere - except the one that runs BEFORE the locals frame exists: the
\ quotation pre-scan, whose spans every later walk steps over. Its two tokens are
\ `[:` and `;]`, and only the closer can ever BE a local's name: a name is the
\ bytes before the annotation's colon, so `[:` names `[`.
\
\ THE BODY PAIRS THE QUOTATION SO THAT THE PRE-SCAN IS SATISFIED, which is what
\ makes this refusal the locals frame's and not the quotation's: a stray `;]`
\ with no opener is refused as E-NELAB-QUOT before the locals pass ever runs,
\ and would prove nothing about a declared name. So the pair is opened outside
\ the group and closed inside it, and the token the group reads as a name is the
\ closer.
: QCLOSE-NAME-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   s" QCN [: {: ;] :} 0" TEXT!
   c SEALED
   {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena v:IR-ARENA:view :}
   c b v p r 1 1 NELAB:COLON drop ;

: QCLOSE-NAME ( -- )
   BND [: QCLOSE-NAME-BODY ;] IR-CTX:WITH-CONTEXT ;

\ And the other half of the same sentence, held against its real owner. A group
\ that writes the OPENER declares `[` - which is nobody's word - and has put its
\ own closer inside the span that token opened, so what refuses this body is the
\ quotation's own check on a locals group, by the quotation's code. Without this
\ case the section above could claim the opener for itself and nothing would say
\ otherwise.
: QOPEN-NAME-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   s" QON {: [: :} ;] 0" TEXT!
   c SEALED
   {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena v:IR-ARENA:view :}
   c b v p r 1 1 NELAB:COLON drop ;

: QOPEN-NAME ( -- )
   BND [: QOPEN-NAME-BODY ;] IR-CTX:WITH-CONTEXT ;

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

: NESTED-GROUP-CASE ( -- )
   s" a group opened inside an open group is refused" T-LABEL
   [: NESTED-GROUP ;] E-NELAB-LOCAL TTHROWSQ ;

: ORPHAN-CLOSE-CASE ( -- )
   s" a closer standing before its definition's group is refused" T-LABEL
   [: ORPHAN-CLOSE ;] E-NELAB-LOCAL TTHROWSQ ;

: LONE-CLOSE-CASE ( -- )
   s" a closer in a definition that opens no group at all is refused" T-LABEL
   [: LONE-CLOSE ;] E-NELAB-LOCAL TTHROWSQ ;

: GROUP-CAP-CASE ( -- )
   s" more groups than the tables hold is refused as a ceiling, by its own code" T-LABEL
   [: GROUP-CAP ;] E-NELAB-LOCAL-CAP TTHROWSQ ;

: OPEN-GROUP-CASE ( -- )
   s" a locals group the body never closes is refused" T-LABEL
   [: OPEN-GROUP ;] E-NELAB-LOCAL TTHROWSQ ;

: QCLOSE-NAME-CASE ( -- )
   s" a local named after the quotation closer is refused" T-LABEL
   [: QCLOSE-NAME ;] E-NELAB-LOCAL TTHROWSQ ;

: QOPEN-NAME-CASE ( -- )
   s" and a group that writes the opener is refused as the quotation's" T-LABEL
   [: QOPEN-NAME ;] E-NELAB-QUOT TTHROWSQ ;

: TWICE-CASE ( -- )
   s" the same local declared twice is refused" T-LABEL
   [: TWICE ;] E-NELAB-LOCAL TTHROWSQ ;

: NESTED-CASE ( -- )
   s" a locals group inside a control structure is refused" T-LABEL
   [: NESTED ;] E-NELAB-LOCAL TTHROWSQ ;

\ ---- a double crossing a block edge, in both directions -----------------------
\ THE SEAM THESE TWO CASES ARE ABOUT. A block argument's type has to be stated
\ when the block is OPENED, and the values that will reach it come from arms
\ written at different places in the body - so it cannot be read off "the value
\ arriving", because there are two of them. The rule is that the FIRST edge into
\ a block states each position's type and every later edge crosses its value to
\ what was stated, with `hir.bits>real` or `hir.real>bits`, neither of which
\ computes anything.
\
\ WHY THE CROSSING IS READ OFF THE MODULE AND NOT OFF AN ANSWER. Both bodies
\ below answer the same eight bytes whichever way the join is typed - that is the
\ whole point of a crossing that computes nothing - so an execution test cannot
\ tell one from the other. What distinguishes them is WHICH block holds the
\ crossing, and that is a fact about the module: the arm whose type was stated
\ carries no crossing, and the arm that had to agree carries exactly one.
\
\ `s>f` IS WHERE THE DOUBLE COMES FROM, and it is used rather than a float
\ literal on purpose: this fixture's lexer reads integers and names, and teaching
\ it the engine's `int.frac` reader would be a second copy of a reader that
\ already exists. `hir.int>real` answers a double from a cell, which is all these
\ cases need.

\ The double arrives FIRST. `dup 0= if s>f else then` states the join's one
\ position from the arm that converts, so the empty arm - which still holds the
\ cell the word was entered with - is the one that crosses.
: JOINR-BODY ( IR-CTX:ctx -- n bool bool bool bool bool bool bool bool bool bool )
   {: c:IR-CTX:ctx :}
   s" JOINR dup 0= if s>f else then" TEXT!
   c SEALED
   {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena v:IR-ARENA:view :}
   c b v p r 1 1 NELAB:COLON {: f:IR-ID:ir-fun-id :}
   c b IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   m f F-BLKS
   m f 2 F-BLK-AT {: arm:IR-ID:ir-block-id :}
   m f 3 F-BLK-AT {: other:IR-ID:ir-block-id :}
   m f 4 F-BLK-AT {: join:IR-ID:ir-block-id :}

   \ the arm that stated the type converts and hands the double straight over
   m  m arm 0 F-OP  s" hir.int>real" F-OPC?
   m  m arm F-TERM  s" hir.br" F-OPC?
   m  m arm F-TERM 0 F-IN   m  m arm 0 F-OP  0 F-OUT SAME?

   \ the arm that had to agree crosses its cell, and hands the crossing over
   m other F-OPS 2 =
   m  m other 0 F-OP  s" hir.bits>real" F-OPC?
   m  m other 0 F-OP 0 F-IN   m other 0 F-ARG SAME?
   m  m other F-TERM 0 F-IN   m  m other 0 F-OP  0 F-OUT SAME?

   \ and the join's own argument is a double, which is why leaving the word
   \ crosses it back into the cell the caller's slot holds
   m  m join 0 F-OP  s" hir.real>bits" F-OPC?
   m  m join 0 F-OP 0 F-IN   m join 0 F-ARG SAME?
   m  m join F-TERM  s" hir.return" F-OPC? ;

: JOINR-CASE ( -- )
   s" the first edge into a join states its type and the second crosses to it" T-LABEL
   BND [: JOINR-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE  5 T= ;

\ The cell arrives first. `dup if s>f then` has no `else`, so the edge that
\ states the join's position is the `if`'s own false stub, which carries the cell
\ the word was entered with - and the arm that converts is the one that crosses
\ back.
: JOINC-BODY ( IR-CTX:ctx -- n n bool bool bool bool bool bool )
   {: c:IR-CTX:ctx :}
   s" JOINC dup if s>f then" TEXT!
   c SEALED
   {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena v:IR-ARENA:view :}
   c b v p r 1 1 NELAB:COLON {: f:IR-ID:ir-fun-id :}
   c b IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   m f F-BLKS
   m f 2 F-BLK-AT {: arm:IR-ID:ir-block-id :}
   m f 3 F-BLK-AT {: join:IR-ID:ir-block-id :}

   \ the converting arm crosses its double back before it hands it over
   m arm F-OPS
   m  m arm 0 F-OP  s" hir.int>real" F-OPC?
   m  m arm 1 F-OP  s" hir.real>bits" F-OPC?
   m  m arm 1 F-OP 0 F-IN   m  m arm 0 F-OP  0 F-OUT SAME?
   m  m arm F-TERM 0 F-IN   m  m arm 1 F-OP  0 F-OUT SAME?

   \ and the join's argument is a cell, so leaving the word crosses nothing
   m join F-OPS 1 =
   m  m join F-TERM  s" hir.return" F-OPC? ;

: JOINC-CASE ( -- )
   s" and where the CELL arrives first the arm that converts is the one that crosses" T-LABEL
   BND [: JOINC-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE  3 T=  4 T= ;

\ ---- the case a body writes the dialect's words in ---------------------------
\ A Habu name is case-insensitive to the ENGINE - src/habu/habu2.f LKWCMP folds
\ the letters of a keyword and src/habu/habu1.f's dictionary find folds the
\ letters of a name - so a body that writes `IF` or `SWAP` runs exactly what one
\ writing `if` or `swap` runs. The word model keys its rows by that same fold
\ (src/compiler/native/hir-word.f KEY-SYM), and what these cases measure is the
\ consequence: the definition a body in capitals becomes is the definition its
\ lower-case twin becomes, operation for operation.
\
\ WHY THE MODULES ARE COMPARED AND NOT JUST BOTH COMPILED. "Both compile" is
\ exactly what the failure this fixes looked like from outside. A capitalised
\ RENAME or OPERATION was not refused: it fell through the resolve pass to the
\ ENGINE, which answered - case-insensitively - with the address of a real word,
\ so `SWAP` compiled as a wordcall where `swap` compiles as nothing at all. Two
\ definitions that both compile can therefore be a fast one and a slow one, or a
\ right one and a wrong one; only reading both modules back says they are the
\ same program. So a twin case reads every block, every operation, its opcode by
\ name, its operands, its results and its successors, and the comparator's own
\ falsifier below proves it can say no.
\
\ AND A LOCAL IS NOT FOLDED, WHICH IS THE OTHER HALF OF THE DECISION. The engine's
\ local lookup (src/habu/habu2.f EMIT-LOC-FIND) compares a local's name BYTE FOR
\ BYTE where its keyword and dictionary compares fold, and the engine was asked
\ rather than assumed: with `{: I:n :}` bound, `: TUP ( n -- n ) {: I:n :} 0 3 0
\ ?do I + loop ;` answers 15 for 5 - the local - and the same definition written
\ `?do i + loop` answers 3 - the loop index. The two spellings are two things
\ there, so they stay two things here: the elaborator matches a local by the
\ bytes the body wrote, and only the mention that is NOT a local is put to the
\ word model under its key. The pair of cases at the end of this section holds
\ the chain to that split.
64 constant TW-CAP

create TW-BUF TW-CAP allot

\ One module's operation, named by the opcode spelling its own module holds, held
\ against the other module's operation. The name is copied out and compared as
\ bytes because two modules intern their own symbols: the same opcode is a
\ different ordinal in each, and comparing ordinals would call every pair equal
\ or every pair different depending on the order the two were built in.
: TW-OPC$ ( IR-BUILD:module IR-ID:ir-op-id -- ptr u8 n )
   {: m:IR-BUILD:module op:IR-ID:ir-op-id :}
   TW-BUF
   m IR-BUILD:FSYM-POOL m IR-BUILD:FSYM-ROWS
   m IR-BUILD:FOP-ROWS m IR-BUILD:FKEY op IR-OP:FOPCODE@
   TW-BUF TW-CAP IR-SYM:FCOPY ;

: TW-OPC? ( IR-BUILD:module IR-ID:ir-op-id IR-BUILD:module IR-ID:ir-op-id -- bool )
   {: ma:IR-BUILD:module oa:IR-ID:ir-op-id mb:IR-BUILD:module ob:IR-ID:ir-op-id :}
   ma oa TW-OPC$ {: a u:n :} \ typed-local-lint: allow-bare-local - a keeps the ptr u8 byte-span role
   mb ob a u F-OPC? ;

\ Operands, results and successors are compared as the module-local ordinals they
\ are: two twins that name their values and blocks in the same order are the same
\ dataflow and the same control flow.
: TW-INS? ( IR-BUILD:module IR-ID:ir-op-id IR-BUILD:module IR-ID:ir-op-id -- bool )
   {: ma:IR-BUILD:module oa:IR-ID:ir-op-id mb:IR-BUILD:module ob:IR-ID:ir-op-id :}
   ma oa F-INS mb ob F-INS <> if false exit then
   true
   ma oa F-INS 0 ?do
      ma oa i F-IN IR-ID:VALUE-LOCAL
      mb ob i F-IN IR-ID:VALUE-LOCAL <> if drop false leave then
   loop ;

: TW-OUTS? ( IR-BUILD:module IR-ID:ir-op-id IR-BUILD:module IR-ID:ir-op-id -- bool )
   {: ma:IR-BUILD:module oa:IR-ID:ir-op-id mb:IR-BUILD:module ob:IR-ID:ir-op-id :}
   ma oa F-OUTS mb ob F-OUTS <> if false exit then
   true
   ma oa F-OUTS 0 ?do
      ma oa i F-OUT IR-ID:VALUE-LOCAL
      mb ob i F-OUT IR-ID:VALUE-LOCAL <> if drop false leave then
   loop ;

: TW-SUCCS? ( IR-BUILD:module IR-ID:ir-op-id IR-BUILD:module IR-ID:ir-op-id -- bool )
   {: ma:IR-BUILD:module oa:IR-ID:ir-op-id mb:IR-BUILD:module ob:IR-ID:ir-op-id :}
   ma oa F-SUCCS mb ob F-SUCCS <> if false exit then
   true
   ma oa F-SUCCS 0 ?do
      ma oa i F-SUCC  mb ob i F-SUCC <> if drop false leave then
   loop ;

: TW-OP? ( IR-BUILD:module IR-ID:ir-op-id IR-BUILD:module IR-ID:ir-op-id -- bool )
   {: ma:IR-BUILD:module oa:IR-ID:ir-op-id mb:IR-BUILD:module ob:IR-ID:ir-op-id :}
   ma oa mb ob TW-OPC? 0= if false exit then
   ma oa mb ob TW-INS? 0= if false exit then
   ma oa mb ob TW-OUTS? 0= if false exit then
   ma oa mb ob TW-SUCCS? ;

: TW-BLOCK? ( IR-BUILD:module IR-ID:ir-block-id IR-BUILD:module IR-ID:ir-block-id -- bool )
   {: ma:IR-BUILD:module ba:IR-ID:ir-block-id mb:IR-BUILD:module bb:IR-ID:ir-block-id :}
   ma ba F-ARGS mb bb F-ARGS <> if false exit then
   ma ba F-OPS mb bb F-OPS <> if false exit then
   true
   ma ba F-OPS 0 ?do
      ma  ma ba i F-OP  mb  mb bb i F-OP  TW-OP? 0= if drop false leave then
   loop ;

: TW-FUN? ( IR-BUILD:module IR-ID:ir-fun-id IR-BUILD:module IR-ID:ir-fun-id -- bool )
   {: ma:IR-BUILD:module fa:IR-ID:ir-fun-id mb:IR-BUILD:module fb:IR-ID:ir-fun-id :}
   ma F-TOTAL mb F-TOTAL <> if false exit then
   ma F-VALUES mb F-VALUES <> if false exit then
   ma fa F-BLKS mb fb F-BLKS <> if false exit then
   true
   ma fa F-BLKS 0 ?do
      ma  ma fa i F-BLK-AT  mb  mb fb i F-BLK-AT  TW-BLOCK? 0= if drop false leave then
   loop ;

\ One source text, compiled and frozen, as the pair a comparison takes.
: BUILT ( IR-CTX:ctx ptr u8 n n n -- IR-BUILD:module IR-ID:ir-fun-id )
   {: c:IR-CTX:ctx a u:n in:n out:n :} \ typed-local-lint: allow-bare-local - a keeps the ptr u8 byte-span role
   a u TEXT!
   c SEALED
   {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena v:IR-ARENA:view :}
   c b v p r in out NELAB:COLON {: f:IR-ID:ir-fun-id :}
   c b IR-BUILD:FREEZE f ;

\ The control words: `if`, `else` and `then`, with a rename and both step words
\ around them. This is the shape the census counted 137 refusals of.
: CTRL-TWIN-BODY ( IR-CTX:ctx -- bool )
   {: c:IR-CTX:ctx :}
   c s" TWIN dup 0 > if 1+ else 1- then" 1 1 BUILT
   {: ma:IR-BUILD:module fa:IR-ID:ir-fun-id :}
   c s" TWIN DUP 0 > IF 1+ ELSE 1- THEN" 1 1 BUILT
   {: mb:IR-BUILD:module fb:IR-ID:ir-fun-id :}
   ma fa mb fb TW-FUN? ;

\ The renames and the operations, which are the class that did not refuse: every
\ word here is one the engine's dictionary would have answered for, so a table
\ that missed them compiled a branch per word instead of a rename or one
\ instruction.
: WORD-TWIN-BODY ( IR-CTX:ctx -- bool )
   {: c:IR-CTX:ctx :}
   c s" TWINW 2dup and nip nip dup cells swap invert xor 1+ 0=" 2 1 BUILT
   {: ma:IR-BUILD:module fa:IR-ID:ir-fun-id :}
   c s" TWINW 2DUP AND NIP NIP DUP CELLS SWAP INVERT XOR 1+ 0=" 2 1 BUILT
   {: mb:IR-BUILD:module fb:IR-ID:ir-fun-id :}
   ma fa mb fb TW-FUN? ;

\ The counted loop and its index.
: LOOP-TWIN-BODY ( IR-CTX:ctx -- bool )
   {: c:IR-CTX:ctx :}
   c s" TWIND 0 swap 0 ?do i + loop" 1 1 BUILT
   {: ma:IR-BUILD:module fa:IR-ID:ir-fun-id :}
   c s" TWIND 0 SWAP 0 ?DO I + LOOP" 1 1 BUILT
   {: mb:IR-BUILD:module fb:IR-ID:ir-fun-id :}
   ma fa mb fb TW-FUN? ;

\ `RECURSE`, which the table declares in CAPITALS, so this twin is the fold read
\ the other way round: the lower-case spelling is the one that used to refuse.
: SELF-TWIN-BODY ( IR-CTX:ctx -- bool )
   {: c:IR-CTX:ctx :}
   c s" TWINR dup 1 <= if drop 1 exit then dup 1- RECURSE *" 1 1 BUILT
   {: ma:IR-BUILD:module fa:IR-ID:ir-fun-id :}
   c s" TWINR dup 1 <= if drop 1 exit then dup 1- recurse *" 1 1 BUILT
   {: mb:IR-BUILD:module fb:IR-ID:ir-fun-id :}
   ma fa mb fb TW-FUN? ;

\ The comparator's own falsifier. Two bodies that differ by one word must compare
\ unequal, or every twin above says nothing.
: UNTWIN-BODY ( IR-CTX:ctx -- bool )
   {: c:IR-CTX:ctx :}
   c s" TWIN dup 0 > if 1+ else 1- then" 1 1 BUILT
   {: ma:IR-BUILD:module fa:IR-ID:ir-fun-id :}
   c s" TWIN dup 0 > if 1- else 1+ then" 1 1 BUILT
   {: mb:IR-BUILD:module fb:IR-ID:ir-fun-id :}
   ma fa mb fb TW-FUN? ;

\ The two mentions of one locals name, which the engine keeps apart and so does
\ this. `I` is the local the group bound; `i` is the loop's index. Both bodies
\ compile, and they are two different programs.
: LOCAL-CASE-BODY ( IR-CTX:ctx -- bool )
   {: c:IR-CTX:ctx :}
   c s" LOCC {: I:n :} 0 3 0 ?do I + loop" 1 1 BUILT
   {: ma:IR-BUILD:module fa:IR-ID:ir-fun-id :}
   c s" LOCC {: I:n :} 0 3 0 ?do i + loop" 1 1 BUILT
   {: mb:IR-BUILD:module fb:IR-ID:ir-fun-id :}
   ma fa mb fb TW-FUN? ;

: CTRL-TWIN-CASE ( -- )
   s" a body that writes the control words in capitals compiles to its lower-case twin's module" T-LABEL
   BND [: CTRL-TWIN-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE ;

: WORD-TWIN-CASE ( -- )
   s" and so does one that writes the renames and the operations in capitals" T-LABEL
   BND [: WORD-TWIN-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE ;

: LOOP-TWIN-CASE ( -- )
   s" and one that writes the counted loop and its index in capitals" T-LABEL
   BND [: LOOP-TWIN-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE ;

: SELF-TWIN-CASE ( -- )
   s" and one that writes the self-call in the case the table did not declare it in" T-LABEL
   BND [: SELF-TWIN-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE ;

: UNTWIN-CASE ( -- )
   s" two bodies that are not the same program do not compare equal" T-LABEL
   BND [: UNTWIN-BODY ;] IR-CTX:WITH-CONTEXT
   TFALSE ;

: LOCAL-CASE-CASE ( -- )
   s" a locals name is matched by its own bytes, so the other case of it is the dialect's word" T-LABEL
   BND [: LOCAL-CASE-BODY ;] IR-CTX:WITH-CONTEXT
   TFALSE ;

\ ---- and what capitals do NOT do ---------------------------------------------
\ The fold is the key of a word this table declared, and nothing else. A word the
\ dialect never modelled is refused in capitals exactly as in lower case, and the
\ record names the bytes the body wrote rather than the key they were looked up
\ under.
: CAPS-UNMOD-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   s" CAPSX dup ZZQX" TEXT!
   c SEALED
   {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena v:IR-ARENA:view :}
   c b v p r 1 1 NELAB:COLON drop ;

: CAPS-UNMOD ( -- )
   BND [: CAPS-UNMOD-BODY ;] IR-CTX:WITH-CONTEXT ;

\ A near miss on both sides: `IFF` folds to `iff`, which is not `if`, so a key is
\ the whole spelling folded and not a prefix of one.
: CAPS-NEAR-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   s" CAPSN dup IFF" TEXT!
   c SEALED
   {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena v:IR-ARENA:view :}
   c b v p r 1 1 NELAB:COLON drop ;

: CAPS-NEAR ( -- )
   BND [: CAPS-NEAR-BODY ;] IR-CTX:WITH-CONTEXT ;

\ The byte the fold must not touch. `[` is $5B and `{` is $7B, one bit apart, so a
\ fold written as "set $20 on every byte" turns the quotation opener `[:` into
\ the locals opener `{:` - and this body would then be read as a locals group
\ rather than refused for the word it really writes.
: CAPS-QUOT-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   s" CAPSQ dup [:" TEXT!
   c SEALED
   {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena v:IR-ARENA:view :}
   c b v p r 1 1 NELAB:COLON drop ;

: CAPS-QUOT ( -- )
   BND [: CAPS-QUOT-BODY ;] IR-CTX:WITH-CONTEXT ;

\ The section's decision held in the other direction. A name is the local's when
\ the mention writes the same BYTES, whichever case those bytes are in - so a
\ group declaring `i` and a body writing `i` is the same program as a group
\ declaring `I` and a body writing `I`, and both are the local rather than the
\ loop index. The pair above says the two spellings are not each other; this
\ says neither of them is the dialect's word.
: LOCAL-LOWER-BODY ( IR-CTX:ctx -- bool )
   {: c:IR-CTX:ctx :}
   c s" LOCI {: i:n :} 0 3 0 ?do i + loop" 1 1 BUILT
   {: ma:IR-BUILD:module fa:IR-ID:ir-fun-id :}
   c s" LOCI {: I:n :} 0 3 0 ?do I + loop" 1 1 BUILT
   {: mb:IR-BUILD:module fb:IR-ID:ir-fun-id :}
   ma fa mb fb TW-FUN? ;

: CAPS-REFUSE-CASE ( -- )
   s" a word the dialect never modelled is refused in capitals too, and named by its own spelling" T-LABEL
   [: CAPS-UNMOD ;] E-HIR-UNMODELED TTHROWSQ
   NELAB:REFUSED$ s" ZZQX" T$= ;

: CAPS-NEAR-CASE ( -- )
   s" a capitalised near miss on a modelled word is refused, and named by its own spelling" T-LABEL
   [: CAPS-NEAR ;] E-HIR-UNMODELED TTHROWSQ
   NELAB:REFUSED$ s" IFF" T$= ;

\ ---- which token a malformed quotation is named by ----------------------------
\ A quotation is two tokens and what stands between them is another function's
\ body. A nested opener is decided BEFORE the walks run, because a walk meets
\ tokens one at a time and would name the outer opener for a fault at the inner
\ one; every other malformed pair is met by a walk standing on the token at fault.
\ The first fixture is written as a TAPE rather than as source the engine
\ compiled, which is the only way it can exist at all - the engine ends the
\ process at a nested opener and never produces a tape for one - and the lexer
\ behind TEXT! has no such opinion. The second is here beside it because the two
\ together say which token each refusal names, which is the whole point.
: QUOT-NESTED-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   s" QNEST dup [: [: 1+ ;] ;]" TEXT!
   c SEALED
   {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena v:IR-ARENA:view :}
   c b v p r 1 1 NELAB:COLON drop ;

: QUOT-NESTED ( -- )
   BND [: QUOT-NESTED-BODY ;] IR-CTX:WITH-CONTEXT ;

: QUOT-ORPHAN-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   s" QORPH dup ;]" TEXT!
   c SEALED
   {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena v:IR-ARENA:view :}
   c b v p r 1 1 NELAB:COLON drop ;

: QUOT-ORPHAN ( -- )
   BND [: QUOT-ORPHAN-BODY ;] IR-CTX:WITH-CONTEXT ;

\ TWO QUOTATIONS ONE AFTER THE OTHER, which is what says the closer really closes.
\ A pass that opened on `[:` and never cleared would read the second opener as a
\ nested one and refuse THERE; the pair is well formed, so the pre-scan has
\ nothing to say about it and the walk declines the first opener it meets. The row
\ is what separates the two answers - both are the same code about a token spelled
\ the same way.
: QUOT-TWO-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   s" QTWO dup [: 1+ ;] drop [: 1- ;] drop" TEXT!
   c SEALED
   {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena v:IR-ARENA:view :}
   c b v p r 1 2 NELAB:COLON drop ;

: QUOT-TWO ( -- )
   BND [: QUOT-TWO-BODY ;] IR-CTX:WITH-CONTEXT ;

: QUOT-PAIR-CASES ( -- )
   s" a quotation opened inside another is refused, at the INNER opener" T-LABEL
   [: QUOT-NESTED ;] E-NELAB-QUOT TTHROWSQ
   NELAB:REFUSED$ s" [:" T$=
   \ Both openers are spelled `[:`, so the spelling above cannot tell them apart
   \ and the ROW is what says which one. The tape is QNEST dup [: [: 1+ ;] ;] -
   \ token 2 is the outer opener and token 3 the inner - and a walk meeting these
   \ tokens one at a time answers 2.
   NELAB:REFUSED-ROW 3 T=
   s" a quotation closer with nothing open is refused at the closer itself" T-LABEL
   [: QUOT-ORPHAN ;] E-NELAB-QUOT TTHROWSQ
   NELAB:REFUSED$ s" ;]" T$=
   \ TWO QUOTATIONS ONE AFTER THE OTHER are two pairs, which is what says the
   \ closer really closes: a pre-scan that opened on `[:` and never cleared would
   \ read the second opener as a nested one and refuse THERE. Both pairs are well
   \ formed, so what refuses the body is that nothing consumes either of them,
   \ and it names the FIRST of the two - the row is the only thing that can say
   \ which, because both openers are spelled the same way.
   s" two quotations in a row are two pairs, and the first opener is named" T-LABEL
   [: QUOT-TWO ;] E-NELAB-QUOT TTHROWSQ
   NELAB:REFUSED-ROW 2 T= ;

\ ---- a quotation body becomes a function of the emission -----------------------
\ WHAT THE FIXTURES HAVE TO SAY, AND WHY READING THE MODULE IS THE ONLY WAY TO
\ SAY IT. A quotation compiles into a second function whose arity is the one its
\ CONSUMER declared, and the enclosing body holds one value naming it by ordinal.
\ Nothing about that is visible from "the elaboration returned": a leaf that
\ built one function and dropped the body, or built the body under the enclosing
\ word's arity, or named the wrong ordinal, all return exactly the same way. So
\ every case below reads the module back - how many functions it holds, what each
\ one's recorded signature says, and what the `hir.quot` in the first one names.
\
\ THE SUBJECT WORDS ARE DEFINED THROUGH THE ENGINE FIRST, because the arity comes
\ from the CHECKER's accepted effect for the name and there is no other place it
\ could come from. The tape is then lexed from the same body, so what is
\ elaborated is the definition the checker certified.
: QDEF ( ptr u8 n ptr u8 n -- )
   {: na nu:n sa su:n :} \ typed-local-lint: allow-bare-local - na and sa keep the ptr u8 byte-span role
   na nu 0 search-wl 0<> if exit then
   sa su EV ;

: QP-ACT! ( -- )
   s" QP-ACT" s" : QP-ACT ( -- [ -- ] ) [: 1 drop ;] ;" QDEF ;

: QP-TAKE! ( -- )
   s" QP-TAKE" s" : QP-TAKE ( [ n -- n ] n -- n ) swap drop ;" QDEF ;

: QP-USE! ( -- )
   s" QP-USE" s" : QP-USE ( n -- n ) [: 1 + ;] swap QP-TAKE ;" QDEF ;

: QP-TAKE2! ( -- )
   s" QP-TAKE2" s" : QP-TAKE2 ( [ n n -- n ] n -- n ) swap drop ;" QDEF ;

: QP-RET! ( -- )
   s" QP-RET" s" : QP-RET ( [ n -- n | a -- a ] n -- n ) swap drop ;" QDEF ;

: QP-THREE! ( -- )
   s" QP-THREE"
   s" : QP-THREE ( -- n [ n n -- n ] [ n n n -- n ] ) 0 [: drop ;] [: drop drop ;] ;"
   QDEF ;

: F-FUNS ( IR-BUILD:module -- n )
   IR-BUILD:FFUN-ROWS IR-FUN:FFUNS ;

: F-ARITY ( IR-BUILD:module n -- n n )
   {: m:IR-BUILD:module k:n :}
   m IR-BUILD:FTYPE-ROWS
   m IR-BUILD:FFUN-ROWS m IR-BUILD:FKEY  m IR-BUILD:FKEY k IR-ID:PACK-FUN
   IR-FUN:FSIGNATURE@  IR-TYPE:FARITY@ ;

\ The name a function of the module carries. A quotation body is a function like
\ any other and the table is keyed by SYMBOL, so two bodies of one definition
\ that were named the same thing would be one function - which is why the name is
\ read back rather than described.
: F-FUN-NAME? ( IR-BUILD:module n ptr u8 n -- bool )
   {: m:IR-BUILD:module k:n a u:n :} \ typed-local-lint: allow-bare-local - a keeps the ptr u8 byte-span role
   m IR-BUILD:FSYM-POOL m IR-BUILD:FSYM-ROWS
   m IR-BUILD:FFUN-ROWS m IR-BUILD:FKEY  m IR-BUILD:FKEY k IR-ID:PACK-FUN
   IR-FUN:FSYMBOL@
   a u IR-SYM:FEQ? ;

: F-QUOT-FUN ( IR-BUILD:module n -- n )
   {: m:IR-BUILD:module k:n :}
   m  m  m IR-BUILD:FKEY 0 IR-ID:PACK-FUN  F-BLK  s" hir.quot" k F-OPC-AT {: op:IR-ID:ir-op-id :}
   m op 0 F-ATTR ;

: F-QUOTS ( IR-BUILD:module -- n )
   {: m:IR-BUILD:module :}
   m  m  m IR-BUILD:FKEY 0 IR-ID:PACK-FUN  F-BLK  s" hir.quot" F-OPC-N ;

\ A rig whose word model has room for the callees a fixture's body names.
: SEALED-ROOM ( IR-CTX:ctx n -- IR-BUILD:builder IR-ARENA:arena IR-ARENA:arena IR-ARENA:view )
   {: c:IR-CTX:ctx extra:n :}
   c HIR-BUILDER {: b:IR-BUILD:builder :}
   c b extra MODEL-ROOM
   {: p:IR-ARENA:arena r:IR-ARENA:arena :}
   c b TAPE {: tp:IR-ARENA:arena :}
   c LEX
   b p r  tp NTAPE:SEAL ;

\ `: QP-ACT ( -- [ -- ] ) [: 1 drop ;] ;` - the returned quotation. The enclosing
\ function leaves one value and the body leaves none, which is the pair a module
\ with one arity for the whole emission cannot hold.
: QUOT-ACT-BODY ( IR-CTX:ctx -- n n n n n n n )
   {: c:IR-CTX:ctx :}
   QP-ACT!
   s" QP-ACT [: 1 drop ;]" TEXT!
   c SEALED
   {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena v:IR-ARENA:view :}
   c b v p r 0 1 NELAB:COLON drop
   c b IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   m F-FUNS
   m F-QUOTS
   m 0 F-QUOT-FUN
   m 0 F-ARITY
   m 1 F-ARITY ;

: QUOT-ACT ( -- n n n n n n n )
   BND [: QUOT-ACT-BODY ;] IR-CTX:WITH-CONTEXT ;

\ A quotation handed to a CALLEE, whose declared operand says what the body is.
\ `QP-TAKE` takes `[ n -- n ]` and a number, so the body is ( n -- n ) while the
\ enclosing definition is ( n -- n ) too - which is why the case reads the
\ ATTRIBUTE and the operand's own descent rather than the two arities alone.
: QUOT-CALL-BODY ( IR-CTX:ctx -- n n n n )
   {: c:IR-CTX:ctx :}
   QP-TAKE! QP-USE!
   s" QP-USE [: 1 + ;] swap QP-TAKE" TEXT!
   c 1 SEALED-ROOM
   {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena v:IR-ARENA:view :}
   c b v p r 1 1 NELAB:COLON drop
   c b IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   m F-FUNS
   m 0 F-QUOT-FUN
   m 1 F-ARITY ;

: QUOT-CALL ( -- n n n n )
   BND [: QUOT-CALL-BODY ;] IR-CTX:WITH-CONTEXT ;

\ THREE BODIES IN ONE DEFINITION, which is what says the ordinals are the build
\ ORDER and not a constant. Each one's arity is its own dout term's, and the
\ three attributes have to be one, two and three in the order the source wrote
\ them - a loop that built them backwards, or that numbered from the wrong base,
\ answers a different triple here rather than failing to return.
: QUOT-THREE-BODY ( IR-CTX:ctx -- bool bool n n n n n n n n n n )
   {: c:IR-CTX:ctx :}
   QP-THREE!
   s" QP-THREE 0 [: drop ;] [: drop drop ;]" TEXT!
   c SEALED
   {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena v:IR-ARENA:view :}
   c b v p r 0 3 NELAB:COLON drop
   c b IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   m 1 s" QP-THREE[:0" F-FUN-NAME?
   m 2 s" QP-THREE[:1" F-FUN-NAME?
   m F-FUNS
   m F-QUOTS
   m 0 F-QUOT-FUN
   m 1 F-QUOT-FUN
   m 1 F-ARITY
   m 2 F-ARITY
   m 0 F-ARITY ;

: QUOT-THREE ( -- bool bool n n n n n n n n n n )
   BND [: QUOT-THREE-BODY ;] IR-CTX:WITH-CONTEXT ;

\ TWO CONSUMERS REACHING ONE BODY, WITH A MADE CALL BETWEEN THEM. This is the
\ case that says which entry of the compile-time vector a body's row rides on.
\ A made call hands the WHOLE vector over as operands and takes it back as the
\ operation's results, so every value that merely survives it comes back a
\ different value - while a call the inliner copied leaves them alone. A row
\ carried on the VALUE would therefore be known after one call and lost after the
\ other, and this definition would compile or not depending on whether an
\ optimisation fired. It is carried on the vector ENTRY, which a call puts back
\ where it found it, so the second `QP-TAKE` still names body zero and states the
\ same arity the first one did.
: QUOT-TWICE-BODY ( IR-CTX:ctx -- n n n n n )
   {: c:IR-CTX:ctx :}
   QP-TAKE!
   s" QTWICE [: 1 + ;] dup 2 QP-TAKE drop 3 QP-TAKE" TEXT!
   c 1 SEALED-ROOM
   {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena v:IR-ARENA:view :}
   c b v p r 0 1 NELAB:COLON drop
   c b IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   m F-FUNS
   m F-QUOTS
   m 0 F-QUOT-FUN
   m 1 F-ARITY ;

: QUOT-TWICE ( -- n n n n n )
   BND [: QUOT-TWICE-BODY ;] IR-CTX:WITH-CONTEXT ;

\ AND THE SAME BODY REACHING TWO CONSUMERS THAT DISAGREE. No certified definition
\ can hold this - the checker refuses a second consumer declaring a different
\ quotation effect, measured on `[: 1 + ;] dup 2 QA swap 3 QB` - so the tape is
\ written straight and the elaborator meets a disagreement the checker would
\ never let through. It refuses at the body's own `[:` rather than compiling the
\ routine under whichever consumer the walk reached first.
: QUOT-DISAGREE-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   QP-TAKE! QP-TAKE2!
   s" QDIS [: 1 + ;] dup 2 QP-TAKE drop 3 QP-TAKE2" TEXT!
   c 2 SEALED-ROOM
   {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena v:IR-ARENA:view :}
   c b v p r 0 1 NELAB:COLON drop ;

: QUOT-DISAGREE ( -- )
   BND [: QUOT-DISAGREE-BODY ;] IR-CTX:WITH-CONTEXT ;

\ A BODY THAT DOES NOT LEAVE WHAT ITS CONSUMER SAID IT WOULD. `QP-TAKE` declares
\ the quotation it takes to be ( n -- n ), and `dup` leaves two where one was
\ promised. No certified definition can hold this either - the checker infers the
\ body's own effect and holds it against the operand - so the tape is written
\ straight, and the body's return refuses at the `[:` rather than staging a
\ return of the wrong width into a function whose signature already says one.
: QUOT-DEEP-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   QP-TAKE!
   s" QDEEP [: dup ;] 2 QP-TAKE" TEXT!
   c 1 SEALED-ROOM
   {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena v:IR-ARENA:view :}
   c b v p r 0 1 NELAB:COLON drop ;

: QUOT-DEEP ( -- )
   BND [: QUOT-DEEP-BODY ;] IR-CTX:WITH-CONTEXT ;

\ A BODY THAT IS NOT AN ORDINARY ROUTINE. `QP-RET` declares the quotation it
\ takes as ( n -- n ) on the data stack AND ( a -- a ) on the RETURN stack, which
\ is a body a caller cannot reach with a branch and come back from: what it does
\ to the return stack is not what a call's own return expects to find there. The
\ checker owns that three-clause question - neutral return rows, no throw edge, a
\ live fall-through - and src/compiler/native/dict.f asks it rather than
\ re-deriving it, so the descent answers "no quotation there" and this refuses.
: QUOT-RSTACK-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   QP-RET!
   s" QRET [: 1 + ;] 2 QP-RET" TEXT!
   c 1 SEALED-ROOM
   {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena v:IR-ARENA:view :}
   c b v p r 0 1 NELAB:COLON drop ;

: QUOT-RSTACK ( -- )
   BND [: QUOT-RSTACK-BODY ;] IR-CTX:WITH-CONTEXT ;

\ ---- the three ways a body never gets an arity -------------------------------
\ NOTHING CONSUMES IT. The value is dropped, so no term ever says what the body
\ takes and leaves, and there is no function to build. It is named by its own
\ `[:`, which is the token a reader can act on.
: QUOT-NONE-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   s" QNONE [: 1 drop ;] drop" TEXT!
   c SEALED
   {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena v:IR-ARENA:view :}
   c b v p r 0 0 NELAB:COLON drop ;

: QUOT-NONE ( -- )
   BND [: QUOT-NONE-BODY ;] IR-CTX:WITH-CONTEXT ;

\ IT CROSSES A BRANCH. A block edge pushes fresh arguments, so the entry the
\ consumer holds on the far side names no body and the row is never told. That is
\ the fail-closed direction and the honest one: a body reached through two arms
\ could be told two arities, and which one won would be the walk's order.
: QUOT-JOIN-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   QP-TAKE!
   s" QJOIN [: 1 + ;] swap if then 2 QP-TAKE" TEXT!
   c 1 SEALED-ROOM
   {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena v:IR-ARENA:view :}
   c b v p r 1 1 NELAB:COLON drop ;

: QUOT-JOIN ( -- )
   BND [: QUOT-JOIN-BODY ;] IR-CTX:WITH-CONTEXT ;

\ A `{: … :}` group inside a body, refused at the group's own closer. The group
\ machinery binds with a cursor the enclosing walk advances, and a walk that skips
\ a body advances none of the body's.
: QUOT-LOCALS-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   s" QLOC [: {: a:n :} a ;] drop" TEXT!
   c SEALED
   {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena v:IR-ARENA:view :}
   c b v p r 0 0 NELAB:COLON drop ;

: QUOT-LOCALS ( -- )
   BND [: QUOT-LOCALS-BODY ;] IR-CTX:WITH-CONTEXT ;

\ `[:` IN THE WRONG ROLE OPENS NOTHING, and the fixture has to build the tape by
\ hand to say so. What separates a quotation opener from a string whose text is
\ `[:` is the token's KIND, not its spelling: both intern to the SAME symbol,
\ because interning is by content. So the tape below carries two string tokens
\ spelled exactly `[:` and `;]`, standing where a pair would, in front of a real
\ pair - and the module still holds two functions and one `hir.quot`.
\
\ IT IS THE WHOLE OF THE CLAIM THIS SUITE CAN MAKE. A `[:` inside a parenthesised
\ COMMENT never becomes a token at all, and that is the reader's fact rather than
\ the elaborator's: there is nothing to push onto a tape and nothing here to
\ assert about. test/compiler/native-quot.f states it where the real reader runs.
\
\ THE TEXT IS `QP-ACT [: ;] 2drop 2drop [: 1 drop ;]` and the two strings leave
\ two cells each, which the two `2drop`s take back, so the definition still
\ leaves exactly the one value its effect declares.
create QHID-TXT
   81 c,  80 c,  45 c,  65 c,  67 c,  84 c,  32 c,  91 c,
   58 c,  32 c,  59 c,  93 c,  32 c,  50 c, 100 c, 114 c,
  111 c, 112 c,  32 c,  50 c, 100 c, 114 c, 111 c, 112 c,
   32 c,  91 c,  58 c,  32 c,  49 c,  32 c, 100 c, 114 c,
  111 c, 112 c,  32 c,  59 c,  93 c,
37 constant QHID-N

: QUOT-HIDDEN-BODY ( IR-CTX:ctx -- n n )
   {: c:IR-CTX:ctx :}
   QP-ACT!
   QHID-TXT QHID-N TEXT!
   c RIG
   {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena tp:IR-ARENA:arena :}
   c 0 6 NTAPE-MODE:INTERPRETING NAME,
   c 7 2 NTAPE-MODE:COMPILING STR,
   c 10 2 NTAPE-MODE:COMPILING STR,
   c 13 5 NTAPE-MODE:COMPILING NAME,
   c 19 5 NTAPE-MODE:COMPILING NAME,
   c 25 2 NTAPE-MODE:COMPILING NAME,
   c 28 1 NTAPE-MODE:COMPILING 1 INT,
   c 30 4 NTAPE-MODE:COMPILING NAME,
   c 35 2 NTAPE-MODE:COMPILING NAME,
   c b  tp NTAPE:SEAL  p r 0 1 NELAB:COLON drop
   c b IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   m F-FUNS
   m F-QUOTS ;

: QUOT-HIDDEN ( -- n n )
   BND [: QUOT-HIDDEN-BODY ;] IR-CTX:WITH-CONTEXT ;

: QUOT-SHAPE-CASES ( -- )
   s" a returned quotation is a second function under the arity its term declares"
   T-LABEL
   QUOT-ACT
   \ Read bottom up: the body is ( -- ), the definition is ( -- n ), the one
   \ hir.quot names function one, there is exactly one of them, and the module
   \ holds two functions.
   0 T= 0 T=  1 T= 0 T=  1 T=  1 T=  2 T=
   s" a quotation handed to a callee is the arity that callee's OPERAND declares"
   T-LABEL
   QUOT-CALL
   1 T= 1 T=  1 T=  2 T=
   s" three bodies are three functions, numbered in the order the source wrote them"
   T-LABEL
   QUOT-THREE
   \ Bottom up: three functions; two hir.quot; the first names function one and
   \ the second function two; function one is ( n n -- n ) and function two is
   \ ( n n n -- n ), each its own dout term's; and the definition is ( -- n n n ).
   3 T= 0 T=  1 T= 3 T=  1 T= 2 T=  2 T= 1 T=  2 T= 3 T=
   \ And each body carries a name of its own, built from the definition's name
   \ and the body's row: two bodies named the same thing would be ONE function.
   TTRUE TTRUE
   s" a `[:` in the wrong role opens nothing" T-LABEL
   QUOT-HIDDEN
   1 T= 2 T=
   s" a body two consumers reach is built ONCE, and a made call between them keeps its row"
   T-LABEL
   QUOT-TWICE
   \ Bottom up: two functions; one hir.quot; it names function one; and that
   \ function is ( n -- n ), which is what BOTH consumers declared.
   1 T= 1 T=  1 T=  1 T=  2 T= ;

\ ---- binding a quotation to a deferred word ------------------------------------
\ WHY THESE ARE TAPES AND NOT SOURCE. The engine's own `is` handler refuses a
\ target that is not a deferred word before the definition is ever certified -
\ measured in test/compiler/native-defer.f, where such a program dies with the
\ engine's code and the elaborator is never entered. So the elaborator's refusal
\ is a backstop that only a hand-built tape can reach, exactly like the nested
\ opener above, and it is made for the same reason: this pass reads a TAPE, and
\ a caller that builds one can present a shape the engine never would.
\
\ THE FOUR SHAPES ARE THE FOUR WAYS THE TARGET CAN BE WRONG: no token after the
\ keyword at all, a token that is not a name, a name that denotes a word which
\ is not deferred, and a name that denotes nothing. The third fixture's target
\ is a `create`d word whose first data cell holds the defer magic exactly, which
\ is the value a real defer's trailer starts with - so a reader that looked for
\ the magic near the record rather than at the record's own START+LEN would bind
\ into an ordinary data word.
: DEFER-TAPE ( IR-CTX:ctx ptr u8 n -- )
   {: c:IR-CTX:ctx a:ptr u:n :}
   a u TEXT!
   c SEALED
   {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena v:IR-ARENA:view :}
   c b v p r 0 0 NELAB:COLON drop ;

: DEFER-BARE-BODY ( IR-CTX:ctx -- )
   s" QBIND [: 1+ ;] is" DEFER-TAPE ;
: DEFER-BARE ( -- )
   BND [: DEFER-BARE-BODY ;] IR-CTX:WITH-CONTEXT ;

: DEFER-LIT-BODY ( IR-CTX:ctx -- )
   s" QBIND [: 1+ ;] is 42" DEFER-TAPE ;
: DEFER-LIT ( -- )
   BND [: DEFER-LIT-BODY ;] IR-CTX:WITH-CONTEXT ;

: DEFER-DATA-BODY ( IR-CTX:ctx -- )
   s" QBIND [: 1+ ;] is NELB-DATA" DEFER-TAPE ;
: DEFER-DATA ( -- )
   BND [: DEFER-DATA-BODY ;] IR-CTX:WITH-CONTEXT ;

: DEFER-PLAIN-BODY ( IR-CTX:ctx -- )
   s" QBIND [: 1+ ;] is NELB-PLAIN" DEFER-TAPE ;
: DEFER-PLAIN ( -- )
   BND [: DEFER-PLAIN-BODY ;] IR-CTX:WITH-CONTEXT ;

: DEFER-ABSENT-BODY ( IR-CTX:ctx -- )
   s" QBIND [: 1+ ;] is NELB-NOWHERE" DEFER-TAPE ;
: DEFER-ABSENT ( -- )
   BND [: DEFER-ABSENT-BODY ;] IR-CTX:WITH-CONTEXT ;

\ And the shape that WORKS, on the same rig: a real deferred word. It is here
\ rather than only in test/compiler/native-defer.f because it is the one place
\ the operand role is observable on its own - the target is a name the dialect
\ does not model, and a walk that failed to pass over it would refuse it as an
\ unmodelled word instead of elaborating the body.
: DEFER-OK-BODY ( IR-CTX:ctx -- )
   s" QBIND [: 1+ ;] is NELB-HOOK" DEFER-TAPE ;
: DEFER-OK ( -- )
   BND [: DEFER-OK-BODY ;] IR-CTX:WITH-CONTEXT ;

: DEFER-CASES ( -- )
   s" `is` with no token after it is refused by name" T-LABEL
   [: DEFER-BARE ;] E-NELAB-DEFER TTHROWSQ
   s" a target that is not a name is refused by name" T-LABEL
   [: DEFER-LIT ;] E-NELAB-DEFER TTHROWSQ
   s" a created word whose data begins with the defer magic is refused by name"
   T-LABEL
   [: DEFER-DATA ;] E-NELAB-DEFER TTHROWSQ
   s" an ordinary colon word is refused by the same name" T-LABEL
   [: DEFER-PLAIN ;] E-NELAB-DEFER TTHROWSQ
   s" and so is a name that denotes nothing here" T-LABEL
   [: DEFER-ABSENT ;] E-NELAB-DEFER TTHROWSQ
   s" a real deferred word elaborates, target token and all" T-LABEL
   DEFER-OK ;

: QUOT-REFUSE-CASES ( -- )
   s" a quotation nothing consumes is refused at its own opener" T-LABEL
   [: QUOT-NONE ;] E-NELAB-QUOT TTHROWSQ
   NELAB:REFUSED-ROW 1 T=
   NELAB:REFUSED$ s" [:" T$=
   s" two consumers that DISAGREE about one body are refused at its opener" T-LABEL
   [: QUOT-DISAGREE ;] E-NELAB-QUOT TTHROWSQ
   NELAB:REFUSED-ROW 1 T=
   NELAB:REFUSED$ s" [:" T$=
   s" a quotation that crosses a branch is refused as one nothing consumed" T-LABEL
   [: QUOT-JOIN ;] E-NELAB-QUOT TTHROWSQ
   NELAB:REFUSED-ROW 1 T=
   s" a body whose declared effect is not an ordinary routine's is refused"
   T-LABEL
   [: QUOT-RSTACK ;] E-NELAB-QUOT TTHROWSQ
   NELAB:REFUSED-ROW 1 T=
   s" a body leaving more than its consumer declared is refused at its opener"
   T-LABEL
   [: QUOT-DEEP ;] E-NELAB-QUOT TTHROWSQ
   NELAB:REFUSED-ROW 1 T=
   s" a locals group inside a body is refused at the group's own closer" T-LABEL
   [: QUOT-LOCALS ;] E-NELAB-QUOT TTHROWSQ
   NELAB:REFUSED$ s" :}" T$= ;

: CAPS-QUOT-CASE ( -- )
   s" the quotation opener is a word the dialect knows and this leaf declines" T-LABEL
   [: CAPS-QUOT ;] E-NELAB-QUOT TTHROWSQ
   NELAB:REFUSED$ s" [:" T$= ;

: LOCAL-LOWER-CASE ( -- )
   s" a local named exactly as a word the dialect models is still that local" T-LABEL
   BND [: LOCAL-LOWER-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE ;

public

: RUN ( -- )
   T-RESET
   JOINR-CASE
   JOINC-CASE
   MAX2-CASE
   LITMEMO-CASE
   LITKIND-CASE
   LERP-CASE
   TWO-GROUPS-CASE
   PRIMLOC-CASE
   IDXLOC-CASE
   OFLOC-CASE
   ISLOC-CASE
   BND [: drop NESTED-GROUP-CASE ;] IR-CTX:WITH-CONTEXT
   BND [: drop ORPHAN-CLOSE-CASE ;] IR-CTX:WITH-CONTEXT
   BND [: drop LONE-CLOSE-CASE ;] IR-CTX:WITH-CONTEXT
   BND [: drop GROUP-CAP-CASE ;] IR-CTX:WITH-CONTEXT
   BND [: drop OPEN-GROUP-CASE ;] IR-CTX:WITH-CONTEXT
   BND [: drop QCLOSE-NAME-CASE ;] IR-CTX:WITH-CONTEXT
   BND [: drop QOPEN-NAME-CASE ;] IR-CTX:WITH-CONTEXT
   BND [: drop TWICE-CASE ;] IR-CTX:WITH-CONTEXT
   BND [: drop NESTED-CASE ;] IR-CTX:WITH-CONTEXT
   COUNTDOWN-CASE
   SUMTO-CASE
   SUMTO-DO-CASE
   FOREVER-CASE
   SUMLV-CASE
   WCOUNT-CASE
   PICK2-CASE
   TWOW-CASE
   BND [: drop ORPHAN-CASE ;] IR-CTX:WITH-CONTEXT
   BND [: drop CROSSED-CASE ;] IR-CTX:WITH-CONTEXT
   BND [: drop UNCLOSED-CASE ;] IR-CTX:WITH-CONTEXT
   BND [: drop LOPSIDED-CASE ;] IR-CTX:WITH-CONTEXT
   BND [: drop STRAY-INDEX-CASE ;] IR-CTX:WITH-CONTEXT
   BND [: drop DOUNDER-CASE ;] IR-CTX:WITH-CONTEXT
   BND [: drop DODBL-CASE ;] IR-CTX:WITH-CONTEXT
   BND [: drop QDODBL-CASE ;] IR-CTX:WITH-CONTEXT
   BND [: drop DOOPEN-CASE ;] IR-CTX:WITH-CONTEXT
   BND [: drop DOUNTIL-CASE ;] IR-CTX:WITH-CONTEXT
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
   BND [: drop CHARTOK-CASE ;] IR-CTX:WITH-CONTEXT
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
   BND [: drop REFUSED-WORD-CASE ;] IR-CTX:WITH-CONTEXT
   BND [: drop REFUSED-OTHER-CASE ;] IR-CTX:WITH-CONTEXT
   BND [: drop REFUSED-LATE-CASE ;] IR-CTX:WITH-CONTEXT
   BND [: drop REFUSED-KIND-CASE ;] IR-CTX:WITH-CONTEXT
   REFUSED-STALE-CASE
   REFUSED-CLEARED-CASE
   REFUSED-NONE-CASE
   REFUSED-RESET-CASE
   REFUSED-CEILING-CASE
   CTRL-TWIN-CASE
   WORD-TWIN-CASE
   LOOP-TWIN-CASE
   SELF-TWIN-CASE
   UNTWIN-CASE
   LOCAL-CASE-CASE
   BND [: drop CAPS-REFUSE-CASE ;] IR-CTX:WITH-CONTEXT
   BND [: drop CAPS-NEAR-CASE ;] IR-CTX:WITH-CONTEXT
   BND [: drop CAPS-QUOT-CASE ;] IR-CTX:WITH-CONTEXT
   QUOT-PAIR-CASES
   QUOT-SHAPE-CASES
   QUOT-REFUSE-CASES
   DEFER-CASES
   LOCAL-LOWER-CASE
   T-REPORT ;

;package

NELAB-TEST:RUN
