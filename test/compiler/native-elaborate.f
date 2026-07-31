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
   s" BAD xor" TEXT!
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

public

: RUN ( -- )
   T-RESET
   SQUARE-CASE
   INC-CASE
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
