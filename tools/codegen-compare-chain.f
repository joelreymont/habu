\ codegen-compare-chain.f - driving the new code generator for the comparison.
\ One concern: turning one line of straight-line Habu source into a routine this
\ process can call.
\
\ This is the real chain and nothing else: the text is lexed onto a source tape
\ by test/compiler/native-source-fixture.f, the elaborator turns that tape into
\ HIR operations, and test/compiler/native-chain-fixture.f selects, allocates,
\ has the allocation accepted and emits. Every stage is the shipped compiler
\ file, run the way its own suite runs it. Nothing here models a compiler, and
\ nothing here catches: a stage that refuses a word the comparison said it could
\ compile is a bug in the comparison's own coverage claim, and it must surface as
\ that stage's refusal rather than as a missing row.
\
\ WHY THE SOURCE TEXT IS RETYPED HERE RATHER THAN READ OFF THE CORPUS. The
\ comparison's subjects are the checked words in tools/codegen-compare-corpus.f,
\ and their bodies carry stack comments, package prefixes and a `create` the tape
\ lexer does not model. What the new chain is given is therefore the same body
\ written as the subset spells it: the same words in the same order, with the
\ declared stack effect handed in as the two arities the elaborator still takes
\ as arguments (dot habu-bind-checker-env-ed4f9f87 moves those onto the frozen
\ checker environment). A reader can hold the two side by side and see they are
\ the same program, which is the strongest tie available until the tape is fed
\ from the engine's own lexer.
\
\ AND WHY IT IS RETYPED WITHOUT ITS FRAME. The line handed to the fixture is
\ `NAME body…`, not `: NAME body… ;`. That is not an abbreviation: it is the
\ shape a real tape has. The engine consumes the opening `:` and the closing `;`
\ before the checker's reader sees anything, so a produced tape carries no frame
\ row, and src/compiler/native/elaborate.f reads the name/body boundary off the
\ recorded parser mode instead. Writing the frame here would hand the elaborator
\ a tape no compilation can produce.
\
\ WHY THE RESULT REGISTER IS CHECKED BEFORE ANYTHING IS CALLED. It used to be
\ luck: the chain had no calling-convention binding, a returned value stayed in
\ whichever register computed it, and for these shapes that happened to be
\ register zero - which is where the C-ABI call reads a result from. Now the
\ routine contract DECLARES it: the arguments arrive in x0 upwards and the
\ result leaves in x0, the allocator pre-colours from that declaration and the
\ validator refuses an assignment that does not honour it. The check below is
\ therefore no longer a hope about where the value landed; it compares the
\ accepted assignment against the declaration the routines are compiled under,
\ so a chain that stopped honouring its own contract stops the harness here
\ instead of calling code whose result is somewhere the caller will not look.
\
\ ONE ROUTINE AT A TIME. The published routine goes into the free code slot, and
\ the next publication uses the same slot, so a routine is compiled, checked,
\ measured and finished with before the next one is compiled.

require lib/errors.f
require src/compiler/native/elaborate.f
require test/compiler/native-source-fixture.f
require test/compiler/native-chain-fixture.f
require test/compiler/native-run-fixture.f

package CODEGEN-CHAIN

private

variable FN                       \ where the routine being measured was published

public

\ Elaborate the source text now in the fixture's text buffer as a definition that
\ takes `in` values and leaves `out`, then select, allocate, accept and emit it
\ for a leaf routine of `regs` registers.
: CHAIN ( IR-CTX:ctx n n n -- )
   {: c:IR-CTX:ctx in:n out:n regs:n :}
   c NSRC:HIR-BUILDER {: b:IR-BUILD:builder :}
   c b NSRC:MODEL {: p:IR-ARENA:arena r:IR-ARENA:arena :}
   c b NSRC:TAPE {: tp:IR-ARENA:arena :}
   c NSRC:LEX
   tp NTAPE:SEAL {: v:IR-ARENA:view :}
   c b v p r in out NELAB:COLON drop
   c b NSRC:TEXT$ 0 regs in out NFIX:RUN-ABI ;

\ How many bytes of machine code the chain emitted for it.
: BYTES ( -- n )
   A64EMIT:SIZE ;

\ The returned value has to be in the register the routine's own contract
\ declares its result leaves in, which is the register the C-ABI call reads one
\ from. Called by every shape that returns one, before it is ever called.
: RESULT-CK ( -- )
   NFIX:RESULT-REG NFIX:ABI-RESULT <> if E-CODEGEN-COMPARE-REG throw then ;

\ Store the emission into code space and keep its entry address for the timing
\ and correctness bodies to call.
: PUBLISH! ( -- )
   NRUN:PUBLISH FN ! ;

: FN@ ( -- n )
   FN @ ;

;package
