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
\ THE ROUTINES ARE COMPILED UNDER THE CONVENTION A HABU WORD IS ENTERED THROUGH.
\ Argument i is taken out of data-stack slot i of the caller's stack and result j
\ is left in slot j (design section 7.6), which is what an ordinary word does, so
\ the emitted routine is entered by the same branch the interpreter uses and the
\ two columns of the comparison are called the same way. It used to be the C ABI
\ and a foreign-call trampoline, and the trampoline cost two orders of magnitude
\ more than the routine it entered, which is what made the nanosecond half of the
\ comparison undecidable (dot habu-measure-the-new-dbaf82dc).
\
\ WHAT REPLACED THE RESULT-REGISTER CHECK. Under the C ABI the harness asserted
\ that the returned value was in the register the call reads one from. There is
\ no such register now: a result is a store into a slot, the validator re-derives
\ every one of those stores against the contract's declared places before the
\ allocation is accepted (A64RAV's DSTACK-CK), and the harness's own head-to-head
\ check compares what the routine really computed against what the old emitter's
\ word computes on the same pinned inputs. A store to the wrong slot fails that
\ comparison, so the guard is execution rather than an assertion about a register.
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
\ under the data-stack convention, with `regs` scratch registers.
: CHAIN ( IR-CTX:ctx n n n -- )
   {: c:IR-CTX:ctx in:n out:n regs:n :}
   c NSRC:HIR-BUILDER {: b:IR-BUILD:builder :}
   c b NSRC:MODEL {: p:IR-ARENA:arena r:IR-ARENA:arena :}
   c b NSRC:TAPE {: tp:IR-ARENA:arena :}
   c NSRC:LEX
   tp NTAPE:SEAL {: v:IR-ARENA:view :}
   c b v p r in out NELAB:COLON drop
   c b NSRC:TEXT$ 0 regs in out NFIX:RUN-HABU ;

\ How many bytes of machine code the chain emitted for it.
: BYTES ( -- n )
   A64EMIT:SIZE ;

\ Store the emission into code space and keep its entry address for the timing
\ and correctness bodies to call.
: PUBLISH! ( -- )
   NRUN:PUBLISH FN ! ;

: FN@ ( -- n )
   FN @ ;

;package
