\ native-chain.f - SOURCE TEXT TO EXECUTED BYTES: the native chain's first
\ end-to-end run.
\
\ Every other suite in this family tests one leaf against fixtures it builds
\ itself. This one builds nothing. It hands a colon definition to `evaluate`,
\ lets the engine compile it the way it compiles every definition, and follows
\ the ONE artefact that comes back out of that compilation - the sealed source
\ tape the checker's own reader filled - through every remaining stage of the
\ native pipeline until the result is machine code, and then it runs that code
\ and compares its answer with what the interpreted word computes on the same
\ input.
\
\ THE CHAIN, IN THE ORDER IT RUNS:
\   evaluate                 the engine compiles `: NCH-SQ ( n -- n ) dup * ;`
\   engine check hook        the checker scans the reconstructed definition
\   NFEED unit               one row per token, in consumption order
\   NCERT result             the verdict, bound to the tape and to the bytes
\   NELAB:COLON              the tape becomes HIR operations
\   NFIX:RUN                 select, allocate, accept, emit
\   NRUN:PUBLISH + EXEC1     the words become a routine this process calls
\
\ NOTHING IS HAND-BUILT ANYWHERE IN IT. This suite has no lexer, pushes no tape
\ row and assembles no instruction - and, unlike every other caller of the shared
\ chain fixtures, it does not even state the source text as a line the fixture
\ lexer will re-read: the tape comes off the engine's own compilation. The only
\ inputs are the definition's source text and the two counts the elaborator is
\ still told rather than reads (dot habu-bind-checker-env-ed4f9f87); everything
\ else is produced by the stage before it and checked by the stage after it. That
\ is what makes this case the chain's acceptance rather than another leaf test:
\ it can only pass if every leaf agrees with its neighbours about the same
\ definition.
\
\ THE BACK HALF IS THE SHARED FIXTURE. Selection, allocation, validation and
\ emission are driven through test/compiler/native-chain-fixture.f and published
\ through test/compiler/native-run-fixture.f, the same two files the emission
\ suite and the code generator comparison drive them through. So the one thing
\ this suite contributes is the FRONT half - a tape nobody typed - and the rest
\ of the chain is entered exactly as its own suites enter it.
\
\ WHY THE ANSWER IS COMPARED WITH THE INTERPRETED WORD. A table of expected
\ instruction words can only disagree with an emitter that CHANGED, never with
\ one that was always wrong. Here the compiled routine's answer is compared with
\ the answer the engine's own compilation of the same source gives, so the two
\ paths have to agree about the program.
\
\ ONE FIXTURE, ONE CONTEXT. This case builds a source module AND a machine
\ module, which is already most of the sixty-four live arena slots the registry
\ holds (see the note in test/compiler/native-emit.f), so it is the only case
\ that runs in its context.

require lib/test.f
require src/compiler/native/feed.f
require src/compiler/native/elaborate.f
require test/compiler/native-chain-fixture.f
require test/compiler/native-run-fixture.f

package NCHAIN-TEST
private

\ ---- the boundaries this suite needs -----------------------------------------
\ `evaluate` is the metaprogramming boundary the checker does not model and the
\ only way to put a definition through the real compile path from inside a test.
\ Publishing and calling the emitted bytes are NRUN's two trusted words, not this
\ file's.
TRUSTED: EV ( ptr u8 n -- ) evaluate ;
TRUSTED: EV-N ( ptr u8 n -- n ) evaluate ;

\ ---- what the run parks ------------------------------------------------------
\ A quotation cannot read the enclosing word's locals, and the whole run is one
\ word, so the handles that cross a stage boundary are parked rather than bound.
here CELL 1- and CELL swap - CELL 1- and allot
1 TYPED-BUFFER R-CTX IR-CTX:ctx
1 TYPED-BUFFER R-BLD IR-BUILD:builder
1 TYPED-BUFFER R-TAPE IR-ARENA:view
1 TYPED-BUFFER R-RES NCERT:result

: CC ( -- IR-CTX:ctx )           0 R-CTX @ ;
: BB ( -- IR-BUILD:builder )     0 R-BLD @ ;
: TAPE ( -- IR-ARENA:view )      0 R-TAPE @ ;
: RES ( -- NCERT:result )        0 R-RES @ ;

\ The buffer the recorded definition's text is kept in. The producer copies the
\ reader's text here as the scan opens; instruction selection reads it back to
\ carry the spans into the machine module, and refuses it unless it digests to
\ the source the HIR module was compiled from.
256 constant TEXT-CAP
create TXT TEXT-CAP allot

64 constant TAPE-CAP
4 constant REGS

\ ---- stage N0: the definition the engine compiles -----------------------------
\ The source is the whole input of this suite. Squaring is the shape the value
\ vector is for: `dup` is a compile-time rename, so a correct chain spends one
\ multiply and no stack traffic on it.
: SRC ( -- ptr u8 n )
   s" : NCH-SQ ( n -- n ) dup * ;" ;

: HIR-MOD ( IR-CTX:ctx -- IR-BUILD:builder )
   {: c:IR-CTX:ctx :}
   IR-BUILD:PLAN-BEGIN
   IR-BUILD:PLAN-DEFAULT
   c HIR:NEW-BUILDER {: b:IR-BUILD:builder :}
   c b HIR:REGISTER
   b ;

: MODEL ( IR-CTX:ctx IR-BUILD:builder -- IR-ARENA:arena IR-ARENA:arena )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b IR-BUILD:MODULE-KEY HIR-WORD:WORDS HIR-WORD:PICK-CELLS HIR-WORD:NEW
   {: p:IR-ARENA:arena r:IR-ARENA:arena :}
   c b p r HIR-WORD:REGISTER-WORDS
   p r ;

\ Compile the definition through the production path with a unit open, and park
\ the sealed tape and the source-bound result the unit publishes.
: RECORD ( -- )
   CC BB IR-BUILD:MODULE-KEY TAPE-CAP NTAPE:NEW {: tp:IR-ARENA:arena :}
   CC BB tp TXT TEXT-CAP NFEED:BEGIN-UNIT
   SRC EV
   NFEED:END-UNIT  0 R-RES !  0 R-TAPE ! ;

\ How many bytes the reader handed over, as the registry recorded them. It is
\ read off the LIVE builder, because the text has to be presented to instruction
\ selection and selection takes its binding before the module freezes.
: TEXT-LEN ( -- n )
   CC BB  RES NCERT:SOURCE  IR-BUILD:SOURCE-LEN ;

\ ---- stage N1 and the machine stages -----------------------------------------
\ Elaborate the produced tape into the module the tape was recorded into. The
\ freeze and everything after it belong to the shared chain fixture, so this word
\ stops at the point where the two halves meet.
\
\ THE ONE SEAM WHERE ARITY ENTERS. `1 1` is the word's declared effect, stated
\ here rather than read from the checker that just accepted it. The checker knows
\ - it parsed `( n -- n )` during this very scan - but it publishes an effect
\ only through a lookup by NAME into its live store, which answers about whoever
\ carries that name now and not about the definition this tape is. Binding the
\ accepted effect to the recorded unit is dot habu-bind-checker-env-ed4f9f87,
\ reached through habu-bind-the-colon-ea509e61; when it lands these two numbers
\ come off this line and nothing else here changes.
: ELABORATE ( IR-ARENA:arena IR-ARENA:arena -- )
   {: p:IR-ARENA:arena r:IR-ARENA:arena :}
   CC BB TAPE p r 1 1 NELAB:COLON drop ;

\ ---- the run -----------------------------------------------------------------
\ The text presented to selection is the one the unit kept, and selection refuses
\ it unless it digests to the source the HIR module carries - so a copy that
\ drifted stops the run here instead of moving spans onto other bytes. That
\ digest check is the production stage doing the binding NCERT:VERIFY states as a
\ value, which is why this case does not restate it: test/compiler/native-feed.f
\ owns the verification of the result itself.
: CHAIN-BODY ( IR-CTX:ctx -- n n n n n n n )
   {: c:IR-CTX:ctx :}
   c 0 R-CTX !
   c HIR-MOD 0 R-BLD !
   CC BB MODEL {: p:IR-ARENA:arena r:IR-ARENA:arena :}
   RECORD
   TAPE NTAPE:TOKENS
   RES NCERT:VERDICT
   p r ELABORATE
   CC BB TXT TEXT-LEN REGS NFIX:RUN
   A64EMIT:INSNS
   NFIX:RESULT-REG
   7 NRUN:PUBLISH NRUN:EXEC1
   s" 7 NCH-SQ" EV-N
   s" 11 NCH-SQ" EV-N ;

: CHAIN-CASE ( -- )
   s" a colon definition compiles from source text to executed bytes" T-LABEL
   NFIX:BINDING [: CHAIN-BODY ;] IR-CTX:WITH-CONTEXT
   121 T= 49 T= 49 T= 0 T= 2 T= -1 T= 3 T= ;

public

: RUN ( -- )
   T-RESET
   CHAIN-CASE
   T-REPORT ;

;package

NCHAIN-TEST:RUN
