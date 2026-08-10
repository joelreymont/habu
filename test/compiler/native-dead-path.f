\ native-dead-path.f - a call control does not come back from, all the way
\ through the chain and into the running engine.
\
\     bin/hb --load test/compiler/native-dead-path.f
\
\ WHAT IS UNDER TEST. The checker certifies that a call to `throw`, to `die`, or
\ to a word whose own paths all end in one has no normal continuation
\ (src/core/checker.f, CTL-DEAD in the control-flag store). The chain reads that
\ fact off the same record it reads the callee's arity off
\ (src/compiler/native/dict.f SPELL-DEAD?, carried on the word-table row by
\ src/compiler/native/hir-word.f and read back as CALLEE-DEAD?), and
\ src/compiler/native/elaborate.f ends the block at such a call: the path neither
\ joins nor states a width, and the block's terminator is the `hir.trap` that
\ leaves without returning.
\
\ THE CALL IS AN ORDINARY CALL, AND THAT IS THE FIRST THING PROVED HERE. Trapping
\ INSTEAD of calling would turn a catchable throw into a process exit and change
\ what the program does, so the dead branch of every case below is executed and
\ the code it throws is compared with the code the source names. The trap sits
\ AFTER the call, where control never arrives.
\
\ NOTHING HERE IS A MODEL OF THE CHAIN. Each case hands source text to
\ NMIGRATE:DEFINE, which compiles it through every stage and publishes the
\ routine under its own name, and then CALLS that name. A case that only measured
\ would not notice a routine that compiled and computed the wrong thing, and a
\ case that built its own module would not notice the elaborator disagreeing with
\ the engine about the same definition.
\
\ THE HOSTILE FIXTURES ARE THE POINT OF SECTION 3. Deadness is a fact about the
\ WORD a token resolves to, so a package that defines its own `throw` must get
\ its own word's answer: the body there goes on after the call and joins like any
\ other. A chain that matched the spelling would end the block at that call and
\ refuse the tokens after it, so the case compiles only under the resolver.

require lib/prelude.f
require lib/errors.f
require lib/test.f
require src/compiler/native/migrate.f

package DEADPATH-CHAIN-TEST
private

18 constant REGS                     \ x0..x17, the whole general pool

\ `evaluate` is the metaprogramming boundary the checker does not model, and
\ every entry below is one call through it: the migration entry takes SOURCE.
TRUSTED: DEFINE ( ptr u8 n n n -- )
   REGS NMIGRATE:DEFINE ;

\ A published routine is called by NAME, and the name does not exist while this
\ file is being compiled - the migration mints it. So every call below is one
\ line of source handed to the same `evaluate` the definition arrived through,
\ which is also how test/compiler/native-chain.f calls what it published.
TRUSTED: EV-N ( ptr u8 n -- n )
   evaluate ;

\ ---- 1. the definition the dot was written about ------------------------------
\ `: JT ( n n -- n ) 0 = if drop E-A-EMPTY throw then ;` is the body the chain
\ refused with E-NELAB-JOIN before this landed: the checker accepts it because
\ the arm ends the path, and the chain would not because the arm left one value
\ fewer than the fall-through.
: MK-JT ( -- )
   s" : DPC-JT ( n n -- n ) 0 = if drop E-A-EMPTY throw then ;" 2 1 DEFINE ;

: JT-CASE ( -- )
   s" a one-armed if whose arm throws compiles, and its live path returns" T-LABEL
   MK-JT
   s" 1 2 DPC-JT" EV-N 1 T=
   s" 9 4 DPC-JT" EV-N 9 T= ;

: JT-DEAD-CASE ( -- )
   s" and its dead path throws the code the arm named, catchably" T-LABEL
   s" 7 0 ' DPC-JT catch nip nip" EV-N E-A-EMPTY T= ;

\ ---- 2. the same arm with an `else` beside it ---------------------------------
\ With two arms no edge into the join comes from the `if` itself, so the arm that
\ survives is the only one stating the join's width. The live answer is computed
\ from the FIRST argument, so an arm that had been confused for the other would
\ answer differently rather than not at all.
: MK-DE ( -- )
   s" : DPC-DE ( n n -- n ) 0 = if drop E-A-EMPTY throw else 1 + then ;" 2 1 DEFINE ;

: ELSE-CASE ( -- )
   s" a dead first arm leaves the second arm to state the join" T-LABEL
   MK-DE
   s" 4 9 DPC-DE" EV-N 5 T=
   s" -3 8 DPC-DE" EV-N -2 T= ;

: ELSE-DEAD-CASE ( -- )
   s" and the dead arm still throws" T-LABEL
   s" 3 0 ' DPC-DE catch nip nip" EV-N E-A-EMPTY T= ;

\ A dead SECOND arm, which is the other side of the same rule: the first arm
\ states the width and the second contributes no edge at all.
: MK-DS ( -- )
   s" : DPC-DS ( n n -- n ) 0 = if drop 5 else drop E-A-BOUNDS throw then ;" 2 1 DEFINE ;

: SECOND-CASE ( -- )
   s" a dead second arm joins the same way round" T-LABEL
   MK-DS
   s" 8 0 DPC-DS" EV-N 5 T=
   s" 8 1 ' DPC-DS catch nip nip" EV-N E-A-BOUNDS T= ;

\ ---- 3. deadness is the word's, not the spelling's ----------------------------
\ A package that defines its own `throw` binds it for every bare mention inside
\ it. It RETURNS, so the body below goes on after the call: the `7` after it is
\ ordinary code on a live path, and a chain that ended the block at the call
\ would refuse that token as code after a path that ended (E-NELAB-CTRL). The
\ case therefore compiles only if the chain resolved the word.
: MK-SHADOW ( -- )
   s" : DPC-SHADOW ( n n -- n ) 0 = if drop 5 DPCX:throw 7 then ;" 2 1 DEFINE ;

: SHADOW-CASE ( -- )
   s" a word whose name is throw but whose record is not dead still joins" T-LABEL
   s" DPCX:throw" NDICT:SPELL-DEAD? TFALSE
   s" throw" NDICT:SPELL-DEAD? TTRUE
   s" die" NDICT:SPELL-DEAD? TTRUE
   MK-SHADOW
   s" 3 0 DPC-SHADOW" EV-N 7 T=
   s" 3 9 DPC-SHADOW" EV-N 3 T= ;

\ And the other direction: a word that is dead by its OWN certified body, whose
\ name is nothing special. A chain reading a list of names would miss it.
: MK-OWNDEAD ( -- )
   s" : DPC-VIA ( n n -- n ) 0 = if drop E-A-EMPTY DPCY:BOOM then ;" 2 1 DEFINE ;

: OWNDEAD-CASE ( -- )
   s" a word the checker certified dead from its own body ends the path too" T-LABEL
   s" DPCY:BOOM" NDICT:SPELL-DEAD? TTRUE
   MK-OWNDEAD
   s" 6 5 DPC-VIA" EV-N 6 T=
   s" 6 0 ' DPC-VIA catch nip nip" EV-N E-A-EMPTY T= ;

\ ---- 4. what may follow a path that ended, and why nothing here tests it ------
\ The elaborator refuses any token after a path has ended except the closer of
\ the structure that path was an arm of. That rule cannot be reached from source:
\ the CHECKER refuses the same bodies first, and refuses them harder, so the
\ chain never sees them. Measured, both through the migration entry:
\
\   : DPC-AFTER ( n n -- n ) 0 = if drop E-A-EMPTY throw 7 then ;
\       habu: in dpc-after: at '7' after 'throw'
\   : DPC-LOOPD ( n -- n ) begin E-A-EMPTY throw repeat ;
\       hb: control-flow closer without opener: repeat
\
\ So the elaborator's check is a backstop against the two walks of the body
\ disagreeing, in the same class as the other backstops this file keeps, and a
\ case here would be a case about the checker wearing the chain's name.

\ ---- 5. the shape that is not compiled yet ------------------------------------
\ A body EVERY path of which ends leaves through its trap and has no return
\ convention at all. The elaborator builds it - no return is staged and no block
\ is left open - and it is refused further down, by the allocation validator,
\ because such a routine still RESERVES a frame and saves its link register and
\ nothing ever releases them: the order those two mint is passed on nowhere
\ (E-A64RAV-ORDER). What is missing is a routine form for a convention that never
\ returns; src/compiler/a64-effect.f models one (`control no-return`) and
\ src/compiler/native/abi.f mints no form for it, so every form the migration can
\ choose declares a frame this routine cannot end.
\
\ IT IS PINNED AS THE REFUSAL IT IS so that the day the form exists this case
\ fails and is rewritten as the publication it should be, rather than staying a
\ shape nobody measured.
: MEASURE ( ptr u8 n n n -- )
   REGS NMIGRATE:MEASURE-HELD ;

: ALL-DEAD ( -- )
   s" : DPC-ALLDEAD ( n -- ) drop E-A-EMPTY throw ;" 1 0 MEASURE ;

: ALL-DEAD-CASE ( -- )
   s" a body whose every path ends is built, and refused for its frame" T-LABEL
   [: ALL-DEAD ;] E-A64RAV-ORDER TTHROWSQ ;

public

: RUN ( -- )
   JT-CASE
   JT-DEAD-CASE
   ELSE-CASE
   ELSE-DEAD-CASE
   SECOND-CASE
   SHADOW-CASE
   OWNDEAD-CASE
   ALL-DEAD-CASE ;

;package

\ ---- the words the fixtures resolve against -----------------------------------
\ A package of its own so the shadowing `throw` is a real binding rather than a
\ redefinition of the primitive, and public so the fixtures can name it.
package DPCX
public

: throw ( n -- )
   drop ;

;package

\ BOOM is in a package of its OWN because a bare `throw` inside DPCX would bind
\ DPCX's word: what makes BOOM dead is that its body calls the primitive, and
\ the primitive is only what a bare `throw` names where nothing shadows it.
package DPCY
public

: BOOM ( n -- )
   throw ;

;package

T-RESET
DEADPATH-CHAIN-TEST:RUN
T-REPORT
