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

\ `evaluate` is the metaprogramming boundary the checker does not model, and
\ every entry below is one call through it: the migration entry takes SOURCE.
TRUSTED: DEFINE ( ptr u8 n -- )
   NMIGRATE:DEFINE ;

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
   s" : DPC-JT ( n n -- n ) 0 = if drop E-A-EMPTY throw then ;" DEFINE ;

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
   s" : DPC-DE ( n n -- n ) 0 = if drop E-A-EMPTY throw else 1 + then ;" DEFINE ;

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
   s" : DPC-DS ( n n -- n ) 0 = if drop 5 else drop E-A-BOUNDS throw then ;" DEFINE ;

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
   s" : DPC-SHADOW ( n n -- n ) 0 = if drop 5 DPCX:throw 7 then ;" DEFINE ;

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
   s" : DPC-VIA ( n n -- n ) 0 = if drop E-A-EMPTY DPCY:BOOM then ;" DEFINE ;

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

\ ---- 5. the body EVERY path of which ends ------------------------------------
\ Such a body leaves through its trap and has no return convention at all: the
\ elaborator closes the block at each dead call, no return is staged and no block
\ is left open. It used to be refused by the allocation validator, because the
\ only routine forms the migration could choose all declared a frame this routine
\ has no epilogue to end - it reserved one and saved its link register, and the
\ memory order those two mint was passed on nowhere (E-A64RAV-ORDER).
\
\ THE FORM EXISTS NOW and this is the publication that refusal was pinned for.
\ src/compiler/native/abi.f NORET-FRAMED declares the direct-call trait - the
\ call is real and is what the routine dies in - declares the caller's return
\ address DESTROYED, and owns no frame: nothing restores an address that is never
\ read again, and no epilogue gives back a frame control never reaches.
\ src/compiler/native/migrate.f asks the checker whether THIS definition is one
\ that never returns, by the same certificate every other caller of it is
\ compiled against.
\
\ EACH CASE IS RUN, not only compiled. A routine published under a convention
\ nothing enters correctly would still compile; what says the convention is right
\ is that the caller gets the code the source names, out of a call the engine
\ made, with the data stack where the caller left it.
: MK-ALLDEAD ( -- )
   s" : DPC-ALLDEAD ( n -- ) drop E-A-EMPTY throw ;" DEFINE ;

: ALL-DEAD-CASE ( -- )
   s" a body whose every path ends compiles, publishes and throws" T-LABEL
   MK-ALLDEAD
   s" 5 ' DPC-ALLDEAD catch nip" EV-N E-A-EMPTY T= ;

\ Two arms, both dead. No edge reaches a join and no path states a width, so the
\ routine has no return anywhere rather than one nothing branches to - and the
\ arm the argument selects is the code the source named, which is what telling
\ the two apart proves.
: MK-BOTH-DEAD ( -- )
   s" : DPC-BOTHDEAD ( n -- ) 0 = if E-A-EMPTY throw else E-A-BOUNDS throw then ;"
   DEFINE ;

: BOTH-DEAD-CASE ( -- )
   s" two dead arms leave no return anywhere, and each arm is its own" T-LABEL
   MK-BOTH-DEAD
   s" 0 ' DPC-BOTHDEAD catch nip" EV-N E-A-EMPTY T=
   s" 1 ' DPC-BOTHDEAD catch nip" EV-N E-A-BOUNDS T= ;

\ THE SHAPE THAT READS LIKE A TAIL CALL. Its one call is the last thing the body
\ does and the callee takes and leaves exactly what this definition does, which
\ is the arithmetic the tail decision is about - and the callee is DEAD, so this
\ routine has no return for a tail branch to stand in place of. The block ended
\ at the call and its terminator is the trap, and a routine leaving through a
\ callee is a routine returning THROUGH somebody else.
\
\ THE CODE IS NOT THE ARGUMENT, so the case can tell the call from the caller.
\ DPCY:DEADN throws one MORE than what it was handed, which no path of the caller
\ computes: an answer equal to the argument would be a routine that returned it,
\ and an answer equal to the code says the callee really ran.
: MK-TAIL-SHAPED ( -- )
   s" : DPC-TAILDEAD ( n -- n ) DPCY:DEADN ;" DEFINE ;

: TAIL-SHAPED-CASE ( -- )
   s" a dead last call of this definition's own arity still never returns" T-LABEL
   MK-TAIL-SHAPED
   s" 4 ' DPC-TAILDEAD catch nip" EV-N 5 T= ;

\ ---- 6. what the whole chain says about that body, without keeping it ---------
\ The measured entry runs every stage the published one runs and keeps nothing,
\ so a shape that compiles only because something earlier in this file published
\ it would fail here. It is also the entry tools/chain-census.f asks with, which
\ is what makes the census's answer about these bodies this file's subject too.
: MEASURE ( ptr u8 n -- )
   NMIGRATE:MEASURE-HELD ;

: ALL-DEAD-MEASURED ( -- )
   s" : DPC-ALLDEADM ( n -- ) drop E-A-EMPTY throw ;" MEASURE ;

\ The same with a declared result. Control never comes back, so the cell the
\ signature names is never published - and the routine still declares it, because
\ the convention a Habu word is entered under is what its CALLERS were compiled
\ against and the module records the same arity.
: OUT-DEAD-MEASURED ( -- )
   s" : DPC-ALLDEADO ( n -- n ) E-A-EMPTY throw ;" MEASURE ;

: MEASURED-CASE ( -- )
   s" the whole chain accepts the shape with nothing published behind it" T-LABEL
   [: ALL-DEAD-MEASURED ;] 0 TTHROWSQ
   [: OUT-DEAD-MEASURED ;] 0 TTHROWSQ ;

\ ---- 7. the shape that is still refused, and what it is waiting for ----------
\ A no-return routine that SPILLS. Its frame is not the selector's - the walk
\ decides the count after selection - so src/compiler/native/spill.f writes the
\ reserve into the entry block and the release in front of the terminator of the
\ block control leaves through. There is no such block here, so the release is
\ never written, and the memory order the reserve minted is passed on nowhere:
\ E-A64RAV-ORDER, the same refusal every all-dead body used to earn, now narrowed
\ to the ones that run out of registers.
\
\ THE VALIDATOR ALREADY EXPECTS THE SHAPE - regalloc-verify.f VNO-RET-SPILL-CK
\ measures a routine that takes its frame and does not give it back - so what is
\ missing is one decision and not a design: either the trap terminator consumes
\ the frame order as it already consumes the data-stack order (an operand the
\ a64.trap form does not have), or the lowering writes the release in front of
\ every terminator that leaves, which is a frame given back that nothing needed
\ and a contradiction of the bracket rule above. Both belong to the owners of
\ src/compiler/native/spill.f and the machine dialect, not to this leaf.
\
\ IT IS PINNED AS THE REFUSAL IT IS, for the same reason section 5's refusal was
\ pinned before the form existed. No census body reaches it: nothing in the tree
\ spills a no-return body at the machine's own pool.
: MEASURE-AT ( ptr u8 n -- )
   NMIGRATE:MEASURE-HELD ;

\ Twenty-eight values every one of which is read after the last of them is
\ written, against the twenty-four registers NABI:SCRATCH leaves a routine, so
\ four of them reach the frame. The live twin below it is the same arithmetic
\ with the result returned instead of thrown away, and it compiles: what the case
\ measures is the frame, so the pressure has to be real on both sides of it - and
\ since no caller states a budget any more, real means bigger than the machine.
: SPILL-DEAD ( -- )
   s" : DPC-SPILLDEAD ( n -- ) {: s:n :} s 1+ s 2 + s 3 + s 4 + s 5 + s 6 + s 7 + s 8 + s 9 + s 10 + s 11 + s 12 + s 13 + s 14 + s 15 + s 16 + s 17 + s 18 + s 19 + s 20 + s 21 + s 22 + s 23 + s 24 + s 25 + s 26 + s 27 + s 28 + + + + + + + + + + + + + + + + + + + + + + + + + + + + drop E-A-EMPTY throw ;"
   MEASURE-AT ;

: SPILL-LIVE ( -- )
   s" : DPC-SPILLLIVE ( n -- n ) {: s:n :} s 1+ s 2 + s 3 + s 4 + s 5 + s 6 + s 7 + s 8 + s 9 + s 10 + s 11 + s 12 + s 13 + s 14 + s 15 + s 16 + s 17 + s 18 + s 19 + s 20 + s 21 + s 22 + s 23 + s 24 + s 25 + s 26 + s 27 + s 28 + + + + + + + + + + + + + + + + + + + + + + + + + + + + ;"
   MEASURE-AT ;

: SPILL-CASE ( -- )
   s" the same arithmetic spills and compiles when it returns" T-LABEL
   [: SPILL-LIVE ;] 0 TTHROWSQ
   NMIGRATE:SPILLS 0 T<>

   s" and is still refused for its frame when every path ends" T-LABEL
   [: SPILL-DEAD ;] E-A64RAV-ORDER TTHROWSQ ;

public

: RUN ( -- )
   JT-CASE
   JT-DEAD-CASE
   ELSE-CASE
   ELSE-DEAD-CASE
   SECOND-CASE
   SHADOW-CASE
   OWNDEAD-CASE
   ALL-DEAD-CASE
   BOTH-DEAD-CASE
   TAIL-SHAPED-CASE
   MEASURED-CASE
   SPILL-CASE ;

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

\ The same deadness at the arity a tail branch would need: what this leaves is
\ what it takes, so a caller of it whose own arity is the same reads as a routine
\ that could leave through it.
: DEADN ( n -- n )
   dup 1+ throw ;

;package

T-RESET
DEADPATH-CHAIN-TEST:RUN
T-REPORT
