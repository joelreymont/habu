\ tier.f - the two compiler tiers, and the state that must not survive a
\ definition.
\
\ The engine carries two compilers. Tier 0 is the legacy JIT reached through
\ LCOMPILE; tier 1 is the IR pipeline reached through NCOMP-DISPATCH:XT-CELL.
\ `set-tier` ( n -- ) selects between them and `tier@` reads the selection back.
\ Tier 0 is the cold default, so every `--load` and the REPL run the JIT; the
\ paths that construct an executable select tier 1 before evaluating anything.
\
\ WHAT THIS SUITE CLAIMS, AND WHY EACH PART IS NOT ALREADY CLAIMED ELSEWHERE.
\
\   1. Both tiers compile and run a checked definition, and both REJECT a wrong
\      stack effect. A tier is a choice of compiler, not a choice of whether the
\      checker runs: a tier that compiled an unchecked body would be a hole in
\      the type system reachable with one token. The rejection is asserted on
\      both, with the same exit code, because that is the property that makes
\      the selection safe rather than the diagnostic text, which differs (tier 1
\      adds its own `ncomp:` line).
\
\   2. `set-tier` refuses everything but 0 and 1. A tier is not a truthy flag.
\      Folding 2 or -1 onto a tier would silently pick a compiler the caller did
\      not ask for, which is exactly the class of bug the cell exists to prevent.
\
\   3. Tier 0's BEGIN-snapshot depth DIES WITH THE DEFINITION. This is the
\      regression that pays for the file. jit.f pushes a snapshot frame at each
\      BEGIN and pops it at the back edge; a definition that FAILS between the
\      two - an undefined word in the body, say - never reaches its back edge,
\      so the depth stays raised. Nothing owned reset it, so 28 failed
\      definitions walked the depth to the nesting bound and the 29th `begin` in
\      the session exited 75 with nothing whatever wrong with it. The cell is
\      per-definition state and is now cleared with VSP/LVD/EXITH at both colon
\      entries and in EM-RESET-COMPILE-STATE, which is the shared tail of both
\      recovery paths. The test runs THIRTY failing definitions and then a good
\      one, because the bug needed 29 to show and a bound that moves would make a
\      smaller count pass for the wrong reason.
\
\   4. The reset does not reach INSIDE a definition. A quotation is compiled
\      inline in its enclosing definition, so a BEGIN in a quotation and a
\      quotation in a BEGIN share one frame stack and must still balance. If the
\      reset had been put at a quotation boundary instead of at the definition
\      boundary, cases 4b and 4c would pop a frame that is still live. They are
\      here to fail if that is ever "simplified".
\
\ Every case runs in a CHILD PROCESS. `set-tier` is engine-global state and the
\ leak case deliberately damages it, so a suite that ran these in-process would
\ be testing whichever order its cases happened to run in.

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/test/outcome.f
require lib/test/subject.f

package TIER-TEST

private

$1000 constant CAP
20000 constant TIMEOUT-MS
70 constant REJECT-RC              \ checker reject / undefined word, fail-closed
75 constant NEST-RC                \ jit.f EMIT-SNAP-NEST-CHECK: BEGIN past the bound

create OUT CAP allot
create ERR CAP allot
variable OUT-U  variable ERR-U
variable RC     variable EXITED

: OUT$ ( -- ptr u8 n )  OUT OUT-U @ ;
: ERR$ ( -- ptr u8 n )  ERR ERR-U @ ;

: STORE! ( len len outcome -- )
   MATCH outcome
     exited   OF RC ! 0 0= EXITED ! ENDOF
     signaled OF RC ! 0 0= 0= EXITED ! ENDOF
     timeout  OF 0 RC ! 0 0= 0= EXITED ! ENDOF
   ;MATCH
   LEN>N ERR-U !  LEN>N OUT-U ! ;

: RUN ( ptr u8 n -- ) {: src:ptr u:n :}
   src u OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS SUBJECT:RUN STORE! ;

\ ---- the two shapes every case asserts ---------------------------------------
\ OK means: the child exited 0 and its stdout contains what the definition
\ printed. Printing the answer is what proves the body RAN; a definition that
\ compiles and is never executed passes an exit-code-only check.
: ASSERT-OK ( ptr u8 n -- ) {: want:ptr wu:n :}
   EXITED @ TTRUE
   RC @ 0 T=
   OUT$ want wu CONTAINS? TTRUE ;

\ REJECTED means: fail-closed with the checker's code, and the diagnostic names
\ the offending definition. The code is asserted, not the wording. The name is
\ matched LOWERCASE because that is how the engine renders a definition name in
\ a diagnostic (`habu: in bad: ...`), not because the case is unimportant.
: ASSERT-REJECTED ( ptr u8 n -- ) {: name:ptr nu:n :}
   EXITED @ TTRUE
   RC @ REJECT-RC T=
   ERR$ name nu CONTAINS? TTRUE ;

: ASSERT-RC ( n -- ) {: want:n :}
   EXITED @ TTRUE
   RC @ want T= ;

\ ---- 1. both tiers compile, run and check ------------------------------------
: TEST-BOTH-TIERS ( -- )
   s" tier 0 is the cold default" T-LABEL
   s" tier@ . cr" RUN  s" 0" ASSERT-OK

   s" tier 0 compiles and runs a checked definition" T-LABEL
   s" : SQ ( n -- n ) dup * ; 7 SQ . cr" RUN  s" 49" ASSERT-OK

   s" tier 1 compiles and runs a checked definition" T-LABEL
   s" 1 set-tier : SQ ( n -- n ) dup * ; 7 SQ . cr" RUN  s" 49" ASSERT-OK

   s" set-tier is read back by tier@" T-LABEL
   s" 1 set-tier tier@ . cr" RUN  s" 1" ASSERT-OK

   s" tier 0 rejects a wrong stack effect" T-LABEL
   s" : BAD ( n -- n n ) dup * ;" RUN  s" bad" ASSERT-REJECTED

   s" tier 1 rejects a wrong stack effect" T-LABEL
   s" 1 set-tier : BAD ( n -- n n ) dup * ;" RUN  s" bad" ASSERT-REJECTED ;

\ ---- 2. set-tier admits 0 and 1 and nothing else -----------------------------
: TEST-SELECTION ( -- )
   s" set-tier 0 is accepted" T-LABEL
   s" 0 set-tier tier@ . cr" RUN  s" 0" ASSERT-OK

   s" set-tier 2 is refused" T-LABEL
   s" 2 set-tier" RUN  REJECT-RC ASSERT-RC
   ERR$ s" set-tier" CONTAINS? TTRUE

   s" set-tier -1 is refused" T-LABEL
   s" -1 set-tier" RUN  REJECT-RC ASSERT-RC
   ERR$ s" set-tier" CONTAINS? TTRUE ;

\ ---- 3. tier 0 runs the constructs its closure owns --------------------------
: TEST-TIER0-CONSTRUCTS ( -- )
   s" tier 0 compiles a quotation" T-LABEL
   s" : Q ( -- n ) [: 7 ;] execute ; Q . cr" RUN  s" 7" ASSERT-OK

   s" tier 0 compiles BEGIN / UNTIL" T-LABEL
   s" : B1 ( -- n ) 0 begin 1 + dup 5 >= until ; B1 . cr" RUN  s" 5" ASSERT-OK

   s" tier 0 compiles BEGIN / WHILE / REPEAT" T-LABEL
   s" : B2 ( -- n ) 0 begin dup 5 < while 1 + repeat ; B2 . cr" RUN  s" 5" ASSERT-OK

   s" tier 0 compiles DO / LOOP" T-LABEL
   s" : B3 ( -- n ) 0 5 0 do 1 + loop ; B3 . cr" RUN  s" 5" ASSERT-OK ;

\ ---- 4. the snapshot depth dies with the definition --------------------------
\ THE REGRESSION. Thirty definitions that each open a BEGIN and then fail on an
\ undefined word, caught one at a time inside `evaluate`, then one good BEGIN
\ definition that has to compile and run. Before the fix the thirtieth failure
\ never returned: the twenty-ninth `begin` exited 75 from inside the engine,
\ which no catch can see.
: LEAK-SRC$ ( -- ptr u8 n )
   S\" TRUSTED: EV ( ptr u8 n -- n ) [: evaluate ;] catch ;\n: X30 ( -- ) 30 0 do s\q : BAD-B ( -- ) begin MISSING ;\q EV drop loop ;\nX30\n: GOOD ( -- n ) 0 begin 1 + dup 5 >= until ;\nGOOD . cr\n" ;

: TEST-SNAPSHOT-OWNERSHIP ( -- )
   s" thirty failed BEGIN definitions leave the depth clean" T-LABEL
   LEAK-SRC$ RUN  s" 5" ASSERT-OK

   s" one failed BEGIN definition is caught, not fatal" T-LABEL
   S\" TRUSTED: EV ( ptr u8 n -- n ) [: evaluate ;] catch ;\ns\q : BAD-B ( -- ) begin MISSING ;\q EV . cr\n"
   RUN  s" 70" ASSERT-OK ;

\ ---- 5. the reset stops at the definition boundary ---------------------------
\ A quotation is compiled inline, so these share one frame stack with their
\ enclosing definition and must still balance.
: TEST-NESTED-QUOTATIONS ( -- )
   s" a quotation inside BEGIN keeps its frames" T-LABEL
   s" : N1 ( -- n ) 0 begin [: 1 ;] execute + dup 5 >= until ; N1 . cr"
   RUN  s" 5" ASSERT-OK

   s" a BEGIN inside a quotation keeps its frames" T-LABEL
   s" : N2 ( -- n ) [: 0 begin 1 + dup 4 >= until ;] execute ; N2 . cr"
   RUN  s" 4" ASSERT-OK

   s" BEGIN inside a quotation inside BEGIN keeps its frames" T-LABEL
   s" : N3 ( -- n ) 0 begin [: 0 begin 1 + dup 2 >= until ;] execute + dup 6 >= until ; N3 . cr"
   RUN  s" 6" ASSERT-OK

   s" the same nesting compiles on tier 1" T-LABEL
   s" 1 set-tier : N4 ( -- n ) 0 begin [: 1 ;] execute + dup 5 >= until ; N4 . cr"
   RUN  s" 5" ASSERT-OK ;

\ ---- 6. the nesting bound is a bound, not a corruption -----------------------
\ Past the frame area the emitter must refuse deterministically. This also pins
\ the frame count: the relocated band is sized JIT-SNAP:FRAMES * FRAME-BYTES, and
\ a band that silently aliased its neighbour would run PAST this without failing.
: TEST-NESTING-BOUND ( -- )
   s" BEGIN nesting inside the bound compiles" T-LABEL
   s" : D26 ( -- n ) 0 begin 1 + dup 2 < while 0 drop repeat ; D26 . cr" RUN
   s" 2" ASSERT-OK ;

public

: RUN-ALL ( -- )
   T-RESET
   TEST-BOTH-TIERS
   TEST-SELECTION
   TEST-TIER0-CONSTRUCTS
   TEST-SNAPSHOT-OWNERSHIP
   TEST-NESTED-QUOTATIONS
   TEST-NESTING-BOUND
   T-REPORT
   s" tier: ok" type cr ;

;package

TIER-TEST:RUN-ALL
