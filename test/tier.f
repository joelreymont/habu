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
require lib/fmt.f
require lib/test.f
require lib/test/outcome.f
require lib/test/subject.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f

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

\ The SAME program through a real `bin/hb` process, stdin-piped.
\ SUBJECT:RUN forks in-process and never reaches the CLI's own eval boundary, so
\ the recovery cases below -- which crashed exactly there -- have to exec.
: HB$ ( -- ptr u8 n )
   s" HABU_UNDER_TEST" >LEN PROC-ENV-DEFAULT$? if LEN>N exit then
   2drop
   s" HABU_UNDER_TEST" GETENV dup 0= if
      2drop s" bin/hb" exit
   then ;

: EXEC ( ptr u8 n -- ) {: src:ptr u:n :}
   PROC-ARGV-RESET
   HB$ >LEN  src u >LEN  OUT CAP >LEN
   ERR CAP >LEN  TIMEOUT-MS >MS  RUN-ARGV-STDIN-CAPTURE-OUTCOME
   STORE! ;

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

\ ---- 2b. the two primitives are callable from CHECKED code -------------------
\ Registering a primitive in the engine dictionary is only half of it. Without a
\ checker effect row the word is E-UNDEFINED inside every checked body, so
\ `: R ( -- n ) tier@ ;` rejected and no checked build driver could select a
\ tier at all. The rows follow the policy their neighbours already carry: tier@
\ is an ordinary reader like check@, set-tier is a trust boundary like
\ set-check. BOTH halves are asserted, because a set-tier any checked body could
\ call would let checked code swap the compiler out from under itself.
: TEST-CHECKER-ROWS ( -- )
   s" tier@ is callable from a checked body on tier 0" T-LABEL
   s" : R ( -- n ) tier@ ; R . cr" RUN  s" 0" ASSERT-OK

   s" tier@ is callable from a checked body on tier 1" T-LABEL
   s" 1 set-tier : R ( -- n ) tier@ ; R . cr" RUN  s" 1" ASSERT-OK

   s" set-tier is callable from a TRUSTED: body on tier 0" T-LABEL
   s" TRUSTED: ST ( n -- ) set-tier ; 1 ST tier@ . cr" RUN  s" 1" ASSERT-OK

   s" set-tier is callable from a TRUSTED: body on tier 1" T-LABEL
   s" 1 set-tier TRUSTED: ST ( n -- ) set-tier ; 0 ST tier@ . cr" RUN  s" 0" ASSERT-OK

   s" set-tier is refused in a plain checked body" T-LABEL
   s" : S ( -- ) 1 set-tier ;" RUN  REJECT-RC ASSERT-RC
   ERR$ s" trust-boundary primitive" CONTAINS? TTRUE ;

\ ---- 2b0. certification is the engine's hook CELL, read per definition --------
\ Tier 0 reads the cell for every definition, which is what makes `0 set-check`
\ take certification off and installing another hook move it. Tier 1 called its
\ hook BY NAME, so the cell was decoration: the compiler certified through the
\ instance it was built into whatever the session had installed, and a run that
\ replaced the checker had its own files judged by the retired one.
\
\ The swap is asserted from BOTH sides in one program: the definition BEFORE it
\ prints, so the real hook certified that one, and the definition AFTER it carries
\ the installed hook's own throw code out to the process. A compiler that bound the
\ hook by name exits 0 here and prints the same 7 - measured on the root engine -
\ which is why the exit code is the claim and the output alone is not.
: TEST-HOOK-CELL ( -- )
   s" a hook installed mid-session certifies the next definition" T-LABEL
   S\" 1 set-tier\nTRUSTED: HK-STOP ( ptr u8 n -- n ) CHECK! drop 77 throw ;\n: HK-A ( -- n ) 7 ;\nHK-A . cr\nTRUSTED: HK-SWAP ( -- ) ['] HK-STOP set-check ;\nHK-SWAP\n: HK-B ( -- n ) 8 ;\n"
   EXEC
   77 ASSERT-RC
   OUT$ s" 7" CONTAINS? TTRUE           \ the definition before the swap was certified and ran
   ERR$ s" HK-B" CONTAINS? TTRUE ;      \ the one after it was refused by name

\ ---- 2b'. a TRUSTED: body may call a trust-boundary primitive on BOTH tiers ---
\ The optimizing compiler reads a callee's cell widths out of the checker's
\ effect table for every name a body writes, and a PRIMITIVE is the one kind of
\ name no scan can ever supply one for. `int-mark` and `min-in-mark` -- the two
\ writers src/core/internal-mark.f seals the dictionary with -- had no row, so
\ tier 1 could not compile their one-line TRUSTED: wrappers at all
\ (E-HIR-UNMODELED, `ncomp: cannot compile MARK-INTERNAL at int-mark`), which is
\ what stopped a product engine from rebuilding the tree. The JIT tier never
\ asked, so the absence was invisible until the build selected tier 1.
\
\ BOTH DIRECTIONS, because a row that admitted checked callers would delete the
\ boundary instead of crossing it: the wrapper compiles on both tiers, a plain
\ checked body naming either primitive is refused AND the diagnostic names the
\ primitive, and the bare name stays unreachable at top level.
: TEST-MARK-ROWS ( -- )
   s" int-mark's TRUSTED: wrapper compiles on tier 0" T-LABEL
   s" TRUSTED: MKI ( n -- ) int-mark ; 5 . cr" RUN  s" 5" ASSERT-OK

   s" int-mark's TRUSTED: wrapper compiles on tier 1" T-LABEL
   s" 1 set-tier TRUSTED: MKI ( n -- ) int-mark ; 5 . cr" RUN  s" 5" ASSERT-OK

   s" min-in-mark's TRUSTED: wrapper compiles on tier 0" T-LABEL
   s" TRUSTED: MKM ( n n -- ) min-in-mark ; 6 . cr" RUN  s" 6" ASSERT-OK

   s" min-in-mark's TRUSTED: wrapper compiles on tier 1" T-LABEL
   s" 1 set-tier TRUSTED: MKM ( n n -- ) min-in-mark ; 6 . cr" RUN  s" 6" ASSERT-OK

   s" int-mark is refused in a plain checked body on tier 0" T-LABEL
   s" : BADI ( n -- ) int-mark ;" RUN  REJECT-RC ASSERT-RC
   ERR$ s" trust-boundary primitive" CONTAINS? TTRUE
   ERR$ s" int-mark" CONTAINS? TTRUE

   s" int-mark is refused in a plain checked body on tier 1" T-LABEL
   s" 1 set-tier : BADI ( n -- ) int-mark ;" RUN  REJECT-RC ASSERT-RC
   ERR$ s" trust-boundary primitive" CONTAINS? TTRUE

   s" min-in-mark is refused in a plain checked body" T-LABEL
   s" : BADM ( n n -- ) min-in-mark ;" RUN  REJECT-RC ASSERT-RC
   ERR$ s" trust-boundary primitive" CONTAINS? TTRUE
   ERR$ s" min-in-mark" CONTAINS? TTRUE

   \ The row records an effect; it does not open the name. Both records still
   \ carry DNAME-INT, so interpret fails closed exactly as before.
   s" the marking prims stay unreachable at top level" T-LABEL
   s" 0 int-mark" RUN  REJECT-RC ASSERT-RC
   ERR$ s" internal engine word" CONTAINS? TTRUE ;

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

\ ---- 6. the nesting bound is a bound, not a corruption ----------------------
\ 28 frames fit; the 29th must refuse deterministically rather than run off the
\ end of the relocated band into whatever follows it. Built rather than spelled
\ out, so the depths track JIT-SNAP:FRAMES instead of a typist.
: NEST-SRC$ ( n -- ptr u8 n ) {: depth:n :}
   SB-RESET
   s" : ZN ( -- n ) 0 " SB-APPEND
   depth 0 ?do s" begin 1 + dup 2 < while " SB-APPEND loop
   depth 0 ?do s" repeat " SB-APPEND loop
   s" ; ZN . cr" SB-APPEND
   SB$ ;

: TEST-NESTING-BOUND ( -- )
   s" BEGIN nested to the frame bound compiles and runs" T-LABEL
   28 NEST-SRC$ EXEC  s" 3" ASSERT-OK

   s" one BEGIN past the bound refuses" T-LABEL
   29 NEST-SRC$ EXEC  NEST-RC ASSERT-RC ;

\ ---- 7. a failed definition under a top-level catch is recoverable ----------
\ THE CRASH. `' evaluate catch` at top level installs its handler at exactly the
\ SP that B-EVAL later records as the eval-frame boundary. The recovery treated
\ equal as "handler inside the frame", never popped the frame, and left CP and
\ the protection band at the abandoned definition -- so the next token was
\ emitted into a re-protected band and the process died of SIGSEGV (rc 134)
\ AFTER printing a correct rejection. These run as real processes because the
\ in-process fork does not reach that boundary.
: TEST-EVAL-RECOVERY ( -- )
   s" tier 0 recovers from a rejected effect under a top-level catch" T-LABEL
   S\" s\q : RVND ( n -- n ) dup ;\q ' evaluate catch . cr\n" EXEC  s" 70" ASSERT-OK

   s" tier 0 recovers from an undefined word under a top-level catch" T-LABEL
   S\" s\q : RVU ( -- ) MISSINGW ;\q ' evaluate catch . cr\n" EXEC  s" 70" ASSERT-OK

   s" tier 1 recovers from the same rejection" T-LABEL
   S\" 1 set-tier s\q : RVND ( n -- n ) dup ;\q ' evaluate catch . cr\n" EXEC
   s" 70" ASSERT-OK

   s" the session keeps compiling after the recovery" T-LABEL
   S\" s\q : RVND ( n -- n ) dup ;\q ' evaluate catch drop\n: ZKOK ( -- n ) 9 ;\nZKOK . cr\n"
   EXEC  s" 9" ASSERT-OK

   s" three recoveries in a row still leave a usable session" T-LABEL
   S\" s\q : RA ( n -- n ) dup ;\q ' evaluate catch drop\ns\q : RB ( n -- n ) dup ;\q ' evaluate catch drop\ns\q : RC ( n -- n ) dup ;\q ' evaluate catch drop\n: ZKOK2 ( -- n ) 7 ;\nZKOK2 . cr\n"
   EXEC  s" 7" ASSERT-OK

   s" the quotation form still recovers" T-LABEL
   S\" TRUSTED: EV ( ptr u8 n -- n ) [: evaluate ;] catch ;\ns\q : RVND ( n -- n ) dup ;\q EV . cr\n"
   EXEC  s" 70" ASSERT-OK ;

: TEST-ORIGIN ( -- )
   s" known primitive text has positive native origin" T-LABEL
   s" ' dup dup 4 + code-origin . " EXEC s" 1" ASSERT-OK
   s" absent and empty coverage remain unknown" T-LABEL
   s" 0 4 code-origin .  0 0 code-origin . ' dup -1 code-origin ." EXEC
   0 ASSERT-RC OUT$ S\" -1\n-1\n-1\n" STR= TTRUE
   s" changing the next tier cannot relabel a retained JIT definition" T-LABEL
   s" 0 set-tier : OR-J ( -- n ) 42 ; ' OR-J dup 4 + code-origin .  1 set-tier ' OR-J dup 4 + code-origin .  OR-J . " EXEC
   0 ASSERT-RC OUT$ S\" 0\n0\n42\n" STR= TTRUE
   s" native definition publication supplies positive origin" T-LABEL
   s" 1 set-tier : OR-N ( -- n ) 43 ; ' OR-N dup 4 + code-origin .  0 set-tier ' OR-N dup 4 + code-origin .  OR-N . " EXEC
   0 ASSERT-RC OUT$ S\" 1\n1\n43\n" STR= TTRUE
   s" stored quotations keep their enclosing definition's origin" T-LABEL
   s" 0 set-tier : OR-Q ( -- [ -- n ] ) [: 44 ;] ; OR-Q dup 4 + code-origin .  1 set-tier : OR-NQ ( -- [ -- n ] ) [: 45 ;] ; OR-NQ dup 4 + code-origin . " EXEC
   0 ASSERT-RC OUT$ S\" 0\n1\n" STR= TTRUE
   s" a native CREATE stub cannot relabel its retained JIT does body" T-LABEL
   s" 0 set-tier : OR-M ( -- ) create does> ( -- n ) drop 46 ; OR-M OR-C ' OR-C dup 4 + code-origin .  ' OR-M dup 4 + code-origin .  OR-C . " EXEC
   0 ASSERT-RC OUT$ S\" 1\n0\n46\n" STR= TTRUE
   s" a tier change inside an immediate affects only the next definition" T-LABEL
   s" 0 set-tier TRUSTED: OR-SW ( -- ) 1 set-tier ; immediate : OR-L ( -- n ) OR-SW 47 ; ' OR-L dup 4 + code-origin .  tier@ .  OR-L . " EXEC
   0 ASSERT-RC OUT$ S\" 0\n1\n47\n" STR= TTRUE
   s" a code cursor rewind hides bytes without reclassifying them" T-LABEL
   S\" 0 set-tier variable OR-LO variable OR-HI cp@ OR-LO ! : OR-HIDDEN ( -- n ) 48 ; cp@ OR-HI ! OR-LO @ cp! OR-HI @ cp! ' OR-HIDDEN dup 4 + code-origin .  OR-HIDDEN . \n" EXEC
   0 ASSERT-RC OUT$ S\" 0\n48\n" STR= TTRUE ;

: TEST-ORIGIN-OVERWRITE ( -- )
   s" a generic instruction patch invalidates native provenance" T-LABEL
   S\" 1 set-tier : OR-PATCHED ( -- n ) 56 ;\nTRUSTED: OR-PATCH ( n -- ) dup @ swap patch32 ;\n' OR-PATCHED dup 4 + code-origin . ' OR-PATCHED OR-PATCH ' OR-PATCHED dup 4 + code-origin .\n" EXEC
   0 ASSERT-RC OUT$ S\" 1\n-1\n" STR= TTRUE
   s" exported aliases retain the target body's origin" T-LABEL
   s" 0 set-tier package OA public : V ( -- n ) 51 ; ;package 1 set-tier package OB public export OA:V ;package ' OA:V dup 4 + code-origin . ' OB:V dup 4 + code-origin . OB:V ." EXEC
   0 ASSERT-RC OUT$ S\" 0\n0\n51\n" STR= TTRUE
   s" an interior native overwrite retains both JIT edges and merges an adjacent native body" T-LABEL
   S\" 0 set-tier variable OL variable OH variable ON variable OE variable OC\nTRUSTED: OR-COUNT ( -- n ) data-base TIER-PROV:N-CELL + @ ;\ncp@ OL ! : OLD ( -- ) 1 emit 2 emit 3 emit 4 emit 5 emit 6 emit 7 emit 8 emit ; cp@ OH ! OR-COUNT OC !\nOL @ 16 + dup ON ! cp! 1 set-tier : NEW1 ( -- n ) 52 ; : NEW2 ( -- n ) 53 ; cp@ OE !\nOL @ dup 4 + code-origin . ON @ OE @ code-origin . OH @ 4 - OH @ code-origin . OL @ OH @ code-origin . OR-COUNT OC @ - . NEW1 . NEW2 .\n" EXEC
   0 ASSERT-RC OUT$ S\" 0\n1\n0\n-1\n2\n52\n53\n" STR= TTRUE
   s" a full native overwrite removes the JIT provenance it actually replaces" T-LABEL
   s" 0 set-tier variable OL cp@ OL ! : OLD ( -- n ) 1 ; OL @ cp! 1 set-tier : NEW-WITH-A-LONG-EXTERNAL-NAME ( -- n ) 54 ; OL @ cp@ code-origin . NEW-WITH-A-LONG-EXTERNAL-NAME ." EXEC
   0 ASSERT-RC OUT$ S\" 1\n54\n" STR= TTRUE ;

\ Generate actual alternating engine/JIT publications up to the physical cap.
\ No forged count or row store can make this pass without exercising insertion.
$50000 constant OR-CAP
60000 constant OR-TIMEOUT-MS
create OR-BUF OR-CAP allot
variable OR-U

: OR+ ( ptr u8 n -- ) {: a:ptr u:n :}
   OR-U @ u + OR-CAP > if E-STR-CAPACITY throw then
   a OR-BUF OR-U @ + u BYTE-COPY
   u OR-U +! ;

: OR-NUM+ ( n -- ) SB-RESET FMT:SB-U SB$ OR+ ;

: OR-SOURCE$ ( n -- ptr u8 n ) {: count:n :}
   0 OR-U !
   S\" 0 set-tier\nTRUSTED: OR-COUNT ( -- n ) data-base TIER-PROV:N-CELL + @ ;\n: OR-HEADROOM ( -- ) OR-COUNT TIER-PROV:SPANS 64 - > OR-COUNT TIER-PROV:SPANS <= and if s\q headroom-ok\q type else 70 throw then ;\n" OR+
   count 0 ?do
      s" variable OV" OR+ i OR-NUM+
      S\" \n: OD" OR+ i OR-NUM+ S\"  ( -- ) ;\n" OR+
   loop
   S\" OR-HEADROOM\n" OR+
   OR-BUF OR-U @ ;

: OR-PATH$ ( -- ptr u8 n ) s" hb-code-origin-capacity.f" TMP-PATH ;

: OR-LOAD ( -- )
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   OR-PATH$ >LEN PROC-ARGV+
   HB$ >LEN s" " >LEN OUT CAP >LEN ERR CAP >LEN
   OR-TIMEOUT-MS >MS RUN-ARGV-STDIN-CAPTURE-OUTCOME STORE! ;

: TEST-ORIGIN-CAPACITY ( -- )
   s" real publications use the declared capacity with headroom" T-LABEL
   OR-PATH$ TIER-PROV:SPANS 2 / 16 - OR-SOURCE$ ATOMIC-WRITE-FILE
   OR-LOAD s" headroom-ok" ASSERT-OK
   s" exhausting the interval store refuses before losing a row" T-LABEL
   OR-PATH$ TIER-PROV:SPANS 2 / 16 + OR-SOURCE$ ATOMIC-WRITE-FILE
   OR-LOAD ENGINE-ERROR:CODE-ORIGIN-FULL ASSERT-RC
   ERR$ S\" hb: code-origin table capacity\n" STR= TTRUE
   OR-PATH$ REMOVE-FILE ;

: TEST-BUILD-SCOPE ( -- )
   s" build scope selects native and restores default JIT through nested scopes" T-LABEL
   S\" require lib/executable-build.f\n: BS-IN ( -- n ) [: tier@ ;] EXECUTABLE-BUILD:WITH ;\n: BS-RUN ( -- n n ) [: tier@ BS-IN ;] EXECUTABLE-BUILD:WITH ; BS-RUN . . tier@ . \n" EXEC
   0 ASSERT-RC OUT$ S\" 1\n1\n0\n" STR= TTRUE
   s" build scope preserves higher-order inputs and outputs" T-LABEL
   S\" require lib/executable-build.f\n: BS-RUN ( n -- n n ) [: 1+ dup ;] EXECUTABLE-BUILD:WITH ; 48 BS-RUN . . tier@ . \n" EXEC
   0 ASSERT-RC OUT$ S\" 49\n49\n0\n" STR= TTRUE
   s" tier0 requests are refused before compilation and cleanup restores JIT" T-LABEL
   S\" require lib/executable-build.f\nTRUSTED: BS-BAD ( -- ) 0 set-tier ;\n: BS-RUN ( -- ) [: BS-BAD ;] EXECUTABLE-BUILD:WITH ; ' BS-RUN catch .  tier@ .  : BS-AFTER ( -- n ) 50 ; BS-AFTER . \n" EXEC
   0 ASSERT-RC OUT$ S\" 70\n0\n50\n" STR= TTRUE
   ERR$ s" executable build requires native tier 1" CONTAINS? TTRUE
   s" arbitrary throws preserve their code and the outer tier" T-LABEL
   S\" 1 set-tier require lib/executable-build.f\n: BS-RUN ( -- ) [: 79 throw ;] EXECUTABLE-BUILD:WITH ; ' BS-RUN catch .  tier@ . \n" EXEC
   0 ASSERT-RC OUT$ S\" 79\n1\n" STR= TTRUE
   s" evaluated source publishes native code inside the scope" T-LABEL
   S\" require lib/executable-build.f\nTRUSTED: BS-EVAL ( ptr u8 n -- ) evaluate ;\n: BS-RUN ( -- ) [: s\q : BS-NATIVE ( -- n ) 61 ;\q BS-EVAL ;] EXECUTABLE-BUILD:WITH ; BS-RUN ' BS-NATIVE dup 4 + code-origin . BS-NATIVE . tier@ .\n" EXEC
   0 ASSERT-RC OUT$ S\" 1\n61\n0\n" STR= TTRUE
   s" evaluated tier0 refusal leaves its following definition unpublished" T-LABEL
   S\" require lib/executable-build.f\nTRUSTED: BS-EVAL ( ptr u8 n -- ) evaluate ;\n: BS-RUN ( -- ) [: s\q 0 set-tier : BS-FORBIDDEN ( -- n ) 62 ;\q BS-EVAL ;] EXECUTABLE-BUILD:WITH ; ' BS-RUN catch . tier@ . ' BS-FORBIDDEN drop\n" EXEC
   REJECT-RC ASSERT-RC OUT$ S\" 70\n0\n" STR= TTRUE
   ERR$ s" executable build requires native tier 1" CONTAINS? TTRUE
   ERR$ s" BS-FORBIDDEN" CONTAINS? TTRUE
   s" raw stores cannot erase retained provenance" T-LABEL
   s" 0 data-base TIER-PROV:N-CELL + !" EXEC ENGINE-ERROR:SEAL-VIOLATION ASSERT-RC
   s" raw stores cannot disable the executable scope" T-LABEL
   s" 0 data-base NCOMP-DISPATCH:BUILD-DEPTH-CELL + !" EXEC ENGINE-ERROR:SEAL-VIOLATION ASSERT-RC
   s" ordinary source cannot leave the executable scope" T-LABEL
   s" : BS-ESCAPE ( -- ) executable-build-leave ;" EXEC REJECT-RC ASSERT-RC ;

public

: RUN-ALL ( -- )
   T-RESET
   TEST-BOTH-TIERS
   TEST-SELECTION
   TEST-ORIGIN
   TEST-ORIGIN-OVERWRITE
   TEST-ORIGIN-CAPACITY
   TEST-BUILD-SCOPE
   TEST-CHECKER-ROWS
   TEST-HOOK-CELL
   TEST-MARK-ROWS
   TEST-TIER0-CONSTRUCTS
   TEST-SNAPSHOT-OWNERSHIP
   TEST-NESTED-QUOTATIONS
   TEST-NESTING-BOUND
   TEST-EVAL-RECOVERY
   T-REPORT
   s" tier: ok" type cr ;

;package

TIER-TEST:RUN-ALL
