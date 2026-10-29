\ tier.f - the two compiler tiers, and the state that must not survive a
\ definition.
\
\ ARM carries the legacy JIT at tier 0 and the IR compiler at tier 1. Intel
\ runs only tier 1. `set-tier` selects an available compiler and `tier@` reads
\ it back. ARM starts at tier 0; Intel starts at tier 1.
\
\ WHAT THIS SUITE CLAIMS, AND WHY EACH PART IS NOT ALREADY CLAIMED ELSEWHERE.
\
\   1. Every available tier compiles and runs a checked definition and rejects
\      a wrong stack effect. A tier is a choice of compiler, not a choice of
\      whether the checker runs. ARM tests both tiers; Intel tests tier 1 and
\      the explicit refusal of tier 0.
\
\   2. `set-tier` refuses everything but the available tiers. A tier is not a
\      truthy flag: 2 and -1 cannot silently pick a compiler.
\
\   3. On ARM, tier 0's BEGIN-snapshot depth DIES WITH THE DEFINITION. jit.f
\      pushes a snapshot frame at each BEGIN and pops it at the back edge; a
\      definition that FAILS between the two - an undefined word in the body,
\      say - never reaches its back edge, so the depth stays raised. The cell
\      is per-definition state, cleared with VSP/LVD/EXITH at both colon
\      entries and in EM-ABORT-COMPILE-STATE, which both recovery paths reach.
\      The frame area holds the control-flow cap's worth of frames, so a leak
\      reaches no frame bound: the test reads the depth cell itself after the
\      failed definitions and wants 0.
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

require src/lib/errors.f
require src/lib/string.f
require src/lib/fmt.f
require src/lib/test.f
require src/lib/test/outcome.f
require src/lib/test/subject.f
require src/lib/memory.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/engine-candidate.f

package TIER-TEST

private

\ A child's whole output. A long-name case that fails prints its name, up to
\ 7900 bytes, and the capture has to hold it for the case to report.
$4000 constant CAP
20000 constant TIMEOUT-MS
70 constant REJECT-RC              \ checker reject / undefined word, fail-closed
75 constant INCOMPLETE-RC          \ habu2.f VALIDATE: a certificate with incomplete width facts
67 constant UNCAUGHT-RC            \ hb's exit status for an uncaught throw

create OUT CAP allot
create ERR CAP allot
variable OUT-U  variable ERR-U
variable RC     variable EXITED

: LEGACY? ( -- bool ) HB-TARGET-LINUX-X86-64? 0= ;

: OUT$ ( -- ptr u8 n )  OUT OUT-U @ ;
: ERR$ ( -- ptr u8 n )  ERR ERR-U @ ;

: STORE! ( len len outcome ptr u8 n -- ) {: outu:len erru:len oc src:ptr u:n :}
   erru LEN>N ERR-U !  outu LEN>N OUT-U !
   oc MATCH outcome
     exited   OF RC ! 0 0= EXITED ! ENDOF
     signaled OF RC ! 0 0= 0= EXITED ! ENDOF
     timeout  OF src u OUT$ ERR$ T-TIMED-OUT ENDOF
   ;MATCH ;

: RUN ( ptr u8 n -- ) {: src:ptr u:n :}
   src u OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS SUBJECT:RUN src u STORE! ;

\ The SAME program through a real engine process, stdin-piped.
\ SUBJECT:RUN forks in-process and never reaches the CLI's own eval boundary, so
\ the recovery cases below -- which crashed exactly there -- have to exec.
\ The engine under test: the gate's freshly built candidate when it exported
\ one, else the running engine itself (src/lib/engine-candidate.f).
: HB$ ( -- ptr u8 n )       ENGINE-CANDIDATE:PATH$ ;

: EXEC ( ptr u8 n -- ) {: src:ptr u:n :}
   PROC-ARGV-RESET
   HB$ >LEN  src u >LEN  OUT CAP >LEN
   ERR CAP >LEN  TIMEOUT-MS >MS  RUN-ARGV-STDIN-CAPTURE-OUTCOME
   src u STORE! ;

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

\ The line the child prints for an uncaught code, rendered from the name so the
\ needle follows src/lib/errors.f instead of repeating its number.
: UNCAUGHT$ ( n -- ptr u8 n )
   {: code:n :}
   SB-RESET
   s" hb: uncaught throw code " SB-APPEND
   code FMT:SB-INT
   SB$ ;

\ ---- 1. both tiers compile, run and check ------------------------------------
: TEST-BOTH-TIERS ( -- )
   s" the default is an available compiler" T-LABEL
   s" tier@ . cr" RUN
   LEGACY? if s" 0" else s" 1" then ASSERT-OK

   LEGACY? if
      s" tier 0 compiles and runs a checked definition" T-LABEL
      s" : SQ ( n -- n ) dup * ; 7 SQ . cr" RUN  s" 49" ASSERT-OK
   then

   s" tier 1 compiles and runs a checked definition" T-LABEL
   s" 1 set-tier : SQ ( n -- n ) dup * ; 7 SQ . cr" RUN  s" 49" ASSERT-OK

   s" set-tier is read back by tier@" T-LABEL
   s" 1 set-tier tier@ . cr" RUN  s" 1" ASSERT-OK

   LEGACY? if
      s" tier 0 rejects a wrong stack effect" T-LABEL
      s" : BAD ( n -- n n ) dup * ;" RUN  s" bad" ASSERT-REJECTED
   then

   s" tier 1 rejects a wrong stack effect" T-LABEL
   s" 1 set-tier : BAD ( n -- n n ) dup * ;" RUN  s" bad" ASSERT-REJECTED ;

\ ---- 2. set-tier admits 0 and 1 and nothing else -----------------------------
: TEST-SELECTION ( -- )
   LEGACY? if
      s" set-tier 0 is accepted" T-LABEL
      s" 0 set-tier tier@ . cr" RUN  s" 0" ASSERT-OK
   else
      s" Intel refuses its unavailable JIT tier" T-LABEL
      s" 0 set-tier" RUN REJECT-RC ASSERT-RC
      ERR$ s" tier 1 only" CONTAINS? TTRUE
   then

   s" set-tier 2 is refused" T-LABEL
   s" 2 set-tier" RUN  REJECT-RC ASSERT-RC
   ERR$ s" set-tier" CONTAINS? TTRUE

   s" set-tier -1 is refused" T-LABEL
   s" -1 set-tier" RUN  REJECT-RC ASSERT-RC
   ERR$ s" set-tier" CONTAINS? TTRUE ;

\ The JIT reaches its trusted tick gate before the closing checker scan;
\ native compilation checks the body before it elaborates the tick.
: TEST-TRUSTED-TICK-ORDER ( -- )
   s" the fresh tier 0 boundary has a trusted gate" T-LABEL
   s" tick-order@ drop . cr" EXEC s" 1" ASSERT-OK
   s" tier 1 starts with checking order" T-LABEL
   s" 1 set-tier tick-order@ drop . cr" EXEC s" 0" ASSERT-OK
   s" tier 0 reports the selected trusted tick before a body underflow" T-LABEL
   s" : IWGGPF ( -- ) drop ['] patch32 drop ;" EXEC
   REJECT-RC ASSERT-RC
   ERR$ s" trusted-only tick" CONTAINS? TTRUE
   s" tier 1 keeps the first checker failure at drop" T-LABEL
   s" 1 set-tier : IWGGPF ( -- ) drop ['] patch32 drop ;" EXEC
   s" iwggpf" ASSERT-REJECTED
   ERR$ s" underflow" CONTAINS? TTRUE
   s" a later undefined token cannot displace the tier 0 gate" T-LABEL
   s" : TICK-LATE ( -- ) drop ['] patch32 drop NO-SUCH-WORD ;" EXEC
   REJECT-RC ASSERT-RC
   ERR$ s" trusted-only tick" CONTAINS? TTRUE
   s" a malformed signature cannot displace the tier 0 gate" T-LABEL
   s" : TICK-SIG ( -- zz ) ['] patch32 drop ;" EXEC
   REJECT-RC ASSERT-RC
   ERR$ s" trusted-only tick" CONTAINS? TTRUE
   s" an earlier undefined token refuses before the tier 0 gate" T-LABEL
   s" : TICK-EARLY ( -- ) NO-SUCH-WORD ['] patch32 drop ;" EXEC
   REJECT-RC ASSERT-RC
   ERR$ s" NO-SUCH-WORD" CONTAINS? TTRUE
   s" an unsafe check before the tier 0 gate leaves the gate first" T-LABEL
   s" : TICK-UNSAFE ( -- ) evaluate ['] patch32 drop ;" EXEC
   REJECT-RC ASSERT-RC
   ERR$ s" trusted-only tick" CONTAINS? TTRUE
   s" a package-local ordinary shadow has no trusted gate" T-LABEL
   s" package TICK-SHADOW public : patch32 ( -- n ) 1 ; ;package : SHADOW-TICK ( -- ) ['] TICK-SHADOW:patch32 drop ;" EXEC
   0 ASSERT-RC
   s" exporting the trusted primitive is refused at its export boundary" T-LABEL
   s" package TICK-ALIAS public EXPORT patch32 ;package" EXEC
   UNCAUGHT-RC ASSERT-RC
   ERR$ s" 7120" CONTAINS? TTRUE
   s" a does clause reaches the native trusted gate before its parent check" T-LABEL
   s" : TICK-DOES ( -- ) drop create does> ( -- ) drop ['] patch32 drop ;" EXEC
   REJECT-RC ASSERT-RC
   ERR$ s" trusted-only tick" CONTAINS? TTRUE
   s" a replacement require leaves the tick to the loaded subject" T-LABEL
   s" include test/tick-provider-replace.f" EXEC
   REJECT-RC ASSERT-RC
   OUT$ s" deferred-ok" CONTAINS? TTRUE
   ERR$ s" underflow" CONTAINS? TTRUE
   s" the original require may call a tier-changing source unit" T-LABEL
   s" include test/tick-provider-unit.f" EXEC
   REJECT-RC ASSERT-RC
   OUT$ s" deferred-ok" CONTAINS? TTRUE
   ERR$ s" underflow" CONTAINS? TTRUE ;

\ ---- 2b. checked code reads and selects the tier ------------------------------
\ Registering a primitive in the engine dictionary is only half of it. Without a
\ checker effect row the word is E-UNDEFINED inside every checked body, so
\ `: R ( -- n ) tier@ ;` rejected and no checked build driver could select a
\ tier at all. tier@ is an ordinary reader like check@, and set-tier's row is
\ global, so a checked body anywhere selects a tier by calling it.
: TEST-CHECKER-ROWS ( -- )
   LEGACY? if
      s" tier@ is callable from a checked body on tier 0" T-LABEL
      s" : R ( -- n ) tier@ ; R . cr" RUN  s" 0" ASSERT-OK
   then

   s" tier@ is callable from a checked body on tier 1" T-LABEL
   s" 1 set-tier : R ( -- n ) tier@ ; R . cr" RUN  s" 1" ASSERT-OK

   LEGACY? if
      s" set-tier selects from a checked body on tier 0" T-LABEL
      s" : ST ( n -- ) set-tier ; 1 ST tier@ . cr" RUN  s" 1" ASSERT-OK
   then

   s" set-tier selects from a checked body on tier 1" T-LABEL
   LEGACY? if
      s" 1 set-tier : ST ( n -- ) set-tier ; 0 ST tier@ . cr" RUN  s" 0" ASSERT-OK
   else
      s" 1 set-tier : ST ( n -- ) set-tier ; 1 ST tier@ . cr" RUN  s" 1" ASSERT-OK
   then ;

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
   LEGACY? if
      s" int-mark's TRUSTED: wrapper compiles on tier 0" T-LABEL
      s" TRUSTED: MKI ( n -- ) int-mark ; 5 . cr" RUN  s" 5" ASSERT-OK
   then

   s" int-mark's TRUSTED: wrapper compiles on tier 1" T-LABEL
   s" 1 set-tier TRUSTED: MKI ( n -- ) int-mark ; 5 . cr" RUN  s" 5" ASSERT-OK

   LEGACY? if
      s" min-in-mark's TRUSTED: wrapper compiles on tier 0" T-LABEL
      s" TRUSTED: MKM ( n n -- ) min-in-mark ; 6 . cr" RUN  s" 6" ASSERT-OK
   then

   s" min-in-mark's TRUSTED: wrapper compiles on tier 1" T-LABEL
   s" 1 set-tier TRUSTED: MKM ( n n -- ) min-in-mark ; 6 . cr" RUN  s" 6" ASSERT-OK

   LEGACY? if
      s" int-mark is refused in a plain checked body on tier 0" T-LABEL
      s" : BADI ( n -- ) int-mark ;" RUN  REJECT-RC ASSERT-RC
      \ Tier 0 reports the hidden name; tier 1 reports the trust boundary below.
      ERR$ s" E-UNDEFINED" CONTAINS? TTRUE
      ERR$ s" int-mark" CONTAINS? TTRUE
   then

   s" int-mark is refused in a plain checked body on tier 1" T-LABEL
   s" 1 set-tier : BADI ( n -- ) int-mark ;" RUN  REJECT-RC ASSERT-RC
   ERR$ s" trust-boundary primitive" CONTAINS? TTRUE

   s" min-in-mark is refused in a plain checked body" T-LABEL
   \ NPUB's owner row makes it an owned name: refused at the trust boundary.
   s" : BADM ( n n -- ) min-in-mark ;" RUN  REJECT-RC ASSERT-RC
   LEGACY? if
      ERR$ s" E-CAP-TRUSTED" CONTAINS? TTRUE
   else
      ERR$ s" trust-boundary primitive" CONTAINS? TTRUE
   then
   ERR$ s" min-in-mark" CONTAINS? TTRUE

   \ The row records an effect; it does not open the name. Both records still
   \ carry DNAME-INT, so interpret fails closed exactly as before.
   s" the marking prims stay unreachable at top level" T-LABEL
   s" 0 int-mark" RUN  REJECT-RC ASSERT-RC
   ERR$ s" internal engine word" CONTAINS? TTRUE ;

\ ---- 2c. a long name compiles on both tiers -----------------------------------
\ Tier 0 and the dictionary hold any name the definition's 8000-byte capture
\ holds, and tier 1 compiles each of them. Every program is generated around
\ one name and prints the same answer on both tiers. The lengths are 65, one
\ past the 64 bytes tier 1 once refused; 1000, past every 128-byte buffer a
\ function name was once copied through; and 7900, the capture less the rest of
\ the body. The forms reach each place a tier-1 compile spells the name: a body
\ with inputs, outputs and a quotation, whose function is named after the
\ definition; a does> definer with a quotation on each side, whose clause is its
\ `;does` companion; a TRUSTED: body; a caller; a caller of a word that never
\ returns, whose trap behind the call is named after the callee; a pending
\ definition whose body calls the prior word of its name; and an ENUM variant or
\ family, whose constructors the declaration generates as definitions holding
\ the name in their own name and body, as a MATCH over the family and a local
\ of its type hold it. The last three hold the name more than once, so the
\ pending definition runs at 1000 and the ENUM forms at LN-TAIL-MAX, the longest
\ family or variant name a declaration takes, still past every 128-byte buffer.
\ A long family's constructors use its family name as their package name, so
\ that program compiles them, its MATCH and its local, and stops. A refused long name
\ is reported whole up to its newline, and a refused TRUSTED: one retracts its
\ signature, which leaves the refusal's own code uncaught. A refusal at a
\ 7900-byte body token names that token whole: a TRUSTED: body skips the
\ checker, so its missing word reaches the elaborator.
$4000 constant LN-CAP
create LN-BUF LN-CAP allot
variable LN-U
variable LN-LEN

: LN+ ( ptr u8 n -- ) {: a:ptr u:n :}
   LN-U @ u + LN-CAP > if E-STR-CAPACITY throw then
   a LN-BUF LN-U @ + u BYTE-COPY
   u LN-U +! ;

\ The name: a two-byte stem, then `x` up to its length. A word's stem is `LN`;
\ an ENUM family or variant is a lowercase tail, `ln`.
: STEM+ ( ptr u8 n -- )
   LN+  LN-LEN @ 2 - 0 ?do s" x" LN+ loop ;

: NAME+ ( -- ) s" LN" STEM+ ;
: TAIL+ ( -- ) s" ln" STEM+ ;

\ The longest family or variant name, src/core/type-family.f TF-NAME-MAX: the
\ declaration refuses a longer one before either tier compiles anything.
TFAM:TF-NAME-MAX constant LN-TAIL-MAX

: LN-OPEN ( n -- ) {: len:n :}
   len LN-LEN !
   0 LN-U ! ;

\ A program opens by selecting its tier.
: TIER-OPEN ( n n -- ) {: tier:n len:n :}
   len LN-OPEN
   tier 1 = if s" 1 set-tier " LN+ exit then
   s" 0 set-tier " LN+ ;

: LN$ ( -- ptr u8 n ) LN-BUF LN-U @ ;

: QUOT$ ( n n -- ptr u8 n )
   TIER-OPEN
   s" : " LN+ NAME+ s"  ( n -- n ) [: 1 + ;] execute 2 * ; 20 " LN+ NAME+
   s"  . cr" LN+ LN$ ;

: DOES$ ( n n -- ptr u8 n )
   TIER-OPEN
   s" : " LN+ NAME+ s"  ( n -- n ) dup create , [: 1 + ;] execute does> ( -- n )" LN+
   s"  [: @ 3 + ;] execute ; 5 " LN+ NAME+ s"  LN-CHILD . LN-CHILD . cr" LN+ LN$ ;

: TRUST$ ( n n -- ptr u8 n )
   TIER-OPEN
   s" TRUSTED: " LN+ NAME+ s"  ( -- n ) 7 ; " LN+ NAME+ s"  . cr" LN+ LN$ ;

\ A TRUSTED: body whose row carries a two-cell value. Tier 0 lowers it from the
\ certificate of a scan that enforces nothing, whatever its verdict; without
\ that certificate the pair moves one cell wide and its cells come back out of
\ order.
: WIDE-TRUST$ ( n -- ptr u8 n )
   0 TIER-OPEN
   s" STRUCTURE twpair FIELD a n FIELD b n ;STRUCTURE " LN+
   s" TRUSTED: TW-TB ( n -- twpair n ) 1 2 TWPAIR:MAKE swap ; " LN+
   s" : TW-SHOW ( -- ) 7 TW-TB . TWPAIR:UNMAKE . . cr ; TW-SHOW" LN+ LN$ ;

\ TRUSTED: bodies whose `execute` and `catch` reach under the declared inputs
\ while a two-cell value is live. The refusal settles the token's rows as a
\ call's does, so the scan keeps its facts and the words run.
: BORROW-TRUST$ ( n -- ptr u8 n )
   0 TIER-OPEN
   s" STRUCTURE bwpair FIELD a n FIELD b n ;STRUCTURE " LN+
   s" : BW-P3 ( n n bwpair n -- n n bwpair n ) ; " LN+
   s" TRUSTED: BW-X ( n -- n bwpair n ) 1 2 BWPAIR:MAKE 5 ['] BW-P3 execute ; " LN+
   s" TRUSTED: BW-C ( n -- n bwpair n n ) 1 2 BWPAIR:MAKE 5 ['] BW-P3 catch ; " LN+
   s" : BW-SHOW ( -- ) 9 7 BW-X . BWPAIR:UNMAKE . . . .  9 7 BW-C . . BWPAIR:UNMAKE . . . . cr ; BW-SHOW" LN+
   LN$ ;

\ TRUSTED: bodies whose scans refuse a token while a two-cell value is live:
\ a ptr-times-zero before the pair moves, after it, inside a quotation, over a
\ callee's pair, in a branch that exits, `depth`, `over` the pair, two
\ refusals before the pair binds a local, a ptr bound into an `n` local
\ over the pair, unread and read, and under it, and a pair-typed local bound
\ beside a refused `n` local, from two loose cells, and under a refused `n`
\ local, an undeclared local beside a refused `n` local, a pair-typed local
\ under an undeclared one, a linear local over the pair, a rejected call to a
\ word whose row holds a naked type variable (RJ-GX), and a pair an `execute`d
\ quotation returns (RJ-EX). The scan applies
\ each refused token's declared effect to the rows it found, so the widths
\ after it stay stated and both tiers move the pair whole. The bodies are
\ independent: one process runs them all.
: REJECT-TRUST$ ( n -- ptr u8 n )
   0 TIER-OPEN
   s" STRUCTURE rjpair FIELD a n FIELD b n ;STRUCTURE " LN+
   S\" TRUSTED: RJ-P15 ( n -- rjpair n ) 1 2 RJPAIR:MAKE s\" x\" drop 0 * drop swap ; " LN+
   S\" TRUSTED: RJ-R3 ( n -- rjpair n ) 1 2 RJPAIR:MAKE [: s\" x\" drop 0 * drop ;] execute swap ; " LN+
   s" TRUSTED: RJ-INNER ( -- rjpair ) 1 2 RJPAIR:MAKE ; " LN+
   S\" TRUSTED: RJ-R4 ( n -- rjpair n ) RJ-INNER s\" x\" drop 0 * drop swap ; " LN+
   S\" TRUSTED: RJ-R5 ( n -- rjpair n ) dup if 1 2 RJPAIR:MAKE s\" x\" drop 0 * drop swap exit then " LN+
   s" 3 4 RJPAIR:MAKE swap ; " LN+
   s" TRUSTED: RJ-P09 ( n -- rjpair n ) 1 2 RJPAIR:MAKE depth drop swap ; " LN+
   S\" TRUSTED: RJ-P01 ( n -- rjpair n ) 1 2 RJPAIR:MAKE swap s\" x\" drop 0 * + ; " LN+
   S\" TRUSTED: RJ-AFTER ( n -- rjpair n ) s\" x\" drop 0 * drop 1 2 RJPAIR:MAKE swap ; " LN+
   s" TRUSTED: RJ-IF ( n -- rjpair n ) 1 2 RJPAIR:MAKE over if swap exit then swap ; " LN+
   S\" TRUSTED: RJ-LOCAL ( n -- rjpair n ) 1 2 RJPAIR:MAKE s\" x\" drop 0 * drop " LN+
   S\" s\" y\" drop 0 * drop {: p :} p swap ; " LN+
   S\" TRUSTED: RJ-Q2 ( -- n ) 1 2 RJPAIR:MAKE 5 s\" x\" drop 0 * drop swap drop ; " LN+
   S\" TRUSTED: RJ-BS1 ( n -- rjpair n ) 1 2 RJPAIR:MAKE s\" x\" drop {: q:n :} swap ; " LN+
   S\" TRUSTED: RJ-BS2 ( n -- rjpair n ) 1 2 RJPAIR:MAKE s\" x\" drop {: q:n :} q drop swap ; " LN+
   S\" TRUSTED: RJ-BS3 ( n -- n rjpair ) s\" x\" drop {: q:n :} 1 2 RJPAIR:MAKE q drop ; " LN+
   S\" TRUSTED: RJ-LP1 ( n -- n rjpair ) 1 2 RJPAIR:MAKE s\" x\" drop {: p:rjpair q:n :} p ; " LN+
   s" TRUSTED: RJ-LP2 ( -- rjpair ) 1 2 {: p:rjpair :} p ; " LN+
   S\" TRUSTED: RJ-LP3 ( n -- n rjpair ) s\" x\" drop 1 2 RJPAIR:MAKE {: q:n p:rjpair :} p ; " LN+
   S\" TRUSTED: RJ-UB1 ( -- n rjpair ) s\" x\" drop 5 {: q:n p :} 1 2 RJPAIR:MAKE p swap ; " LN+
   S\" TRUSTED: RJ-UB2 ( n -- n rjpair ) s\" x\" drop 1 2 RJPAIR:MAKE 5 {: q:n p:rjpair r :} p ; " LN+
   s" package RJLP public DEFLINEAR RJLP:own ;package " LN+
   s" TRUSTED: RJ-MK-OWN ( n -- RJLP:own ) ;  TRUSTED: RJ-FREE-OWN ( RJLP:own -- n ) ; " LN+
   s" TRUSTED: RJ-LL1 ( n -- rjpair n ) 1 2 RJPAIR:MAKE 5 RJ-MK-OWN {: x:RJLP:own :} x RJ-FREE-OWN drop swap ; " LN+
   s" : RJ-GN ( a bool -- bool a ) swap ;  TRUSTED: RJ-GX ( -- n n ) 1 2 RJ-GN ; " LN+
   S\" TRUSTED: RJ-EX ( n -- rjpair n ) [: 1 2 RJPAIR:MAKE ;] execute s\" x\" drop 0 * drop swap ; " LN+
   s" : RJ-ALL ( -- ) 7 RJ-P15 . RJPAIR:UNMAKE . .  7 RJ-R3 . RJPAIR:UNMAKE . . " LN+
   s" 7 RJ-R4 . RJPAIR:UNMAKE . .  7 RJ-R5 . RJPAIR:UNMAKE . . " LN+
   s" 7 RJ-P09 . RJPAIR:UNMAKE . .  7 RJ-P01 . RJPAIR:UNMAKE . . " LN+
   s" 7 RJ-AFTER . RJPAIR:UNMAKE . .  7 RJ-IF . RJPAIR:UNMAKE . . " LN+
   s" 0 RJ-IF . RJPAIR:UNMAKE . .  7 RJ-LOCAL . RJPAIR:UNMAKE . .  RJ-Q2 . " LN+
   s" 7 RJ-BS1 . RJPAIR:UNMAKE . .  7 RJ-BS2 . RJPAIR:UNMAKE . . " LN+
   s" 7 RJ-BS3 RJPAIR:UNMAKE . . .  7 RJ-LP1 RJPAIR:UNMAKE . . .  RJ-LP2 RJPAIR:UNMAKE . . " LN+
   s" 7 RJ-LP3 RJPAIR:UNMAKE . . .  RJ-UB1 RJPAIR:UNMAKE . . .  7 RJ-UB2 RJPAIR:UNMAKE . . . " LN+
   s" 7 RJ-LL1 . RJPAIR:UNMAKE . .  RJ-GX . .  7 RJ-EX . RJPAIR:UNMAKE . . cr ; RJ-ALL" LN+
   LN$ ;
: REJECT-TRUST-OUT$ ( -- ptr u8 n )
   S\" 7\n2\n1\n7\n2\n1\n7\n2\n1\n7\n2\n1\n7\n2\n1\n7\n2\n1\n7\n2\n1\n7\n2\n1\n0\n2\n1\n7\n2\n1\n5\n7\n2\n1\n7\n2\n1\n2\n1\n7\n2\n1\n7\n2\n1\n2\n1\n7\n2\n1\n5\n2\n1\n7\n7\n2\n1\n1\n2\n7\n2\n1\n" ;

\ TRUSTED: bodies whose scans meet a token they cannot apply while a two-cell
\ value is live: an unchecked word with no row, and `evaluate`. The widths
\ after it are unknown, the certificate says its facts are incomplete, tier 0
\ names the definition and exits before anything runs, and tier 1 refuses to
\ compile it. A body compiled under `0 set-check` is still scanned by the
\ parked hook, which gives it a row, so IC-RAW is compiled after
\ `discard-check` drops that hook, with no checker at all; the saved hook is
\ then armed afresh. Tier 0 only: tier 1 scans IC-RAW itself and gives it a
\ row, so no rowless call exists there.
: INCOMPLETE-RAW$ ( n -- ptr u8 n )
   0 TIER-OPEN
   s" check@ 0 set-check discard-check : IC-RAW swap ; set-check " LN+
   s" STRUCTURE icpair FIELD a n FIELD b n ;STRUCTURE " LN+
   s" TRUSTED: INCOMPLETE ( n -- icpair n ) 1 2 ICPAIR:MAKE IC-RAW swap ; " LN+
   s" : IC-SHOW ( -- ) 7 INCOMPLETE . ICPAIR:UNMAKE . . cr ; IC-SHOW" LN+ LN$ ;

: INCOMPLETE-EVAL$ ( n -- ptr u8 n )
   0 TIER-OPEN
   s" STRUCTURE icpair FIELD a n FIELD b n ;STRUCTURE " LN+
   S\" TRUSTED: INCOMPLETE ( n -- icpair n ) 1 2 ICPAIR:MAKE s\" \" evaluate swap ; " LN+
   s" : IC-SHOW ( -- ) 7 INCOMPLETE . ICPAIR:UNMAKE . . cr ; IC-SHOW" LN+ LN$ ;

\ A hookless body whose unjudged scan lost a width fact while a pair was live:
\ the verdict is not enforced, but the lowering still needs the facts.
: INCOMPLETE-HOOKLESS$ ( n -- ptr u8 n )
   0 TIER-OPEN
   s" STRUCTURE icpair FIELD a n FIELD b n ;STRUCTURE 0 set-check " LN+
   s" : IC-HOOKLESS ( -- ) 1 2 ICPAIR:MAKE 5 + . . cr ; IC-HOOKLESS" LN+ LN$ ;

\ A refused bind that cuts a live pair: the pair-typed `q` takes the ptr and
\ the pair's top cell, so the undeclared `p` lies over half a pair. What the
\ runtime binds into `p` is unknown, so the facts after it are incomplete.
: INCOMPLETE-BIND$ ( n -- ptr u8 n )
   0 TIER-OPEN
   s" STRUCTURE icpair FIELD a n FIELD b n ;STRUCTURE " LN+
   S\" TRUSTED: U1 ( n -- n icpair ) 1 2 ICPAIR:MAKE s\" x\" drop {: p q:icpair :} q ; " LN+
   s" : IC-SHOW ( -- ) 7 U1 ICPAIR:UNMAKE . . . cr ; IC-SHOW" LN+ LN$ ;

\ A refused bind past the known row over one-cell values: the undeclared `p`
\ binds a cell of the caller's frame, so the scan cannot read the row the
\ locals bind, yet the pair-typed `q` binds two cells. What the runtime binds
\ into `q` is unknown, so the facts after it are incomplete.
: INCOMPLETE-LOST$ ( n -- ptr u8 n )
   0 TIER-OPEN
   s" STRUCTURE icpair FIELD a n FIELD b n ;STRUCTURE " LN+
   s" TRUSTED: U2 ( n n -- ) {: p q:icpair :} p . q ICPAIR:UNMAKE . . ; " LN+
   s" TRUSTED: IC-SHOW ( -- ) 1 10 20 U2 cr ; IC-SHOW" LN+ LN$ ;

\ `--` in a bind is a local like any other name, as the engine binds it, so the
\ pair-typed `q` binds the top two cells and `--` the 9 beneath them.
: DASH-BIND$ ( n -- ptr u8 n )
   0 TIER-OPEN
   s" STRUCTURE dbpair FIELD a n FIELD b n ;STRUCTURE " LN+
   s" TRUSTED: DB-T ( n n n -- ) {: -- q:dbpair :} q DBPAIR:UNMAKE . . ; " LN+
   s" TRUSTED: DB-SHOW ( -- ) 9 1 2 DB-T cr ; DB-SHOW" LN+ LN$ ;

\ A pair-typed local after two one-cell locals binds the top two cells, so `a`
\ binds the 1 beneath them.
: PAIR-LAST$ ( n -- ptr u8 n )
   0 TIER-OPEN
   s" STRUCTURE plpair FIELD a n FIELD b n ;STRUCTURE " LN+
   s" TRUSTED: PL-T ( n n n n -- ) {: a z p:plpair :} p PLPAIR:UNMAKE . . a . ; " LN+
   s" TRUSTED: PL-SHOW ( -- ) 1 2 3 4 PL-T cr ; PL-SHOW" LN+ LN$ ;

\ A pair-typed local between one-cell locals binds the two cells between them,
\ and leaves the body whole.
: PAIR-MID$ ( n -- ptr u8 n )
   0 TIER-OPEN
   s" STRUCTURE pmpair FIELD a n FIELD b n ;STRUCTURE " LN+
   S\" TRUSTED: PM-T ( -- pmpair ) s\" x\" drop 1 2 5 {: q:n p:pmpair r :} p ; " LN+
   s" : PM-SHOW ( -- ) PM-T PMPAIR:UNMAKE . . cr ; PM-SHOW" LN+ LN$ ;

\ The whole of tier 0's refusal of an incomplete certificate, and no output.
: ASSERT-INCOMPLETE ( ptr u8 n -- ) {: want:ptr want-u:n :}
   INCOMPLETE-RC ASSERT-RC
   OUT-U @ 0 T=
   ERR$ want want-u STR= TTRUE ;

\ The first body checked: the same scan still refuses it at `*`, and nothing runs.
: REJECT-CHECKED$ ( n -- ptr u8 n )
   0 TIER-OPEN
   s" STRUCTURE rjpair FIELD a n FIELD b n ;STRUCTURE " LN+
   S\" : RJ-CHECKED ( n -- rjpair n ) 1 2 RJPAIR:MAKE s\" x\" drop 0 * drop swap ; " LN+
   s" : RJ-SHOW ( -- ) 7 RJ-CHECKED . RJPAIR:UNMAKE . . cr ; RJ-SHOW" LN+ LN$ ;

: CALL$ ( n n -- ptr u8 n )
   TIER-OPEN
   s" : " LN+ NAME+ s"  ( n -- n ) 1 + ; : LN-CALLER ( -- n ) 4 " LN+ NAME+
   s"  1 + ; LN-CALLER . cr" LN+ LN$ ;

\ The callee dies, so a tier-1 caller closes its branch behind the call with a
\ trap whose message names the callee. The branch runs only on 0.
: NORET$ ( n n -- ptr u8 n )
   TIER-OPEN
   s" : " LN+ NAME+ S\"  ( -- ) s\q died\q 3 die ; : LN-CALLER ( n -- n ) dup 0= if " LN+
   NAME+ s"  then 1 + ; 5 LN-CALLER . cr" LN+ LN$ ;

\ The pending definition is a package member, so it shadows no global.
: PRIOR$ ( n n -- ptr u8 n )
   TIER-OPEN
   s" package LN-PRIOR public : " LN+ NAME+ s"  ( n -- n ) 1 + ; ;package " LN+
   s" package LN-USER using LN-PRIOR : " LN+ NAME+ s"  ( n -- n ) " LN+ NAME+
   s"  10 * ; ;using public : LN-RUN ( n -- n ) " LN+ NAME+
   s"  ; ;package 4 LN-USER:LN-RUN . cr" LN+ LN$ ;

: VARIANT$ ( n n -- ptr u8 n )
   TIER-OPEN
   s" ENUM lnfam " LN+ TAIL+ s"  green ;ENUM : LN-E ( lnfam -- n ) MATCH lnfam " LN+
   TAIL+ s"  OF 10 ENDOF green OF 20 ENDOF ;MATCH ; LNFAM:" LN+ TAIL+
   s"  LN-E . cr" LN+ LN$ ;

: FAMILY$ ( n n -- ptr u8 n )
   TIER-OPEN
   s" ENUM " LN+ TAIL+ s"  red green ;ENUM : LN-E ( " LN+ TAIL+ s"  -- n ) MATCH " LN+
   TAIL+ s"  red OF 10 ENDOF green OF 20 ENDOF ;MATCH ; : LN-L ( " LN+ TAIL+
   s"  -- n ) {: v:" LN+ TAIL+ s"  :} 7 ; 30 . cr" LN+ LN$ ;

: UNEVEN$ ( n -- ptr u8 n )
   1 swap TIER-OPEN
   s" : TN-SHORT ( -- n ) 3 ; : " LN+ NAME+ s"  ( -- n ) ;" LN+ LN$ ;

: TRUSTED-MISSING$ ( n -- ptr u8 n )
   1 swap TIER-OPEN
   s" : TN-SHORT ( -- n ) 3 ; TRUSTED: " LN+ NAME+ s"  ( -- n ) LN-MISSING ;" LN+
   LN$ ;

\ The line a refusal of the name prints, from the name to the newline.
: REFUSED-LINE$ ( ptr u8 n n -- ptr u8 n ) {: tail:ptr tu:n len:n :}
   len LN-OPEN
   s" ncomp: cannot compile " LN+ NAME+ tail tu LN+ S\" \n" LN+ LN$ ;

: TOKEN-MISSING$ ( n -- ptr u8 n )
   1 swap TIER-OPEN
   s" TRUSTED: LN-TOK ( -- n ) " LN+ NAME+ s"  ;" LN+ LN$ ;

\ The line a refusal at that token prints, from the definition to the newline.
: TOKEN-LINE$ ( n -- ptr u8 n )
   LN-OPEN
   s" ncomp: cannot compile LN-TOK at " LN+ NAME+ S\" \n" LN+ LN$ ;

: TEST-LONG-NAMES ( -- )
   LEGACY? if
      s" a 65-byte name with a quotation runs on tier 0" T-LABEL
      0 65 QUOT$ RUN  s" 42" ASSERT-OK
   then
   s" a 65-byte name with a quotation runs on tier 1" T-LABEL
   1 65 QUOT$ RUN  s" 42" ASSERT-OK
   LEGACY? if
      s" a 1000-byte name with a quotation runs on tier 0" T-LABEL
      0 1000 QUOT$ RUN  s" 42" ASSERT-OK
   then
   s" a 1000-byte name with a quotation runs on tier 1" T-LABEL
   1 1000 QUOT$ RUN  s" 42" ASSERT-OK
   LEGACY? if
      s" a 7900-byte name with a quotation runs on tier 0" T-LABEL
      0 7900 QUOT$ RUN  s" 42" ASSERT-OK
   then
   s" a 7900-byte name with a quotation runs on tier 1" T-LABEL
   1 7900 QUOT$ RUN  s" 42" ASSERT-OK

   LEGACY? if
      s" a 7900-byte does> definer runs on tier 0" T-LABEL
      0 7900 DOES$ RUN  S\" 6\n8\n" ASSERT-OK
   then
   s" a 7900-byte does> definer runs on tier 1" T-LABEL
   1 7900 DOES$ RUN  S\" 6\n8\n" ASSERT-OK

   LEGACY? if
      s" a 7900-byte TRUSTED: name runs on tier 0" T-LABEL
      0 7900 TRUST$ RUN  s" 7" ASSERT-OK
   then
   s" a 7900-byte TRUSTED: name runs on tier 1" T-LABEL
   1 7900 TRUST$ RUN  s" 7" ASSERT-OK

   LEGACY? if
      s" a TRUSTED: body moves a two-cell value on tier 0" T-LABEL
      0 WIDE-TRUST$ RUN  S\" 7\n2\n1\n" ASSERT-OK
      s" rejected TRUSTED: bodies move two-cell values on tier 0" T-LABEL
      0 REJECT-TRUST$ RUN  REJECT-TRUST-OUT$ ASSERT-OK
      s" TRUSTED: execute and catch reaching under their inputs keep their facts on tier 0" T-LABEL
      0 BORROW-TRUST$ RUN  S\" 5\n2\n1\n7\n9\n0\n5\n2\n1\n7\n9\n" ASSERT-OK
      s" the checked twin is refused at its token on tier 0" T-LABEL
      0 REJECT-CHECKED$ RUN  s" rj-checked: at '*'" ASSERT-REJECTED  OUT-U @ 0 T=
      \ One process each: the refusal ends the process it happens in.
      s" an unchecked word over a two-cell value is refused by name on tier 0" T-LABEL
      0 INCOMPLETE-RAW$ RUN  S\" hb: incomplete layout facts: INCOMPLETE\n" ASSERT-INCOMPLETE
      s" evaluate over a two-cell value is refused by name on tier 0" T-LABEL
      0 INCOMPLETE-EVAL$ RUN  S\" hb: incomplete layout facts: INCOMPLETE\n" ASSERT-INCOMPLETE
      s" a refused bind leaving an undeclared local over half a pair is refused by name on tier 0" T-LABEL
      0 INCOMPLETE-BIND$ RUN  S\" hb: incomplete layout facts: U1\n" ASSERT-INCOMPLETE
      s" a refused bind past the known row with a pair-typed local is refused by name on tier 0" T-LABEL
      0 INCOMPLETE-LOST$ RUN  S\" hb: incomplete layout facts: U2\n" ASSERT-INCOMPLETE
      s" a hookless body with incomplete width facts is refused by name on tier 0" T-LABEL
      0 INCOMPLETE-HOOKLESS$ RUN  S\" hb: incomplete layout facts: IC-HOOKLESS\n" ASSERT-INCOMPLETE
      s" a `--` in a TRUSTED: bind is bound as a local on tier 0" T-LABEL
      0 DASH-BIND$ RUN  S\" 2\n1\n" ASSERT-OK
      s" a TRUSTED: pair local after one-cell locals binds the top cells on tier 0" T-LABEL
      0 PAIR-LAST$ RUN  S\" 4\n3\n1\n" ASSERT-OK
      s" a TRUSTED: pair local between one-cell locals binds its cells on tier 0" T-LABEL
      0 PAIR-MID$ RUN  S\" 2\n1\n" ASSERT-OK
   then
   s" a TRUSTED: body moves a two-cell value on tier 1" T-LABEL
   1 WIDE-TRUST$ RUN  S\" 7\n2\n1\n" ASSERT-OK
   s" a `--` in a TRUSTED: bind is bound as a local on tier 1" T-LABEL
   1 DASH-BIND$ RUN  S\" 2\n1\n" ASSERT-OK
   s" a TRUSTED: pair local after one-cell locals binds the top cells on tier 1" T-LABEL
   1 PAIR-LAST$ RUN  S\" 4\n3\n1\n" ASSERT-OK
   s" a TRUSTED: pair local between one-cell locals binds its cells on tier 1" T-LABEL
   1 PAIR-MID$ RUN  S\" 2\n1\n" ASSERT-OK
   s" rejected TRUSTED: bodies move two-cell values on tier 1" T-LABEL
   1 REJECT-TRUST$ RUN  REJECT-TRUST-OUT$ ASSERT-OK
   s" the checked twin is refused at its token on tier 1" T-LABEL
   1 REJECT-CHECKED$ RUN  s" rj-checked: at '*'" ASSERT-REJECTED  OUT-U @ 0 T=
   s" evaluate over a two-cell value is refused on tier 1" T-LABEL
   1 INCOMPLETE-EVAL$ RUN  UNCAUGHT-RC ASSERT-RC  OUT-U @ 0 T=
   ERR$ S\" ncomp: cannot compile INCOMPLETE\n" CONTAINS? TTRUE
   ERR$ E-NELAB-BUNDLE UNCAUGHT$ CONTAINS? TTRUE
   s" a hookless body with incomplete width facts is refused on tier 1" T-LABEL
   1 INCOMPLETE-HOOKLESS$ RUN  UNCAUGHT-RC ASSERT-RC  OUT-U @ 0 T=
   ERR$ S\" ncomp: cannot compile IC-HOOKLESS\n" CONTAINS? TTRUE
   ERR$ E-NELAB-BUNDLE UNCAUGHT$ CONTAINS? TTRUE
   s" a refused bind leaving an undeclared local over half a pair is refused by name on tier 1" T-LABEL
   1 INCOMPLETE-BIND$ RUN  UNCAUGHT-RC ASSERT-RC  OUT-U @ 0 T=
   ERR$ S\" ncomp: cannot compile U1\n" CONTAINS? TTRUE
   ERR$ E-NELAB-BUNDLE UNCAUGHT$ CONTAINS? TTRUE
   s" a refused bind past the known row with a pair-typed local is refused by name on tier 1" T-LABEL
   1 INCOMPLETE-LOST$ RUN  UNCAUGHT-RC ASSERT-RC  OUT-U @ 0 T=
   ERR$ S\" ncomp: cannot compile U2\n" CONTAINS? TTRUE
   ERR$ E-NELAB-BUNDLE UNCAUGHT$ CONTAINS? TTRUE

   LEGACY? if
      s" a caller of a 7900-byte name runs on tier 0" T-LABEL
      0 7900 CALL$ RUN  s" 6" ASSERT-OK
   then
   s" a caller of a 7900-byte name runs on tier 1" T-LABEL
   1 7900 CALL$ RUN  s" 6" ASSERT-OK

   LEGACY? if
      s" a caller of a 7900-byte word that never returns runs on tier 0" T-LABEL
      0 7900 NORET$ RUN  s" 6" ASSERT-OK
   then
   s" a caller of a 7900-byte word that never returns runs on tier 1" T-LABEL
   1 7900 NORET$ RUN  s" 6" ASSERT-OK

   LEGACY? if
      s" a 1000-byte pending name calls its prior word on tier 0" T-LABEL
      0 1000 PRIOR$ RUN  s" 50" ASSERT-OK
   then
   s" a 1000-byte pending name calls its prior word on tier 1" T-LABEL
   1 1000 PRIOR$ RUN  s" 50" ASSERT-OK

   LEGACY? if
      s" the longest ENUM variant constructs and matches on tier 0" T-LABEL
      0 LN-TAIL-MAX VARIANT$ RUN  s" 10" ASSERT-OK
   then
   s" the longest ENUM variant constructs and matches on tier 1" T-LABEL
   1 LN-TAIL-MAX VARIANT$ RUN  s" 10" ASSERT-OK

   LEGACY? if
      s" the longest ENUM family, its MATCH and its local compile on tier 0" T-LABEL
      0 LN-TAIL-MAX FAMILY$ RUN  s" 30" ASSERT-OK
   then
   s" the longest ENUM family, its MATCH and its local compile on tier 1" T-LABEL
   1 LN-TAIL-MAX FAMILY$ RUN  s" 30" ASSERT-OK

   s" a refused 1000-byte name is reported whole" T-LABEL
   1000 UNEVEN$ RUN  REJECT-RC ASSERT-RC
   ERR$ s" " 1000 REFUSED-LINE$ CONTAINS? TTRUE

   s" a refused 1000-byte TRUSTED: name retracts its signature" T-LABEL
   1000 TRUSTED-MISSING$ RUN  UNCAUGHT-RC ASSERT-RC
   ERR$ s"  at LN-MISSING" 1000 REFUSED-LINE$ CONTAINS? TTRUE
   ERR$ E-HIR-UNMODELED UNCAUGHT$ CONTAINS? TTRUE

   s" a refusal at a 7900-byte body token names the token whole" T-LABEL
   7900 TOKEN-MISSING$ RUN  UNCAUGHT-RC ASSERT-RC
   ERR$ 7900 TOKEN-LINE$ CONTAINS? TTRUE ;

\ ---- 2d. a refusal line names only its own token ------------------------------
\ The elaborator names the token it refused at. A later definition refused
\ before elaboration has no such token, and its line must not carry the earlier
\ one's.
: TEST-REFUSAL-TOKEN ( -- )
   s" a refusal line carries no earlier definition's token" T-LABEL
   S\" 1 set-tier\nTRUSTED: TN-EV ( ptr u8 n -- ) evaluate ;\n: TN-TRY ( -- ) [: s\q TRUSTED: TN-ELAB ( -- n ) TN-ELAB-MISSING ;\q TN-EV ;] catch . cr ;\nTN-TRY\n: TN-UNEVEN ( -- n ) ;\n"
   RUN  REJECT-RC ASSERT-RC
   ERR$ S\" ncomp: cannot compile TN-ELAB at TN-ELAB-MISSING\n" CONTAINS? TTRUE
   ERR$ S\" ncomp: cannot compile TN-UNEVEN\n" CONTAINS? TTRUE ;

\ ---- 3. tier 0 runs the constructs its closure owns --------------------------
\ A definer whose clause breaks its declared effect. Each tier reports the
\ checker's reason under the clause name and exits 70.
: MKW-BAD$ ( -- ptr u8 n )
   s" : MKW ( n -- ) create , does> ( -- n n ) @ ;" ;

\ A definer whose package holds a private word named `check-does!` that would
\ approve any clause, printing SHADOW.
: CDS-SHADOW$ ( -- ptr u8 n )
   s\" package CDS private : check-does! ( ptr u8 n ptr u8 n -- n ) 2drop 2drop .\q SHADOW\q cr -1 ; public : MK ( n -- ) create , does> ( -- n ) @ 1 + ; ;package 7 CDS:MK W : SHOW ( -- ) W . cr ; SHOW" ;

: TEST-TIER0-CONSTRUCTS ( -- )
   LEGACY? if
      s" tier 0 compiles a quotation" T-LABEL
      s" : Q ( -- n ) [: 7 ;] execute ; Q . cr" RUN  s" 7" ASSERT-OK

      s" tier 0 compiles BEGIN / UNTIL" T-LABEL
      s" : B1 ( -- n ) 0 begin 1 + dup 5 >= until ; B1 . cr" RUN  s" 5" ASSERT-OK

      s" tier 0 compiles BEGIN / WHILE / REPEAT" T-LABEL
      s" : B2 ( -- n ) 0 begin dup 5 < while 1 + repeat ; B2 . cr" RUN  s" 5" ASSERT-OK

      s" tier 0 compiles DO / LOOP" T-LABEL
      s" : B3 ( -- n ) 0 5 0 do 1 + loop ; B3 . cr" RUN  s" 5" ASSERT-OK

      \ The JIT's frame sequences must not touch pooled loop registers.
      s" tier 0 keeps loop values across a local bound in the body" T-LABEL
      s" : B4 ( n -- n ) {: u:n :} 0 0 begin dup u < while dup 3 + {: end:n :} drop 1+ end 1+ repeat drop ; 10 B4 . cr" RUN  s" 3" ASSERT-OK

      \ The head's frame is dropped once, on its own exit: the clause runs with
      \ none, and the shared `;` drops nothing of the head's.
      s" tier 0 runs a does> definer whose head binds a local" T-LABEL
      s" : MKV ( n -- ) {: v:n :} create v , does> ( -- n ) @ 1 + ; 7 MKV W : SHOW ( -- ) W . cr ; SHOW" RUN  s" 8" ASSERT-OK

      s" tier 0 refuses a does> clause that names a head local" T-LABEL
      s" : MKV ( n -- ) {: v:n :} create v , does> ( -- n ) drop v ;" RUN  REJECT-RC ASSERT-RC
      ERR$ s" E-UNDEFINED: v" CONTAINS? TTRUE

      \ A head with no width facts and a clause that moves a two-cell value,
      \ on the stack and through a clause local: the clause lowers from its own
      \ certificate. Lowered cell by cell, the pair came back as 1 2 7.
      s" tier 0 lowers a does> clause's width facts" T-LABEL
      s" STRUCTURE pr FIELD a n FIELD b n ;STRUCTURE : MK ( n -- ) create , does> ( -- pr n ) @ 1 2 PR:MAKE swap ; 7 MK W : SHOW1 ( -- ) W . PR:UNMAKE . . cr ; SHOW1 : MKL ( n -- ) create , does> ( -- n pr n ) @ 1 2 PR:MAKE {: x:n p:pr :} 3 p x ; 7 MKL WL : SHOW2 ( -- ) WL . PR:UNMAKE . . . cr ; SHOW2"
      RUN  S\" 7\n2\n1\n\n7\n2\n1\n3\n" ASSERT-OK

      \ A TRUSTED: head that binds a two-cell local inside an `if` closed before
      \ does>, and a clause that moves a pair: each span lowers from its own
      \ certificate. Lowered from the head's alone, the pair came back as 1 2 10.
      s" tier 0 lowers a TRUSTED: definer whose head binds locals in a branch" T-LABEL
      s" STRUCTURE pr FIELD a n FIELD b n ;STRUCTURE TRUSTED: MKW ( n -- ) dup 0 > if 1 2 PR:MAKE {: p:pr :} p PR:UNMAKE + + else 1 - then create , does> ( -- pr n ) @ 1 2 PR:MAKE swap ; 7 MKW WW : SHOWW ( -- ) WW . PR:UNMAKE . . cr ; SHOWW"
      RUN  S\" 10\n2\n1\n" ASSERT-OK

      \ The clause check is the source owner's, reached through its record: a
      \ word of the same name in the definer's package is not called for it.
      s" tier 0 checks a does> clause with the owner's check, not a package's" T-LABEL
      CDS-SHADOW$ RUN  s" 8" ASSERT-OK
      OUT$ s" SHADOW" CONTAINS? TFALSE

      s" tier 0 refuses a does> clause whose body breaks its effect" T-LABEL
      MKW-BAD$ RUN  s" mkw;does" ASSERT-REJECTED
   then

   s" tier 1 checks a does> clause with the owner's check, not a package's" T-LABEL
   SB-RESET s" 1 set-tier " SB-APPEND CDS-SHADOW$ SB-APPEND SB$ RUN  s" 8" ASSERT-OK
   OUT$ s" SHADOW" CONTAINS? TFALSE

   \ Tier 0's reader refuses the token before the checker sees the clause; tier
   \ 1 compiles the whole definition and the checker refuses the clause, which
   \ reports as a refused body does: the reason, naming the definer, and 70.
   s" tier 1 refuses a does> clause that names a head local" T-LABEL
   s" 1 set-tier : MKV ( n -- ) {: v:n :} create v , does> ( -- n ) drop v ;" RUN  s" mkv;does" ASSERT-REJECTED
   ERR$ s" undefined word 'v'" CONTAINS? TTRUE

   \ A load stops at its first refused clause, so a second refusal needs its own run.
   s" tier 1 refuses a does> clause whose body breaks its effect" T-LABEL
   SB-RESET s" 1 set-tier " SB-APPEND MKW-BAD$ SB-APPEND SB$ RUN  s" mkw;does" ASSERT-REJECTED

   s" tier 1 keeps loop values across a local bound in the body" T-LABEL
   s" 1 set-tier : B4 ( n -- n ) {: u:n :} 0 0 begin dup u < while dup 3 + {: end:n :} drop 1+ end 1+ repeat drop ; 10 B4 . cr" RUN  s" 3" ASSERT-OK ;

\ ---- 4. the snapshot depth dies with the definition --------------------------
\ THE REGRESSION. Three definitions that each open a BEGIN and then fail on an
\ undefined word, caught one at a time by `TEST-EVAL:RC`; then the depth cell,
\ which a missed recovery reset leaves non-zero, and one good BEGIN
\ definition that has to compile and run.
: LEAK-SRC$ ( -- ptr u8 n )
   S\" : X3 ( -- ) 3 0 do s\q : BAD-B ( -- ) begin MISSING ;\q TEST-EVAL:RC drop loop ;\nX3\ns\q depth \q type JIT-SNAP:SP-CELL data-base + @ . cr\n: GOOD ( -- n ) 0 begin 1 + dup 5 >= until ;\nGOOD . cr\n" ;

: TEST-SNAPSHOT-OWNERSHIP ( -- )
   s" failed BEGIN definitions leave the depth clean" T-LABEL
   LEAK-SRC$ RUN  s" depth 0" ASSERT-OK  s" 5" ASSERT-OK

   s" one failed BEGIN definition is caught, not fatal" T-LABEL
   S\" s\q : BAD-B ( -- ) begin MISSING ;\q TEST-EVAL:RC . cr\n"
   RUN  s" 70" ASSERT-OK ;

\ ---- 5. the reset stops at the definition boundary ---------------------------
\ A quotation is compiled inline, so these share one frame stack with their
\ enclosing definition and must still balance.
: TEST-NESTED-QUOTATIONS ( -- )
   LEGACY? if
      s" a quotation inside BEGIN keeps its frames" T-LABEL
      s" : N1 ( -- n ) 0 begin [: 1 ;] execute + dup 5 >= until ; N1 . cr"
      RUN  s" 5" ASSERT-OK

      s" a BEGIN inside a quotation keeps its frames" T-LABEL
      s" : N2 ( -- n ) [: 0 begin 1 + dup 4 >= until ;] execute ; N2 . cr"
      RUN  s" 4" ASSERT-OK

      s" BEGIN inside a quotation inside BEGIN keeps its frames" T-LABEL
      s" : N3 ( -- n ) 0 begin [: 0 begin 1 + dup 2 >= until ;] execute + dup 6 >= until ; N3 . cr"
      RUN  s" 6" ASSERT-OK
   then

   s" the same nesting compiles on tier 1" T-LABEL
   s" 1 set-tier : N4 ( -- n ) 0 begin [: 1 ;] execute + dup 5 >= until ; N4 . cr"
   RUN  s" 5" ASSERT-OK ;

\ ---- 7. a failed definition under a top-level catch is recoverable ----------
\ THE CRASH. `' evaluate catch` at top level installs its handler at exactly the
\ SP that B-EVAL later records as the eval-frame boundary. The recovery treated
\ equal as "handler inside the frame", never popped the frame, and left CP and
\ the protection band at the abandoned definition -- so the next token was
\ emitted into a re-protected band and the process died of SIGSEGV (rc 134)
\ AFTER printing a correct rejection. These run as real processes because the
\ in-process fork does not reach that boundary. A top-level `catch` of
\ `evaluate` has no row the checker can follow, so each text saves the checker
\ (`check@`), turns it off for the top-level `catch`, restores it inside the
\ evaluated text before the failing definition, and then drops the text's two
\ cells under `0 set-check` before putting the checker back.
: TEST-EVAL-RECOVERY ( -- )
   LEGACY? if
      s" tier 0 recovers from a rejected effect under a top-level catch" T-LABEL
      S\" check@ 0 set-check s\q dup set-check : RVND ( n -- n ) dup ;\q ' evaluate catch 0 set-check . 2drop set-check cr\n" EXEC  s" 70" ASSERT-OK

      s" tier 0 recovers from an undefined word under a top-level catch" T-LABEL
      S\" check@ 0 set-check s\q dup set-check : RVU ( -- ) MISSINGW ;\q ' evaluate catch 0 set-check . 2drop set-check cr\n" EXEC  s" 70" ASSERT-OK
   then

   s" tier 1 recovers from the same rejection" T-LABEL
   S\" 1 set-tier check@ 0 set-check s\q dup set-check : RVND ( n -- n ) dup ;\q ' evaluate catch 0 set-check . 2drop set-check cr\n" EXEC
   s" 70" ASSERT-OK

   s" the session keeps compiling after the recovery" T-LABEL
   S\" check@ 0 set-check s\q dup set-check : RVND ( n -- n ) dup ;\q ' evaluate catch 0 set-check drop 2drop set-check\n: ZKOK ( -- n ) 9 ;\nZKOK . cr\n"
   EXEC  s" 9" ASSERT-OK

   s" three recoveries in a row still leave a usable session" T-LABEL
   S\" check@ 0 set-check s\q dup set-check : RA ( n -- n ) dup ;\q ' evaluate catch 0 set-check drop 2drop set-check\ncheck@ 0 set-check s\q dup set-check : RB ( n -- n ) dup ;\q ' evaluate catch 0 set-check drop 2drop set-check\ncheck@ 0 set-check s\q dup set-check : RC ( n -- n ) dup ;\q ' evaluate catch 0 set-check drop 2drop set-check\n: ZKOK2 ( -- n ) 7 ;\nZKOK2 . cr\n"
   EXEC  s" 7" ASSERT-OK

   s" the quotation form still recovers" T-LABEL
   S\" PTR-VARIABLE EV-A variable EV-U\nTRUSTED: EV-RUN ( -- ) EV-A @ EV-U @ evaluate ;\n: EV ( ptr u8 n -- n ) EV-U ! EV-A ! [: EV-RUN ;] catch ;\ns\q : RVND ( n -- n ) dup ;\q EV . cr\n"
   EXEC  s" 70" ASSERT-OK ;

\ ---- 7b. a refused TRUSTED: declaration that records no row ------------------
\ Its signature does not parse, so the definition recorded no row and its
\ retract cuts nothing (src/compiler/native/definition.f RETRACT): the refusal's
\ own code goes uncaught, and as the checker rendered it the engine exits as a
\ refusal.
: TEST-TRUSTED-NO-ROW ( -- )
   s" a tier-1 TRUSTED: declaration that records no row leaves its own code uncaught" T-LABEL
   s" 1 set-tier TRUSTED: TB-BAD ( n -- no-such-type ) drop drop ;" RUN
   REJECT-RC ASSERT-RC
   ERR$ E-BAD-STORED-SIGNATURE UNCAUGHT$ CONTAINS? TTRUE ;

\ ---- 7c. checking off still lowers wide values ------------------------------
\ `0 set-check` empties the hook cell: the verdict is not enforced, but each
\ tier still scans a body unjudged, signed or not, and a definer's signed does>
\ clause, to learn their cell widths. A tier that compiled the body one cell
\ per value moved the pair as 1 2 7. The clause scan leaves no latch for a
\ later word: a tier whose publication kept it recorded the next checked
\ definitions as creating the clause's word, and FOO and W printed -1.
: UW-TIER ( n -- ) {: tier:n :}
   0 LN-OPEN
   tier 1 = if s" 1 set-tier " LN+ else s" 0 set-tier " LN+ then
   s" STRUCTURE pr FIELD a n FIELD b n ;STRUCTURE " LN+ ;

: UW-OPEN ( n -- ) UW-TIER s" 0 set-check " LN+ ;

: UW-PROGRAM$ ( n ptr u8 n -- ptr u8 n ) {: tier:n head:ptr hu:n :}
   tier UW-OPEN
   head hu LN+
   s"  1 2 PR:MAKE swap ; : SHOW ( -- ) 7 TB . PR:UNMAKE . . cr ; SHOW" LN+
   LN$ ;

: UW-CASE ( n ptr u8 n -- ) {: tier:n head:ptr hu:n :}
   tier head hu UW-PROGRAM$ RUN  S\" 7\n2\n1\n" ASSERT-OK ;

: UW-DOES-CASE ( n -- )
   UW-OPEN
   s" : MKP ( -- ) create does> ( n -- pr n ) drop 1 2 PR:MAKE swap ; MKP TB" LN+
   s"  : SHOW ( -- ) 7 TB . PR:UNMAKE . . cr ; SHOW" LN+
   LN$ RUN  S\" 7\n2\n1\n" ASSERT-OK ;

: UW-LATCH-CASE ( n -- )
   UW-TIER
   s" TRUSTED: CR? ( ptr u8 n -- n ) CHECKER-FIND-ACTIVE-SYM CHECKER-CREATES-SYM? 0<> ; " LN+
   s" check@ 0 set-check : MKP ( -- ) create does> ( n -- pr n ) drop 1 2 PR:MAKE swap ; " LN+
   s" : OTHER ( -- n ) 3 ; set-check : FOO ( -- ) ; : W ( -- ) FOO ; " LN+
   S\" s\" FOO\" CR? . s\" W\" CR? . s\" MKP\" CR? . s\" OTHER\" CR? . cr" LN+
   LN$ RUN  S\" 0\n0\n0\n0\n" ASSERT-OK ;

: TEST-UNCHECKED-WIDTHS ( -- )
   LEGACY? if
      s" tier 0 lowers a wide value in an unchecked : body" T-LABEL
      0 s" : TB ( n -- pr n )" UW-CASE
      s" tier 0 lowers a wide value in an unchecked TRUSTED: body" T-LABEL
      0 s" TRUSTED: TB ( n -- pr n )" UW-CASE
      s" tier 0 lowers a wide value in an unchecked signature-less body" T-LABEL
      0 s" : TB" UW-CASE
      s" tier 0 lowers a wide value in an unchecked does> clause" T-LABEL
      0 UW-DOES-CASE
      s" tier 0 leaves no unchecked does> clause latch for a later word" T-LABEL
      0 UW-LATCH-CASE
   then
   s" tier 1 lowers a wide value in an unchecked : body" T-LABEL
   1 s" : TB ( n -- pr n )" UW-CASE
   s" tier 1 lowers a wide value in an unchecked TRUSTED: body" T-LABEL
   1 s" TRUSTED: TB ( n -- pr n )" UW-CASE
   s" tier 1 lowers a wide value in an unchecked does> clause" T-LABEL
   1 UW-DOES-CASE
   s" tier 1 leaves no unchecked does> clause latch for a later word" T-LABEL
   1 UW-LATCH-CASE ;

: TEST-BUILD-SCOPE ( -- )
   s" build scope keeps the caller's tier through nested scopes" T-LABEL
   S\" require src/lib/executable-build.f\n: BS-IN ( -- n ) [: tier@ ;] EXECUTABLE-BUILD:WITH ;\n: BS-RUN ( -- n n ) [: tier@ BS-IN ;] EXECUTABLE-BUILD:WITH ; BS-RUN . . tier@ . \n" EXEC
   0 ASSERT-RC OUT$
   LEGACY? if S\" 0\n0\n0\n" else S\" 1\n1\n1\n" then STR= TTRUE
   s" build scope preserves higher-order inputs and outputs" T-LABEL
   S\" require src/lib/executable-build.f\n: BS-RUN ( n -- n n ) [: 1+ dup ;] EXECUTABLE-BUILD:WITH ; 48 BS-RUN . . tier@ . \n" EXEC
   0 ASSERT-RC OUT$
   LEGACY? if S\" 49\n49\n0\n" else S\" 49\n49\n1\n" then STR= TTRUE
   s" the scope restores a tier its body selected, through throws too" T-LABEL
   S\" require src/lib/executable-build.f\n: BS-SEL ( -- ) [: 1 set-tier ;] EXECUTABLE-BUILD:WITH ;\n: BS-RUN ( -- ) [: 1 set-tier 79 throw ;] EXECUTABLE-BUILD:WITH ; BS-SEL tier@ . ' BS-RUN catch . tier@ . \n" EXEC
   0 ASSERT-RC OUT$
   LEGACY? if S\" 0\n79\n0\n" else S\" 1\n79\n1\n" then STR= TTRUE
   s" evaluated source inside the scope compiles at the caller's tier" T-LABEL
   S\" require src/lib/executable-build.f\nTRUSTED: BS-EVAL ( ptr u8 n -- ) evaluate ;\n: BS-RUN ( -- ) [: s\q tier@ . : BS-NATIVE ( -- n ) 61 ;\q BS-EVAL ;] EXECUTABLE-BUILD:WITH ; BS-RUN BS-NATIVE . tier@ .\n" EXEC
   0 ASSERT-RC OUT$
   LEGACY? if S\" 0\n61\n0\n" else S\" 1\n61\n1\n" then STR= TTRUE
   s" raw stores cannot disable the executable scope" T-LABEL
   s" 0 data-base NCOMP-DISPATCH:BUILD-DEPTH-CELL + !" EXEC ENGINE-ERROR:SEAL-VIOLATION ASSERT-RC
   s" ordinary source cannot leave the executable scope" T-LABEL
   s" : BS-ESCAPE ( -- ) executable-build-leave ;" EXEC REJECT-RC ASSERT-RC ;

public

: RUN-ALL ( -- )
   T-RESET
   TEST-BOTH-TIERS
   TEST-SELECTION
   TEST-TRUSTED-TICK-ORDER
   TEST-BUILD-SCOPE
   TEST-CHECKER-ROWS
   TEST-HOOK-CELL
   TEST-MARK-ROWS
   TEST-LONG-NAMES
   TEST-REFUSAL-TOKEN
   TEST-TIER0-CONSTRUCTS
   LEGACY? if TEST-SNAPSHOT-OWNERSHIP then
   TEST-NESTED-QUOTATIONS
   TEST-EVAL-RECOVERY
   TEST-TRUSTED-NO-ROW
   TEST-UNCHECKED-WIDTHS
   T-REPORT
   s" tier: ok" type cr ;

;package

TIER-TEST:RUN-ALL
