\ policy-test.f - the design seal through the real load path, at both tiers.
\
\ Every row is one child: `bin/hb --load test/policy/<harness>.f -- test/policy/<case>.f`.
\ The harness admits package PDEP (test/policy/dep.f), seals and loads the case,
\ so a case may name PDEP's public words, its own definitions and the design
\ built-ins (docs/policy.md). allow.f compiles bodies with the JIT and
\ allow-tier1.f with the native compiler; a case is run under both, because the
\ two read a body's tokens at different sites.
\
\ A child is judged by its exit code, the first line of its stderr and its whole
\ stdout, and leaves one line in build/policy-run.txt:
\    <harness> <case> rc=<n> <first stderr line>
\ with a location's path cut back to the tree-relative one, so the file reads
\ the same from any checkout.
\ Run: bin/hb --load lib/policy-test.f
require lib/test.f
require lib/string.f
require lib/adt/option.f
require lib/fmt.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-command.f

package POLICY-TEST
private

60000 constant TIMEOUT-MS
10 constant LF

PTR-VARIABLE STEM-A                    \ the harness the next child loads, as its file stem
variable STEM-U
variable CODE                          \ the last child's exit code, -1 when it did not exit

: ARTIFACT$ ( -- ptr u8 n ) s" build/policy-run.txt" ;
: DIR$ ( -- ptr u8 n ) s" test/policy/" ;

: HARNESS! ( ptr u8 n -- ) STEM-U ! STEM-A ! ;
: STEM$ ( -- ptr u8 n ) STEM-A @ STEM-U @ ;
: TIER0 ( -- ) s" allow" HARNESS! ;
: TIER1 ( -- ) s" allow-tier1" HARNESS! ;

\ ---- one child ----------------------------------------------------------------

: CODE! ( outcome -- )
   MATCH outcome
     exited OF CODE ! ENDOF
     signaled OF drop -1 CODE ! ENDOF
     timeout OF -1 CODE ! ENDOF
   ;MATCH ;

: FILE-ARG+ ( ptr u8 n -- ) {: stem:ptr su:n :}
   SB-RESET DIR$ SB-APPEND stem su SB-APPEND s" .f" SB-APPEND
   SB$ >LEN PROC-CMD:ARG+ ;

\ An empty case name loads the harness alone.
: SPAWN ( ptr u8 n -- ) {: name:ptr nu:n :}
   PROC-CMD:RESET
   s" --load" >LEN PROC-CMD:ARG+
   STEM$ FILE-ARG+
   nu 0 > if
      s" --" >LEN PROC-CMD:ARG+
      name nu FILE-ARG+
   then
   s" bin/hb" >LEN TIMEOUT-MS >MS PROC-CMD:RUN-OUTCOME CODE! ;

\ ---- what the child said ------------------------------------------------------

: ERR-LINE$ ( -- ptr u8 n )
   PROC-CMD:ERR$ {: a:ptr u:n :}
   a u LF INDEX-OF MATCH option
     none OF a u ENDOF
     some OF IDX>N a swap ENDOF
   ;MATCH ;

\ The span from its `test/policy/` on; the whole span when it has none.
: TREE-PATH ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   a u DIR$ FIND-SUB MATCH option
     none OF a u ENDOF
     some OF IDX>N {: at:n :} a at + u at - ENDOF
   ;MATCH ;

\ Append the first stderr line to the builder. A location is ` at <absolute
\ path>:<line>`; the path is cut back to the one the harness was given.
: SAID+ ( -- )
   ERR-LINE$ {: a:ptr u:n :}
   a u s"  at /" FIND-SUB MATCH option
     none OF a u SB-APPEND ENDOF
     some OF IDX>N 4 + {: cut:n :}
        a cut SB-APPEND
        a cut + u cut - TREE-PATH SB-APPEND ENDOF
   ;MATCH ;

\ The first `want` bytes of that line, so a row states as much of a diagnostic
\ as the seal owns: a policy line whole, a checker line by its head.
: SAID-HEAD$ ( n -- ptr u8 n ) {: want:n :}
   SB-RESET SAID+
   SB$ {: a:ptr u:n :}
   a u want min ;

: LOG ( ptr u8 n -- ) {: name:ptr nu:n :}
   SB-RESET STEM$ SB-APPEND s"  " SB-APPEND
   nu 0 > if name nu SB-APPEND else s" -" SB-APPEND then
   s"  rc=" SB-APPEND CODE @ FMT:SB-INT s"  " SB-APPEND
   SAID+ LF SB-APPEND-C
   ARTIFACT$ SB$ APPEND-FILE ;

\ ---- the verdict --------------------------------------------------------------

: LABEL ( ptr u8 n ptr u8 n -- ) {: name:ptr nu:n what:ptr wu:n :}
   SB-RESET STEM$ SB-APPEND s"  " SB-APPEND name nu SB-APPEND
   s" : " SB-APPEND what wu SB-APPEND
   SB$ T-LABEL ;

: RUNS ( ptr u8 n n ptr u8 n ptr u8 n -- )
   {: name:ptr nu:n rc:n line:ptr lu:n out:ptr ou:n :}
   name nu SPAWN
   name nu LOG
   name nu s" exit code" LABEL
   CODE @ rc T=
   name nu s" first stderr line" LABEL
   lu SAID-HEAD$ line lu T$=
   name nu s" stdout" LABEL
   PROC-CMD:OUT$ out ou T$= ;

\ The engine's one refusal: the located line, ENGINE-ERROR:POLICY, no output.
: REFUSES ( ptr u8 n ptr u8 n -- ) {: name:ptr nu:n line:ptr lu:n :}
   name nu ENGINE-ERROR:POLICY line lu s" " RUNS ;

: BOTH-REFUSE ( ptr u8 n ptr u8 n -- ) {: name:ptr nu:n line:ptr lu:n :}
   TIER0 name nu line lu REFUSES
   TIER1 name nu line lu REFUSES ;

\ ---- inside the vocabulary ----------------------------------------------------

: OK-OUT$ ( -- ptr u8 n ) S\" 42\n5\n5\n42\n0\n" ;

\ A local shadows every word, as it does unsealed: tier 1 records the names a
\ sealed `{:` declares, so it matches them before any lookup, as tier 0 does.
: LOCALS ( -- )
   TIER0 s" local-name" 0 s" " S\" 42\n" RUNS
   TIER1 s" local-name" 0 s" " S\" 42\n" RUNS
   TIER1 s" locals-many" 0 s" " S\" 0\n64\n" RUNS ;

\ The seal removes words; it does not change what an admitted program does, what
\ the checker refuses, how a throw ends the process, or the refusal of a source
\ that ends inside a definition.
: ADMITTED ( -- )
   TIER0 s" ok" 0 s" " OK-OUT$ RUNS
   TIER1 s" ok" 0 s" " OK-OUT$ RUNS
   TIER0 s" dead" 67 s" hb: uncaught throw code -30001" s" " RUNS
   TIER1 s" dead" 67 s" hb: uncaught throw code -30001" s" " RUNS
   TIER0 s" throw" 67 s" hb: uncaught throw code -30001" s" " RUNS
   TIER1 s" throw" 67 s" hb: uncaught throw code -30001" s" " RUNS
   TIER0 s" checked" 70 s" habu: in bad: at 'BAD' expected: n" s" " RUNS
   TIER1 s" checked" 70 s" habu: in bad: at 'BAD' expected: n" s" " RUNS
   TIER0 s" eof" 74 s" hb: source ended inside definition: X at test/policy/eof.f:2" s" " RUNS
   TIER1 s" eof" 74 s" hb: source ended inside definition: X at test/policy/eof.f:2" s" " RUNS
   LOCALS ;

\ ---- refused at top level -----------------------------------------------------

: LOADERS ( -- )
   s" bypass" s" hb: not in vocabulary: require at test/policy/bypass.f:2" BOTH-REFUSE
   s" included" s" hb: not in vocabulary: included at test/policy/included.f:2" BOTH-REFUSE
   s" evaluate" s" hb: not in vocabulary: evaluate at test/policy/evaluate.f:2" BOTH-REFUSE ;

: KEYWORDS ( -- )
   s" trusted" s" hb: not in vocabulary: trusted: at test/policy/trusted.f:2" BOTH-REFUSE
   s" tick" s" hb: not in vocabulary: ' at test/policy/tick.f:2" BOTH-REFUSE
   s" string" S\" hb: not in vocabulary: c\q at test/policy/string.f:2" BOTH-REFUSE ;

\ A global a used package also exports is outside the vocabulary before it is
\ shadowed: the seal refuses it ahead of the shadowing rule, as in a body.
: SCOPES ( -- )
   s" using-ffi" s" hb: not in vocabulary: NOW at test/policy/using-ffi.f:3" BOTH-REFUSE
   s" shadow" s" hb: not in vocabulary: emit at test/policy/shadow.f:3" BOTH-REFUSE
   s" private" s" hb: not in vocabulary: PDEP:HIDDEN at test/policy/private.f:2" BOTH-REFUSE
   s" reopen" s" hb: not in vocabulary: HIDDEN at test/policy/reopen.f:3" BOTH-REFUSE
   s" foreign-call" s" hb: not in vocabulary: PFOREIGN:LEAK at test/policy/foreign-call.f:2" BOTH-REFUSE
   s" undefined" s" hb: not in vocabulary: NOSUCHWORD at test/policy/undefined.f:2" BOTH-REFUSE ;

: WIDENING ( -- )
   s" allow-call" s" hb: not in vocabulary: POLICY:ALLOW at test/policy/allow-call.f:2" BOTH-REFUSE
   s" seal-call" s" hb: not in vocabulary: POLICY:SEAL at test/policy/seal-call.f:2" BOTH-REFUSE ;

\ ---- refused in a body --------------------------------------------------------

\ Tier 0 refuses at its compile rows and call lookup, tier 1 while it captures
\ the body: the same line from both.
: BODIES ( -- )
   s" capture" s" hb: not in vocabulary: require at test/policy/capture.f:2" BOTH-REFUSE
   s" execute" s" hb: not in vocabulary: execute at test/policy/execute.f:2" BOTH-REFUSE
   s" store" s" hb: not in vocabulary: ! at test/policy/store.f:2" BOTH-REFUSE
   s" arith" s" hb: not in vocabulary: + at test/policy/arith.f:2" BOTH-REFUSE
   s" dotq" S\" hb: not in vocabulary: .\q at test/policy/dotq.f:2" BOTH-REFUSE
   s" used" s" hb: not in vocabulary: LEAK at test/policy/used.f:3" BOTH-REFUSE
   s" internal" s" hb: not in vocabulary: STEP at test/policy/internal.f:6" BOTH-REFUSE ;

\ A keyword the native compiler models itself, a character literal, a call to
\ the definition being compiled and a `:}` outside a `{:` group are neither
\ records nor numbers: tier 1's capture refuses each at the token, as tier 0
\ does, before the compiler or the checker reads the body.
: MODELED ( -- )
   s" begin" s" hb: not in vocabulary: begin at test/policy/begin.f:2" BOTH-REFUSE
   s" recurse" s" hb: not in vocabulary: recurse at test/policy/recurse.f:2" BOTH-REFUSE
   s" quote" s" hb: not in vocabulary: [: at test/policy/quote.f:2" BOTH-REFUSE
   s" btick" s" hb: not in vocabulary: ['] at test/policy/btick.f:2" BOTH-REFUSE
   s" char-literal" s" hb: not in vocabulary: [char] at test/policy/char-literal.f:2" BOTH-REFUSE
   s" self" s" hb: not in vocabulary: R at test/policy/self.f:2" BOTH-REFUSE
   s" exit" s" hb: not in vocabulary: exit at test/policy/exit.f:2" BOTH-REFUSE
   s" bracket" s" hb: not in vocabulary: [ at test/policy/bracket.f:2" BOTH-REFUSE
   s" postpone" s" hb: not in vocabulary: postpone at test/policy/postpone.f:2" BOTH-REFUSE
   s" rstack" s" hb: not in vocabulary: >r at test/policy/rstack.f:2" BOTH-REFUSE
   s" do" s" hb: not in vocabulary: do at test/policy/do.f:2" BOTH-REFUSE
   s" stray-close" s" hb: not in vocabulary: :} at test/policy/stray-close.f:2" BOTH-REFUSE ;

\ ---- what a definition may be named -------------------------------------------

\ No keyword row: at tier 0 the row would answer a global name before any
\ lookup, and at tier 1 the definition. A row outside the design span is refused as a
\ token; a row inside it is the reserved name, the same refusal unsealed. The
\ pass-2 rows count too: tier 0 lowers a body holding a wide value again, and
\ they answer its tokens first. So do `:}` (in the span), `kernel:` and
\ `;match`, compared only inside their construct (habu2.f EMIT-ROW-WALK).
: NAMES ( -- )
   s" kwname" s" hb: not in vocabulary: begin at test/policy/kwname.f:2" BOTH-REFUSE
   s" own-keyword" s" hb: not in vocabulary: dup at test/policy/own-keyword.f:4" BOTH-REFUSE
   s" defname-p2" s" hb: not in vocabulary: tuck at test/policy/defname-p2.f:2" BOTH-REFUSE
   s" own-p2-keyword" s" hb: not in vocabulary: tuck at test/policy/own-p2-keyword.f:5" BOTH-REFUSE
   s" defname-interpret" s" hb: not in vocabulary: create at test/policy/defname-interpret.f:2" BOTH-REFUSE
   TIER0 s" defname-span" 70 s" hb: compile keyword cannot be a definition name: package" s" " RUNS
   TIER1 s" defname-span" 70 s" hb: compile keyword cannot be a definition name: package" s" " RUNS
   TIER0 s" defname-endloc" 70 s" hb: compile keyword cannot be a definition name: :} at test/policy/defname-endloc.f:4" s" " RUNS
   TIER1 s" defname-endloc" 70 s" hb: compile keyword cannot be a definition name: :} at test/policy/defname-endloc.f:4" s" " RUNS
   s" defname-kernel" s" hb: not in vocabulary: kernel: at test/policy/defname-kernel.f:4" BOTH-REFUSE
   s" defname-semimatch" s" hb: not in vocabulary: ;match at test/policy/defname-semimatch.f:4" BOTH-REFUSE ;

\ ---- the harness's own edges --------------------------------------------------

: HARNESSES ( -- )
   s" allow-sealed-admit" HARNESS!
   s" " ENGINE-ERROR:POLICY s" hb: policy: sealed" s" " RUNS
   s" allow-seal-twice" HARNESS!
   s" " ENGINE-ERROR:POLICY s" hb: policy: sealed" s" " RUNS
   s" allow-no-package" HARNESS!
   s" " ENGINE-ERROR:POLICY s" hb: policy: no package NOPE" s" " RUNS
   s" allow-keyword-package" HARNESS!
   s" " ENGINE-ERROR:POLICY s" hb: policy: package PKW publishes keyword DUP" s" " RUNS
   s" allow-design-keyword-package" HARNESS!
   s" " ENGINE-ERROR:POLICY s" hb: policy: package PSYNTAX publishes keyword package" s" " RUNS
   s" allow-p2-package" HARNESS!
   s" " ENGINE-ERROR:POLICY s" hb: policy: package PTUCK publishes keyword TUCK" s" " RUNS
   s" allow-catch" HARNESS!
   s" evaluate" 0 s" hb: not in vocabulary: evaluate at test/policy/evaluate.f:2" S\" 107\n" RUNS ;

\ A wordlist id at or above PROT-WID-MAX has no bit in the admission bitmap: it
\ cannot be admitted, and a record in it is never in the vocabulary.
: BOUND ( -- )
   s" allow-bound" HARNESS!
   s" " ENGINE-ERROR:POLICY s" hb: policy: package wid above the bound" s" " RUNS
   s" far" s" hb: not in vocabulary: PBIG:FAR at test/policy/far.f:2" REFUSES ;

\ A harness that bound the loaded-bytes seam to the interpret loop written in
\ Habu. The final count proves the sealed load reaches that loop.
: SEAM ( -- )
   s" allow-outer" HARNESS!
   s" ok" 0 s" " S\" 42\n5\n5\n42\n0\n1\n" RUNS
   s" bypass" s" hb: not in vocabulary: require at test/policy/bypass.f:2" REFUSES ;

public

: RUN ( -- )
   T-RESET
   s" build" MAKE-DIRS
   ARTIFACT$ s" " WRITE-ALL
   ADMITTED
   LOADERS KEYWORDS SCOPES WIDENING
   BODIES MODELED
   NAMES
   HARNESSES BOUND SEAM
   T-REPORT ;

;package

POLICY-TEST:RUN
