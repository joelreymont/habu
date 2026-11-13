\ vocabulary-test.f - the design seal through the real load path, at tier 0;
\ each case is one child: bin/hb --load <harness>.f -- <case>.f, in test/policy.
\ Run: bin/hb --load src/lib/vocabulary-test.f

\ Each child leaves one line in build/vocabulary-run.txt,
\ `<harness> <case> rc=<n> <first stderr line>`.

require src/lib/test.f
require src/lib/test/ending.f
require src/lib/string.f
require src/lib/adt/option.f
require src/lib/fmt.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process.f
require src/lib/process-command.f

package VOCABULARY-TEST
private

10 constant LF

PTR-VARIABLE STEM-A                    \ the harness the next child loads, as its file stem
variable STEM-U
variable CODE                          \ the last child's exit code, -1 when it did not exit

: ARTIFACT$ ( -- ptr u8 n ) s" build/vocabulary-run.txt" ;
: DIR$ ( -- ptr u8 n ) s" test/policy/" ;

: HARNESS! ( ptr u8 n -- ) STEM-U ! STEM-A ! ;
: STEM$ ( -- ptr u8 n ) STEM-A @ STEM-U @ ;
: TIER0 ( -- ) s" allow" HARNESS! ;

: FILE$ ( ptr u8 n -- ptr u8 n )
   {: stem:ptr su:n :}
   SB-RESET DIR$ SB-APPEND stem su SB-APPEND s" .f" SB-APPEND
   SB$ ;

: CODE! ( CMD:command PROCESS:ending ptr u8 n -- CMD:command )
   {: oc ran:ptr ranu:n :}
   CMD:OUT$ {: out:ptr outu:n :}
   CMD:ERR$ {: err:ptr erru:n :}
   oc MATCH PROCESS:ending
     exited OF CODE ! ENDOF
     signaled OF drop -1 CODE ! ENDOF
     timeout OF ran ranu out outu err erru T-TIMED-OUT ENDOF
   ;MATCH ;

: FILE-ARG+ ( CMD:command ptr u8 n -- CMD:command )
   FILE$ CMD:ARG+ ;

\ An empty case name loads the harness alone.
: SPAWN ( CMD:command ptr u8 n -- CMD:command )
   {: name:ptr nu:n :}
   s" --load" CMD:ARG+
   STEM$ FILE-ARG+
   nu 0 > if
      s" --" CMD:ARG+
      name nu FILE-ARG+
   then
   s" bin/hb" PROCESS:WAIT-FOREVER CMD:RUN-OUTCOME
   nu 0 > if name nu else STEM$ then FILE$ CODE! ;

: ERR-LINE$ ( CMD:command -- CMD:command ptr u8 n )
   CMD:ERR$ {: a:ptr u:n :}
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

\ Append the first stderr line to the builder, the absolute path after
\ `habu: ` cut back to the one the harness was given.
: SAID+ ( CMD:command -- CMD:command )
   ERR-LINE$ {: a:ptr u:n :}
   a u s" habu: /" FIND-SUB MATCH option
     none OF a u SB-APPEND ENDOF
     some OF IDX>N 6 + {: cut:n :}
        a cut SB-APPEND
        a cut + u cut - TREE-PATH SB-APPEND ENDOF
   ;MATCH ;

\ The first `want` bytes of that line, so a row states as much of a diagnostic
\ as the seal owns: a policy line whole, a checker line by its head.
: SAID-HEAD$ ( CMD:command n -- CMD:command ptr u8 n ) {: want:n :}
   SB-RESET SAID+
   SB$ {: a:ptr u:n :}
   a u want min ;

: LOG ( CMD:command ptr u8 n -- CMD:command ) {: name:ptr nu:n :}
   SB-RESET STEM$ SB-APPEND s"  " SB-APPEND
   nu 0 > if name nu SB-APPEND else s" -" SB-APPEND then
   s"  rc=" SB-APPEND CODE @ FMT:SB-INT s"  " SB-APPEND
   SAID+ LF SB-APPEND-C
   ARTIFACT$ SB$ FS:APPEND-FILE ;

: LABEL ( ptr u8 n ptr u8 n -- ) {: name:ptr nu:n what:ptr wu:n :}
   SB-RESET STEM$ SB-APPEND s"  " SB-APPEND name nu SB-APPEND
   s" : " SB-APPEND what wu SB-APPEND
   SB$ T-LABEL ;

\ One scope per child: its captures stay readable until the verdict is taken.
: RUNS-IN ( ptr u8 n n ptr u8 n ptr u8 n CMD:command -- CMD:command )
   >r {: name:ptr nu:n rc:n line:ptr lu:n out:ptr ou:n :} r>
   name nu s" exit code" LABEL
   name nu SPAWN
   name nu LOG
   CODE @ rc T=
   name nu s" first stderr line" LABEL
   lu SAID-HEAD$ line lu T$=
   name nu s" stdout" LABEL
   CMD:OUT$ out ou T$= ;

: RUNS ( ptr u8 n n ptr u8 n ptr u8 n -- )
   [: RUNS-IN ;] CMD:WITH-COMMAND ;

\ The engine's one refusal: the located line, ENGINE-ERROR:POLICY, no output.
: REFUSES ( ptr u8 n ptr u8 n -- ) {: name:ptr nu:n line:ptr lu:n :}
   name nu ENGINE-ERROR:POLICY line lu s" " RUNS ;

: TIER0-REFUSES ( ptr u8 n ptr u8 n -- ) {: name:ptr nu:n line:ptr lu:n :}
   TIER0 name nu line lu REFUSES ;

: OK-OUT$ ( -- ptr u8 n ) S\" 42\n5\n5\n42\n0\n11\n" ;

\ A local shadows every word, as it does unsealed.
: LOCALS ( -- )
   TIER0 s" local-name" 0 s" " S\" 42\n" RUNS ;

: ESCAPED ( -- )
   TIER0 s" escaped" 0 s" " S\" Bracket \\ \"Café\"Bracket \\ \"Café\"" RUNS
   TIER0 s" escaped-tail" 107 s" E-POLICY habu: test/policy/escaped-tail.f:3: not in vocabulary: required" S\" Bracket \\ \"Café\"" RUNS
   s" escaped-body-tail" s" E-POLICY habu: test/policy/escaped-body-tail.f:2: not in vocabulary: required" TIER0-REFUSES
   s" escaped-c" S\" E-POLICY habu: test/policy/escaped-c.f:2: not in vocabulary: C\\\q" TIER0-REFUSES
   s" escaped-dotq" S\" E-POLICY habu: test/policy/escaped-dotq.f:2: not in vocabulary: .\\\q" TIER0-REFUSES ;

\ The seal removes words; it does not change what an admitted program does, what
\ the checker refuses, how a throw ends the process, or the refusal of a source
\ that ends inside a definition.
: ADMITTED ( -- )
   TIER0 s" ok" 0 s" " OK-OUT$ RUNS
   TIER0 s" dead" 67 s" hb: uncaught throw code -30001" s" " RUNS
   TIER0 s" throw" 67 s" hb: uncaught throw code -30001" s" " RUNS
   TIER0 s" checked" 70 s" habu: in bad: at 'BAD' expected: n" s" " RUNS
   TIER0 s" eof" 74 s" E-VS-UNTERMINATED-DEFINITION habu: test/policy/eof.f:2: source ended inside definition: X" s" " RUNS
   LOCALS ESCAPED ;

: LOADERS ( -- )
   s" bypass" s" E-POLICY habu: test/policy/bypass.f:2: not in vocabulary: require" TIER0-REFUSES
   s" included" s" E-POLICY habu: test/policy/included.f:1: not in vocabulary: included" TIER0-REFUSES
   s" evaluate" s" E-POLICY habu: test/policy/evaluate.f:1: not in vocabulary: evaluate" TIER0-REFUSES ;

: KEYWORDS ( -- )
   s" trusted" s" E-POLICY habu: test/policy/trusted.f:1: not in vocabulary: trusted:" TIER0-REFUSES
   s" tick" s" E-POLICY habu: test/policy/tick.f:2: not in vocabulary: '" TIER0-REFUSES
   s" string" S\" E-POLICY habu: test/policy/string.f:2: not in vocabulary: c\q" TIER0-REFUSES ;

\ A global a used package also exports is outside the vocabulary before it is
\ shadowed: the seal refuses it ahead of the shadowing rule, as in a body.
\ A package loaded before the seal, admitted or not, cannot be reopened: inside
\ it its private words come first. ok.f reopens the design's own package.
: SCOPES ( -- )
   s" using-ffi" s" E-POLICY habu: test/policy/using-ffi.f:3: not in vocabulary: NOW" TIER0-REFUSES
   s" shadow" s" E-POLICY habu: test/policy/shadow.f:3: not in vocabulary: emit" TIER0-REFUSES
   s" private" s" E-POLICY habu: test/policy/private.f:2: not in vocabulary: PDEP:HIDDEN" TIER0-REFUSES
   s" reopen" s" E-POLICY habu: test/policy/reopen.f:2: policy: package PDEP predates the seal" TIER0-REFUSES
   s" reopen-foreign" s" E-POLICY habu: test/policy/reopen-foreign.f:2: policy: package PFOREIGN predates the seal" TIER0-REFUSES
   s" foreign-call" s" E-POLICY habu: test/policy/foreign-call.f:2: not in vocabulary: PFOREIGN:LEAK" TIER0-REFUSES
   s" undefined" s" E-POLICY habu: test/policy/undefined.f:1: not in vocabulary: NOSUCHWORD" TIER0-REFUSES ;

: WIDENING ( -- )
   s" allow-call" s" E-POLICY habu: test/policy/allow-call.f:2: not in vocabulary: VOCABULARY:ALLOW-PACKAGE" TIER0-REFUSES
   s" seal-call" s" E-POLICY habu: test/policy/seal-call.f:2: not in vocabulary: VOCABULARY:RESTRICT" TIER0-REFUSES ;

\ Tier 0 refuses at its compile rows and call lookup.
: BODIES ( -- )
   s" capture" s" E-POLICY habu: test/policy/capture.f:2: not in vocabulary: require" TIER0-REFUSES
   s" execute" s" E-POLICY habu: test/policy/execute.f:1: not in vocabulary: execute" TIER0-REFUSES
   s" store" s" E-POLICY habu: test/policy/store.f:1: not in vocabulary: !" TIER0-REFUSES
   s" arith" s" E-POLICY habu: test/policy/arith.f:1: not in vocabulary: +" TIER0-REFUSES
   s" dotq" S\" E-POLICY habu: test/policy/dotq.f:2: not in vocabulary: .\q" TIER0-REFUSES
   s" used" s" E-POLICY habu: test/policy/used.f:3: not in vocabulary: LEAK" TIER0-REFUSES
   s" internal" s" E-POLICY habu: test/policy/internal.f:4: not in vocabulary: STEP" TIER0-REFUSES ;

\ A keyword the native compiler models itself, a character literal, a call to
\ the definition being compiled and a `:}` outside a `{:` group are neither
\ records nor numbers: tier 0 refuses each at the token, before the compiler or
\ the checker reads the body. `[package]`, in the design span, compiles.
: MODELED ( -- )
   TIER0 s" package-literal" 0 s" " s" pdesign-name" RUNS
   s" begin" s" E-POLICY habu: test/policy/begin.f:1: not in vocabulary: begin" TIER0-REFUSES
   s" recurse" s" E-POLICY habu: test/policy/recurse.f:1: not in vocabulary: recurse" TIER0-REFUSES
   s" quote" s" E-POLICY habu: test/policy/quote.f:1: not in vocabulary: [:" TIER0-REFUSES
   s" btick" s" E-POLICY habu: test/policy/btick.f:2: not in vocabulary: [']" TIER0-REFUSES
   s" char-literal" s" E-POLICY habu: test/policy/char-literal.f:2: not in vocabulary: [char]" TIER0-REFUSES
   s" self" s" E-POLICY habu: test/policy/self.f:1: not in vocabulary: R" TIER0-REFUSES
   s" exit" s" E-POLICY habu: test/policy/exit.f:1: not in vocabulary: exit" TIER0-REFUSES
   s" bracket" s" E-POLICY habu: test/policy/bracket.f:2: not in vocabulary: [" TIER0-REFUSES
   s" postpone" s" E-POLICY habu: test/policy/postpone.f:1: not in vocabulary: postpone" TIER0-REFUSES
   s" rstack" s" E-POLICY habu: test/policy/rstack.f:1: not in vocabulary: >r" TIER0-REFUSES
   s" do" s" E-POLICY habu: test/policy/do.f:1: not in vocabulary: do" TIER0-REFUSES
   s" stray-close" s" E-POLICY habu: test/policy/stray-close.f:1: not in vocabulary: :}" TIER0-REFUSES ;

\ No keyword row: at tier 0 the row would answer a global name before any
\ lookup. A row outside the design span is refused as a token; a row inside it
\ is the reserved name, the same refusal unsealed. The pass-2 rows count too:
\ tier 0 lowers a body holding a wide value again, and they answer its tokens
\ first. So do `:}` (in the span), `kernel:` and `;match`, compared only inside
\ their construct.
: NAMES ( -- )
   s" kwname" s" E-POLICY habu: test/policy/kwname.f:2: not in vocabulary: begin" TIER0-REFUSES
   s" own-keyword" s" E-POLICY habu: test/policy/own-keyword.f:4: not in vocabulary: dup" TIER0-REFUSES
   s" defname-p2" s" E-POLICY habu: test/policy/defname-p2.f:2: not in vocabulary: tuck" TIER0-REFUSES
   s" own-p2-keyword" s" E-POLICY habu: test/policy/own-p2-keyword.f:5: not in vocabulary: tuck" TIER0-REFUSES
   s" defname-interpret" s" E-POLICY habu: test/policy/defname-interpret.f:2: not in vocabulary: create" TIER0-REFUSES
   TIER0 s" defname-span" 70 s" E-RESERVED-DEFINITION habu: test/policy/defname-span.f:2: compile keyword cannot be a definition name: package" s" " RUNS
   TIER0 s" defname-endloc" 70 s" E-RESERVED-DEFINITION habu: test/policy/defname-endloc.f:4: compile keyword cannot be a definition name: :}" s" " RUNS
   s" defname-kernel" s" E-POLICY habu: test/policy/defname-kernel.f:4: not in vocabulary: kernel:" TIER0-REFUSES
   s" defname-semimatch" s" E-POLICY habu: test/policy/defname-semimatch.f:4: not in vocabulary: ;match" TIER0-REFUSES ;

: HARNESSES ( -- )
   s" allow-sealed-admit" HARNESS!
   s" " ENGINE-ERROR:POLICY s" E-POLICY habu: policy: sealed lr " s" " RUNS
   s" allow-seal-twice" HARNESS!
   s" " ENGINE-ERROR:POLICY s" E-POLICY habu: policy: sealed lr " s" " RUNS
   s" allow-no-package" HARNESS!
   s" " ENGINE-ERROR:POLICY s" E-POLICY habu: policy: no package NOPE lr " s" " RUNS
   s" allow-keyword-package" HARNESS!
   s" " ENGINE-ERROR:POLICY s" E-POLICY habu: policy: package PKW publishes keyword DUP lr " s" " RUNS
   s" allow-design-keyword-package" HARNESS!
   s" " ENGINE-ERROR:POLICY s" E-POLICY habu: policy: package PSYNTAX publishes keyword package lr " s" " RUNS
   s" allow-ordinary-keyword-package" HARNESS!
   s" ordinary-keyword" 0 s" " S\" 42\n" RUNS
   s" allow-p2-package" HARNESS!
   s" " ENGINE-ERROR:POLICY s" E-POLICY habu: policy: package PTUCK publishes keyword TUCK lr " s" " RUNS
   s" allow-seal-keyword-package" HARNESS!
   s" " ENGINE-ERROR:POLICY s" E-POLICY habu: policy: package PSEAL publishes keyword seal-package lr " s" " RUNS
   s" allow-task-package" HARNESS!
   s" " ENGINE-ERROR:POLICY s" E-POLICY habu: policy: package PTKW publishes keyword DUP lr " S\" admitted\n0\n" RUNS
   s" allow-catch" HARNESS!
   s" evaluate" 0 s" E-POLICY habu: test/policy/evaluate.f:1: not in vocabulary: evaluate" S\" 107\n" RUNS ;

\ A wordlist id at or above PROT-WID-MAX has no bit in the admission bitmap: it
\ cannot be admitted, and a record in it is never in the vocabulary.
: BOUND ( -- )
   s" allow-bound" HARNESS!
   s" " ENGINE-ERROR:POLICY s" E-POLICY habu: policy: package wid above the bound lr " s" " RUNS
   s" far" s" E-POLICY habu: test/policy/far.f:2: not in vocabulary: PBIG:FAR" REFUSES ;

\ A harness that bound the loaded-bytes seam to the interpret loop written in
\ Habu, which reads no seal: a sealed load is read by the engine's loop all the
\ same (src/core/include.f INTERPRET-SAVED-RANGE), so it runs and refuses as it does there.
: SEAM ( -- )
   s" allow-interpreter" HARNESS!
   s" ok" 0 s" " OK-OUT$ RUNS
   s" bypass" s" E-POLICY habu: test/policy/bypass.f:2: not in vocabulary: require" REFUSES ;

\ Confined static require uses the original source paths through nested loads.
\ A registry hit or a symlink still has to pass the resolved path boundary.
: CONFINED ( -- )
   s" allow-root" HARNESS!
   s" confined/scene" 0 s" " S\" 42\n" RUNS
   s" confined/parent" s" E-POLICY habu: policy: source outside root: " REFUSES
   s" confined/known" s" E-POLICY habu: policy: source outside root: " REFUSES
   s" confined/library" s" E-POLICY habu: policy: source outside root: " REFUSES
   s" confined/absolute" s" E-POLICY habu: policy: source outside root: " REFUSES
   s" confined/symlink" s" E-POLICY habu: policy: source outside root: " REFUSES
   s" confined/forbidden" s" E-POLICY habu: test/policy/confined/forbidden.f:1: not in vocabulary: included" REFUSES
   s" confined/body" s" E-POLICY habu: test/policy/confined/body.f:1: not in vocabulary: require" REFUSES
   s" confined/string-required" s" E-POLICY habu: test/policy/confined/string-required.f:1: not in vocabulary: required" REFUSES
   s" confined/root-call" s" E-POLICY habu: test/policy/confined/root-call.f:1: not in vocabulary: VOCABULARY:SOURCE-ROOT!" REFUSES
   s" allow-root-direct" HARNESS!
   s" " 107 s" E-POLICY habu: policy: source outside root: " s" " RUNS
   s" allow-root-preflight" HARNESS!
   s" " 0 s" E-POLICY habu: policy: source outside root: " S\" 0\n107\n0\n\n" RUNS
   s" allow-root-scope-throw" HARNESS!
   s" " 0 s" " S\" -30001\n0\n\n" RUNS
   s" allow-root-scope-nested" HARNESS!
   s" " 0 s" E-POLICY habu: policy: source outside root: " S\" 107\n0\n0\n\n" RUNS
   s" allow-root-scope-seal" HARNESS!
   s" " 0 s" E-POLICY habu: policy: sealed" S\" 107\n107\n107\n107\n\n" RUNS ;

public

: RUN ( -- )
   T-RESET
   s" build" FS:MAKE-DIRS
   ARTIFACT$ s" " FS:WRITE-ALL
   ADMITTED
   LOADERS KEYWORDS SCOPES WIDENING
   BODIES MODELED
   NAMES
   HARNESSES BOUND SEAM CONFINED
   T-REPORT ;

;package

VOCABULARY-TEST:RUN
