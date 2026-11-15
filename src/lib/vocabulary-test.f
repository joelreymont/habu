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

\ Append the first stderr line to the builder.
: SAID+ ( CMD:command -- CMD:command )
   ERR-LINE$ SB-APPEND ;

\ The first `want` bytes of that line, so a case pins a record's first line
\ whole or by its head.
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

\ RUNS, and a span the whole stderr holds: the record's place or its notes.
: SAYS-IN ( ptr u8 n n ptr u8 n ptr u8 n ptr u8 n CMD:command -- CMD:command )
   >r {: name:ptr nu:n rc:n line:ptr lu:n span:ptr su:n out:ptr ou:n :} r>
   >r name nu rc line lu out ou r> RUNS-IN
   name nu s" stderr span" LABEL
   CMD:ERR$ span su CONTAINS? TTRUE ;

: SAYS ( ptr u8 n n ptr u8 n ptr u8 n ptr u8 n -- )
   [: SAYS-IN ;] CMD:WITH-COMMAND ;

\ A rejection the engine throws: the record's first line and its place,
\ status 70, no output.
: REFUSES ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: name:ptr nu:n line:ptr lu:n place:ptr pu:n :}
   name nu CHECKER-REJECT-RC line lu place pu s" " SAYS ;

: TIER0-REFUSES ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: name:ptr nu:n line:ptr lu:n place:ptr pu:n :}
   TIER0 name nu line lu place pu REFUSES ;

\ A harness the engine stops with an E-POLICY record: its first line, the
\ notes span, the stdout.
: STOPS ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: line:ptr lu:n notes:ptr nu:n out:ptr ou:n :}
   s" " ENGINE-ERROR:POLICY line lu notes nu out ou SAYS ;

: SEALED$ ( -- ptr u8 n ) s" error[E-POLICY]: the vocabulary is sealed" ;
: KEYWORD$ ( -- ptr u8 n ) s" error[E-POLICY]: a package publishes a word spelled like a keyword" ;

: OK-OUT$ ( -- ptr u8 n ) S\" 42\n5\n5\n42\n0\n11\n" ;

\ A local shadows every word, as it does unsealed.
: LOCALS ( -- )
   TIER0 s" local-name" 0 s" " S\" 42\n" RUNS ;

: ESCAPED ( -- )
   TIER0 s" escaped" 0 s" " S\" Bracket \\ \"Café\"Bracket \\ \"Café\"" RUNS
   TIER0 s" escaped-tail" 70 s" error[E-POLICY]: `required` is not in the vocabulary this source may use" s" test/policy/escaped-tail.f:3:" S\" Bracket \\ \"Café\"" SAYS
   s" escaped-body-tail" s" error[E-POLICY]: `required` is not in the vocabulary this source may use" s" test/policy/escaped-body-tail.f:2:" TIER0-REFUSES
   s" escaped-c" S\" error[E-POLICY]: `C\\\q` is not in the vocabulary this source may use" s" test/policy/escaped-c.f:2:" TIER0-REFUSES
   s" escaped-dotq" S\" error[E-POLICY]: `.\\\q` is not in the vocabulary this source may use" s" test/policy/escaped-dotq.f:2:" TIER0-REFUSES ;

\ The seal removes words; it does not change what an admitted program does, what
\ the checker refuses, how a throw ends the process, or the refusal of a source
\ that ends inside a definition.
: ADMITTED ( -- )
   TIER0 s" ok" 0 s" " OK-OUT$ RUNS
   TIER0 s" dead" 67 s" hb: uncaught throw code -30001" s" " RUNS
   TIER0 s" throw" 67 s" hb: uncaught throw code -30001" s" " RUNS
   TIER0 s" checked" 70 s" error[E-MISMATCH]: the body ends with nothing on the stack; the signature promises n" s" test/policy/checked.f:2:" s" " SAYS
   TIER0 s" eof" 74 s" error[E-VS-UNTERMINATED-DEFINITION]: the source ends inside the definition of `X`" s" test/policy/eof.f:2:" s" " SAYS
   LOCALS ESCAPED ;

: LOADERS ( -- )
   s" bypass" s" error[E-POLICY]: `require` is not in the vocabulary this source may use" s" test/policy/bypass.f:2:" TIER0-REFUSES
   s" included" s" error[E-POLICY]: `included` is not in the vocabulary this source may use" s" test/policy/included.f:1:" TIER0-REFUSES
   s" evaluate" s" error[E-POLICY]: `evaluate` is not in the vocabulary this source may use" s" test/policy/evaluate.f:1:" TIER0-REFUSES ;

: KEYWORDS ( -- )
   s" trusted" s" error[E-POLICY]: `trusted:` is not in the vocabulary this source may use" s" test/policy/trusted.f:1:" TIER0-REFUSES
   s" tick" s" error[E-POLICY]: `'` is not in the vocabulary this source may use" s" test/policy/tick.f:2:" TIER0-REFUSES
   s" string" S\" error[E-POLICY]: `c\q` is not in the vocabulary this source may use" s" test/policy/string.f:2:" TIER0-REFUSES ;

\ A global a used package also exports is outside the vocabulary before it is
\ shadowed: the seal refuses it ahead of the shadowing rule, as in a body.
\ A package loaded before the seal, admitted or not, cannot be reopened: inside
\ it its private words come first. ok.f reopens the design's own package.
: SCOPES ( -- )
   s" using-ffi" s" error[E-POLICY]: `NOW` is not in the vocabulary this source may use" s" test/policy/using-ffi.f:3:" TIER0-REFUSES
   s" shadow" s" error[E-POLICY]: `emit` is not in the vocabulary this source may use" s" test/policy/shadow.f:3:" TIER0-REFUSES
   s" private" s" error[E-POLICY]: `PDEP:HIDDEN` is not in the vocabulary this source may use" s" test/policy/private.f:2:" TIER0-REFUSES
   s" reopen" s" error[E-POLICY]: package `PDEP` predates the seal, so sealed source cannot reopen it" s" test/policy/reopen.f:2:" TIER0-REFUSES
   s" reopen-foreign" s" error[E-POLICY]: package `PFOREIGN` predates the seal, so sealed source cannot reopen it" s" test/policy/reopen-foreign.f:2:" TIER0-REFUSES
   s" foreign-call" s" error[E-POLICY]: `PFOREIGN:LEAK` is not in the vocabulary this source may use" s" test/policy/foreign-call.f:2:" TIER0-REFUSES
   s" undefined" s" error[E-POLICY]: `NOSUCHWORD` is not in the vocabulary this source may use" s" test/policy/undefined.f:1:" TIER0-REFUSES ;

: WIDENING ( -- )
   s" allow-call" s" error[E-POLICY]: `VOCABULARY:ALLOW-PACKAGE` is not in the vocabulary this source may use" s" test/policy/allow-call.f:2:" TIER0-REFUSES
   s" seal-call" s" error[E-POLICY]: `VOCABULARY:RESTRICT` is not in the vocabulary this source may use" s" test/policy/seal-call.f:2:" TIER0-REFUSES ;

\ Tier 0 refuses at its compile rows and call lookup.
: BODIES ( -- )
   s" capture" s" error[E-POLICY]: `require` is not in the vocabulary this source may use" s" test/policy/capture.f:2:" TIER0-REFUSES
   s" execute" s" error[E-POLICY]: `execute` is not in the vocabulary this source may use" s" test/policy/execute.f:1:" TIER0-REFUSES
   s" store" s" error[E-POLICY]: `!` is not in the vocabulary this source may use" s" test/policy/store.f:1:" TIER0-REFUSES
   s" arith" s" error[E-POLICY]: `+` is not in the vocabulary this source may use" s" test/policy/arith.f:1:" TIER0-REFUSES
   s" dotq" S\" error[E-POLICY]: `.\q` is not in the vocabulary this source may use" s" test/policy/dotq.f:2:" TIER0-REFUSES
   s" used" s" error[E-POLICY]: `LEAK` is not in the vocabulary this source may use" s" test/policy/used.f:3:" TIER0-REFUSES
   s" internal" s" error[E-POLICY]: `STEP` is not in the vocabulary this source may use" s" test/policy/internal.f:4:" TIER0-REFUSES ;

\ A keyword the native compiler models itself, a character literal, a call to
\ the definition being compiled and a `:}` outside a `{:` group are neither
\ records nor numbers: tier 0 refuses each at the token, before the compiler or
\ the checker reads the body. `[package]`, in the design span, compiles.
: MODELED ( -- )
   TIER0 s" package-literal" 0 s" " s" pdesign-name" RUNS
   s" begin" s" error[E-POLICY]: `begin` is not in the vocabulary this source may use" s" test/policy/begin.f:1:" TIER0-REFUSES
   s" recurse" s" error[E-POLICY]: `recurse` is not in the vocabulary this source may use" s" test/policy/recurse.f:1:" TIER0-REFUSES
   s" quote" s" error[E-POLICY]: `[:` is not in the vocabulary this source may use" s" test/policy/quote.f:1:" TIER0-REFUSES
   s" btick" s" error[E-POLICY]: `[']` is not in the vocabulary this source may use" s" test/policy/btick.f:2:" TIER0-REFUSES
   s" char-literal" s" error[E-POLICY]: `[char]` is not in the vocabulary this source may use" s" test/policy/char-literal.f:2:" TIER0-REFUSES
   s" self" s" error[E-POLICY]: `R` is not in the vocabulary this source may use" s" test/policy/self.f:1:" TIER0-REFUSES
   s" exit" s" error[E-POLICY]: `exit` is not in the vocabulary this source may use" s" test/policy/exit.f:1:" TIER0-REFUSES
   s" bracket" s" error[E-POLICY]: `[` is not in the vocabulary this source may use" s" test/policy/bracket.f:2:" TIER0-REFUSES
   s" postpone" s" error[E-POLICY]: `postpone` is not in the vocabulary this source may use" s" test/policy/postpone.f:1:" TIER0-REFUSES
   s" rstack" s" error[E-POLICY]: `>r` is not in the vocabulary this source may use" s" test/policy/rstack.f:1:" TIER0-REFUSES
   s" do" s" error[E-POLICY]: `do` is not in the vocabulary this source may use" s" test/policy/do.f:1:" TIER0-REFUSES
   s" stray-close" s" error[E-POLICY]: `:}` is not in the vocabulary this source may use" s" test/policy/stray-close.f:1:" TIER0-REFUSES ;

\ No keyword row: at tier 0 the row would answer a global name before any
\ lookup. A row outside the design span is refused as a token; a row inside it
\ is the reserved name, the same refusal unsealed. The pass-2 rows count too:
\ tier 0 lowers a body holding a wide value again, and they answer its tokens
\ first. So do `:}` (in the span), `kernel:` and `;match`, compared only inside
\ their construct.
: NAMES ( -- )
   s" kwname" s" error[E-POLICY]: `begin` is an engine keyword outside the vocabulary and cannot name a definition" s" test/policy/kwname.f:2:" TIER0-REFUSES
   s" own-keyword" s" error[E-POLICY]: `dup` is an engine keyword outside the vocabulary and cannot name a definition" s" test/policy/own-keyword.f:4:" TIER0-REFUSES
   s" defname-p2" s" error[E-POLICY]: `tuck` is an engine keyword outside the vocabulary and cannot name a definition" s" test/policy/defname-p2.f:2:" TIER0-REFUSES
   s" own-p2-keyword" s" error[E-POLICY]: `tuck` is an engine keyword outside the vocabulary and cannot name a definition" s" test/policy/own-p2-keyword.f:5:" TIER0-REFUSES
   s" defname-interpret" s" error[E-POLICY]: `create` is an engine keyword outside the vocabulary and cannot name a definition" s" test/policy/defname-interpret.f:2:" TIER0-REFUSES
   TIER0 s" defname-span" 70 s" error[E-RESERVED-DEFINITION]: `package` is a compile keyword and cannot name a definition" s" test/policy/defname-span.f:2:" s" " SAYS
   TIER0 s" defname-endloc" 70 s" error[E-RESERVED-DEFINITION]: `:}` is a compile keyword and cannot name a definition" s" test/policy/defname-endloc.f:4:" s" " SAYS
   s" defname-kernel" s" error[E-POLICY]: `kernel:` is an engine keyword outside the vocabulary and cannot name a definition" s" test/policy/defname-kernel.f:4:" TIER0-REFUSES
   s" defname-semimatch" s" error[E-POLICY]: `;match` is an engine keyword outside the vocabulary and cannot name a definition" s" test/policy/defname-semimatch.f:4:" TIER0-REFUSES ;

: HARNESSES ( -- )
   s" allow-sealed-admit" HARNESS!
   SEALED$ s" = in: ALLOW-PACKAGE" s" " STOPS
   s" allow-seal-twice" HARNESS!
   SEALED$ s" = in: RESTRICT" s" " STOPS
   s" allow-no-package" HARNESS!
   s" error[E-POLICY]: no package" s" = package: NOPE" s" " STOPS
   s" allow-keyword-package" HARNESS!
   KEYWORD$ S\" = package: PKW\n  = keyword: DUP\n" s" " STOPS
   s" allow-design-keyword-package" HARNESS!
   KEYWORD$ S\" = package: PSYNTAX\n  = keyword: package\n" s" " STOPS
   s" allow-ordinary-keyword-package" HARNESS!
   s" ordinary-keyword" 0 s" " S\" 42\n" RUNS
   s" allow-p2-package" HARNESS!
   KEYWORD$ S\" = package: PTUCK\n  = keyword: TUCK\n" s" " STOPS
   s" allow-seal-keyword-package" HARNESS!
   KEYWORD$ S\" = package: PSEAL\n  = keyword: seal-package\n" s" " STOPS
   s" allow-task-package" HARNESS!
   KEYWORD$ S\" = package: PTKW\n  = keyword: DUP\n" S\" admitted\n0\n" STOPS
   s" allow-catch" HARNESS!
   s" evaluate" 0 s" error[E-POLICY]: `evaluate` is not in the vocabulary this source may use" s" test/policy/evaluate.f:1:" S\" 70\n" SAYS ;

\ A wordlist id at or above PROT-WID-MAX has no bit in the admission bitmap: it
\ cannot be admitted, and a record in it is never in the vocabulary.
: BOUND ( -- )
   s" allow-bound" HARNESS!
   s" " ENGINE-ERROR:POLICY s" E-POLICY habu: policy: package wid above the bound lr " s" " RUNS
   s" far" s" error[E-POLICY]: `PBIG:FAR` is not in the vocabulary this source may use" s" test/policy/far.f:2:" REFUSES ;

\ A harness that bound the loaded-bytes seam to the interpret loop written in
\ Habu, which reads no seal: a sealed load is read by the engine's loop all the
\ same (src/core/include.f INTERPRET-SAVED-RANGE), so it runs and refuses as it does there.
: SEAM ( -- )
   s" allow-interpreter" HARNESS!
   s" ok" 0 s" " OK-OUT$ RUNS
   s" bypass" s" error[E-POLICY]: `require` is not in the vocabulary this source may use" s" test/policy/bypass.f:2:" REFUSES ;

\ A record that names a path outside the root, whose head is the same for each.
: ROOT$ ( -- ptr u8 n ) s" error[E-POLICY]: `" ;

\ Confined static require uses the original source paths through nested loads.
\ A registry hit or a symlink still has to pass the resolved path boundary.
: CONFINED ( -- )
   s" allow-root" HARNESS!
   s" confined/scene" 0 s" " S\" 42\n" RUNS
   s" confined/parent" ROOT$ s" `../outside.f` lies outside the source root" REFUSES
   s" confined/known" ROOT$ s" `../dep.f` lies outside the source root" REFUSES
   s" confined/library" ROOT$ s" `src/lib/string.f` lies outside the source root" REFUSES
   s" confined/absolute" ROOT$ s" `/etc/passwd` lies outside the source root" REFUSES
   s" confined/symlink" ROOT$ s" `link.f` lies outside the source root" REFUSES
   s" confined/forbidden" s" error[E-POLICY]: `included` is not in the vocabulary this source may use" s" test/policy/confined/forbidden.f:1:" REFUSES
   s" confined/body" s" error[E-POLICY]: `require` is not in the vocabulary this source may use" s" test/policy/confined/body.f:1:" REFUSES
   s" confined/string-required" s" error[E-POLICY]: `required` is not in the vocabulary this source may use" s" test/policy/confined/string-required.f:1:" REFUSES
   s" confined/root-call" s" error[E-POLICY]: `VOCABULARY:SOURCE-ROOT!` is not in the vocabulary this source may use" s" test/policy/confined/root-call.f:1:" REFUSES
   s" allow-root-direct" HARNESS!
   s" " CHECKER-REJECT-RC ROOT$ s" /test/policy/confined/link.f` lies outside the source root" s" " SAYS
   s" allow-root-preflight" HARNESS!
   s" " 0 ROOT$ s" `../outside.f` lies outside the source root" S\" 0\n70\n0\n\n" SAYS
   s" allow-root-scope-throw" HARNESS!
   s" " 0 s" " S\" -30001\n0\n\n" RUNS
   s" allow-root-scope-nested" HARNESS!
   s" " 0 ROOT$ s" `test/policy/confined/value.f` lies outside the source root" S\" 70\n0\n0\n\n" SAYS
   s" allow-root-scope-seal" HARNESS!
   s" " 0 s" error[E-POLICY]: the source policy is already sealed" s" test/policy/allow-root-scope-seal.f:14:" S\" 70\n70\n70\n70\n\n" SAYS ;

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
