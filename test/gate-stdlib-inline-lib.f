\ gate-stdlib-inline-lib.f - in-process stdlib gate slices for resident runners.
\
\ Load after test/gate-stdlib-lib.f in the resident test runner.
\
variable GSI-TIMINGS
variable GSI-PATH-A
variable GSI-PATH-U
variable GSI-START-NS
variable GSI-RC
variable GSI-SETUP
variable GSI-TEST-READY
variable GSI-TOOL-BASE-READY

600000 constant GSI-FORK-TIMEOUT-MS

0 constant GSI-GROUP-SEQ
1 constant GSI-GROUP-PAR

: GSI-PATH-A-FIELD ( -- ptr ptr u8 )
   GSI-PATH-A 0 ptr-field ;

: GSI-PATH-A@ ( -- ptr u8 )
   GSI-PATH-A-FIELD @ ;

: GSI-PATH-A! ( ptr u8 -- )
   GSI-PATH-A-FIELD ! ;

: GSI-PATH$ ( -- ptr u8 n )
   GSI-PATH-A@ GSI-PATH-U @ ;

: GSI-TIMINGS! ( -- )
   -1 GSI-TIMINGS ! ;

: GSI-TIMINGS? ( -- bool )
   GSI-TIMINGS @ 0 <> ;

: GSI-SETUP! ( -- )
   -1 GSI-SETUP ! ;

: GSI-TEST! ( -- )
   0 GSI-SETUP ! ;

: GSI-SETUP? ( -- bool )
   GSI-SETUP @ 0 <> ;

: GSI-TEST-READY! ( -- )
   -1 GSI-TEST-READY ! ;

: GSI-TEST-READY? ( -- bool )
   GSI-TEST-READY @ 0 <> ;

: GSI-TOOL-BASE-READY! ( -- )
   -1 GSI-TOOL-BASE-READY ! ;

: GSI-TOOL-BASE-READY? ( -- bool )
   GSI-TOOL-BASE-READY @ 0 <> ;

: GSI-GROUP-MODE. ( n -- ) {: mode:n :}
   mode case
      GSI-GROUP-SEQ of s" sequential" type endof
      GSI-GROUP-PAR of s" parallel" type endof
      E-TBL-FIELD throw
   endcase ;

: GSI-GROUP-HEADER ( ptr u8 n n -- ) {: name:ptr nameu:n mode:n :}
   s" GROUP: " type name nameu type
   s"  [" type mode GSI-GROUP-MODE. s" ]" type cr ;

: GSI-PASS ( ptr u8 n n -- ) {: path:ptr pathu:n ms:n :}
   GSI-SETUP? if exit then
   GSI-TIMINGS? 0= if exit then
   s" PASS: " type path pathu type
   s"  (" type ms GT-U-TYPE s" ms)" type cr ;

: GSI-FAIL ( ptr u8 n n -- ) {: path:ptr pathu:n ms:n :}
   s" FAIL: " type
   GSI-SETUP? if s" setup " type then
   path pathu type
   s"  (" type ms GT-U-TYPE s" ms)" type cr ;

: GSI-SPAN ( ptr u8 n n -- ) {: path:ptr pathu:n ms:n :}
   GSI-SETUP? if exit then
   path pathu ms GS-SPAN ;

: GSI-INCLUDE-ACT ( -- )
   GSI-PATH$ included ;

: GSI-REQUIRE-ACT ( -- )
   GSI-PATH$ required ;

: GSI-INCLUDE-MS ( -- n )
   mono-ns GSI-START-NS @ - PROC-NS-PER-MS / ;

: GSI-LOAD-START ( ptr u8 n -- ) {: path:ptr pathu:n :}
   path GSI-PATH-A!
   pathu GSI-PATH-U !
   mono-ns GSI-START-NS ! ;

: GSI-LOAD-FINISH ( -- )
   GSI-INCLUDE-MS {: ms:n :}
   GSI-PATH$ ms GSI-SPAN
   GSI-RC @ 0= if GSI-PATH$ ms GSI-PASS exit then
   GSI-PATH$ ms GSI-FAIL
   GSI-RC @ throw ;

: GSI-INCLUDE ( ptr u8 n -- )
   GSI-LOAD-START
   [: GSI-INCLUDE-ACT ;] catch GSI-RC !
   GSI-LOAD-FINISH ;

: GSI-REQUIRE ( ptr u8 n -- )
   GSI-LOAD-START
   [: GSI-REQUIRE-ACT ;] catch GSI-RC !
   GSI-LOAD-FINISH ;

\ typed-local-lint: allow-bare-local - q keeps the action effect from the stack signature.
: GSI-RUN ( ptr u8 n [ -- ] -- ) {: label:ptr labelu:n q :}
   label GSI-PATH-A!
   labelu GSI-PATH-U !
   mono-ns GSI-START-NS !
   q catch GSI-RC !
   GSI-INCLUDE-MS {: ms:n :}
   GSI-PATH$ ms GSI-SPAN
   GSI-RC @ 0= if GSI-PATH$ ms GSI-PASS exit then
   GSI-PATH$ ms GSI-FAIL
   GSI-RC @ throw ;

: GSI-FORK-RESET ( -- )
   GT-POOL-RESET ;

: GSI-FORK-DRAIN ( -- )
   GT-POOL-DRAIN ;

: GSI-FORK-INCLUDE-ACT ( -- )
   GSI-PATH$ GSI-INCLUDE ;

: GSI-FORK-INCLUDE ( ptr u8 n -- ) {: path:ptr pathu:n :}
   path GSI-PATH-A!
   pathu GSI-PATH-U !
   path pathu GSI-FORK-TIMEOUT-MS [: GSI-FORK-INCLUDE-ACT ;] GT-POOL-START-FORK ;

: GSI-TOOL-BASE ( -- )
   GSI-TOOL-BASE-READY? if exit then
   s" lib/date.f" GSI-REQUIRE
   GSI-TEST-READY? 0= if
      s" lib/test.f" GSI-REQUIRE
      GSI-TEST-READY!
   then
   s" lib/source.f" GSI-REQUIRE
   s" tools/lint/text.f" GSI-REQUIRE
   s" tools/lint/intern.f" GSI-REQUIRE
   s" tools/lint/token.f" GSI-REQUIRE
   s" tools/lint/lib.f" GSI-REQUIRE
   s" tools/lint/json-writer.f" GSI-REQUIRE
   s" tools/lint/source-lex.f" GSI-REQUIRE
   s" lib/argv.f" GSI-REQUIRE
   s" tools/check-all-errors-core.f" GSI-REQUIRE
   s" tools/diag-origin-core.f" GSI-REQUIRE
   s" tools/json.f" GSI-REQUIRE
   s" tools/json-only-core.f" GSI-REQUIRE
   s" tools/aot-lint-core.f" GSI-REQUIRE
   s" tools/signature-lint-core.f" GSI-REQUIRE
   s" tools/checked-boundary-lint-core.f" GSI-REQUIRE
   s" tools/reserved-name-lint-core.f" GSI-REQUIRE
   s" tools/duplicate-definition-lint-core.f" GSI-REQUIRE
   s" tools/bundle-lib-core.f" GSI-REQUIRE
   GSI-TOOL-BASE-READY! ;

: GSI-TOOL-SETUP ( -- )
   GSI-SETUP!
   GSI-TOOL-BASE
   GSI-TEST! ;

: GSI-TOOL-SETUP-FILE ( ptr u8 n -- )
   GSI-SETUP!
   GSI-REQUIRE
   GSI-TEST! ;

: GSI-TOOL-REPAIR-CHECK ( -- )
   s" stdlib/tool-repair/check-all-errors" GSI-GROUP-SEQ GSI-GROUP-HEADER
   GSI-TOOL-SETUP
   s" tools/check-all-errors-test.f" GSI-INCLUDE ;

package AOT-CALL-GATE
public

\ The mirror of SUITE tool-boundary-aot-call. Both members report over the tree
\ rather than judging one file; the census is the second, and it rides here for
\ the reason written beside that suite.
: RUN ( -- )
   s" stdlib/tool-aot-call" GSI-GROUP-SEQ GSI-GROUP-HEADER
   GSI-TOOL-SETUP
   s" tools/aot-call-report-test.f" GSI-INCLUDE
   s" tools/chain-census-test.f" GSI-INCLUDE ;

;package

package CHECK-CLI-GATE

: REQUIRE-WORD ( ptr u8 n n -- )
   search-wl 0= if E-TBL-FIELD throw then ;

: REJECT-WORD ( ptr u8 n n -- )
   search-wl 0<> if E-TBL-FIELD throw then ;

public

: RUN ( -- )
   s" stdlib/check-cli" GSI-GROUP-SEQ GSI-GROUP-HEADER
   GSI-TOOL-SETUP
   s" tools/check-core.f" GSI-TOOL-SETUP-FILE
   s" tools/check-test.f" GSI-INCLUDE ;

;package

: GSI-TOOL-REPAIR-PACKET ( -- )
   s" stdlib/tool-repair/repair-packet" GSI-GROUP-SEQ GSI-GROUP-HEADER
   GSI-TOOL-SETUP
   s" tools/repair-packet-core.f" GSI-TOOL-SETUP-FILE
   s" tools/repair-packet-test.f" GSI-INCLUDE ;

: GSI-TOOL-DOC-PUBLIC ( -- )
   s" stdlib/tool-doc/public-signatures" GSI-GROUP-SEQ GSI-GROUP-HEADER
   GSI-TOOL-SETUP
   s" tools/public-signatures-core.f" GSI-TOOL-SETUP-FILE
   s" tools/public-signatures-test.f" GSI-INCLUDE ;

: GSI-TOOL-DOC-SCHEMA ( -- )
   s" stdlib/tool-doc/schema-examples" GSI-GROUP-SEQ GSI-GROUP-HEADER
   GSI-TOOL-SETUP
   s" tools/repair-schema-doc-test.f" GSI-INCLUDE
   s" tools/examples-test.f" GSI-INCLUDE ;

: GSI-TOOL-LINT-REPL ( -- )
   s" tools/repl-lint-test.f" GSI-INCLUDE
   s" tools/diag-origin-test.f" GSI-INCLUDE ;

: GSI-TOOL-LINT-AOT ( -- )
   s" stdlib/tool-lints/aot-signature" GSI-GROUP-SEQ GSI-GROUP-HEADER
   GSI-TOOL-SETUP
   s" tools/aot-lint-test.f" GSI-INCLUDE
   s" tools/signature-lint-test.f" GSI-INCLUDE ;

: GSI-TOOL-LINT-NAMES ( -- )
   s" stdlib/tool-lints/names" GSI-GROUP-SEQ GSI-GROUP-HEADER
   GSI-TOOL-SETUP
   s" tools/checked-boundary-lint-test.f" GSI-INCLUDE
   s" tools/reserved-name-lint-test.f" GSI-INCLUDE
   s" tools/duplicate-definition-lint-test.f" GSI-INCLUDE ;

: GSI-TOOL-LINT-BUNDLE ( -- )
   s" stdlib/tool-lints/bundle-json" GSI-GROUP-SEQ GSI-GROUP-HEADER
   GSI-TOOL-SETUP
   s" tools/bundle-lib-test.f" GSI-INCLUDE
   s" tools/json-only-test.f" GSI-INCLUDE ;

: GSI-TOOL-LINT-REPL-PHASE ( -- )
   s" stdlib/tool-lints/repl" GSI-GROUP-SEQ GSI-GROUP-HEADER
   GSI-TOOL-SETUP
   s" tools/repl-lint-core.f" GSI-TOOL-SETUP-FILE
   GSI-TOOL-LINT-REPL ;

: GSI-TOOL-REPAIR-SETUP ( -- )
   GSI-TOOL-SETUP
   s" tools/repair-packet-core.f" GSI-TOOL-SETUP-FILE ;

: GSI-TOOL-REPAIR-BODY ( -- )
   s" tools/check-all-errors-test.f" GSI-INCLUDE
   s" tools/repair-packet-test.f" GSI-INCLUDE ;

package TOOL-SEMANTICS
private

: SETUP ( -- )
   GSI-TOOL-SETUP
   s" tools/public-signatures-core.f" GSI-TOOL-SETUP-FILE ;

: BODY ( -- )
   s" tools/public-signatures-test.f" GSI-INCLUDE
   \ The manifest's load-time bracket. It is here as well as in the
   \ tool-boundary-doc-public SUITE row because only this list is what
   \ test/run.f forks - phase 22 runs TOOL-SEMANTICS:DOC, and a
   \ registration no forked list names is scheduled nowhere it runs.
   s" tools/public-signatures-bracket-test.f" GSI-INCLUDE
   s" tools/repair-schema-doc-test.f" GSI-INCLUDE
   s" tools/examples-test.f" GSI-INCLUDE ;

;package

: GSI-TOOL-LINT-SETUP ( -- )
   GSI-TOOL-SETUP
   s" tools/repl-lint-core.f" GSI-TOOL-SETUP-FILE ;

: GSI-TOOL-LINT-BODY ( -- )
   GSI-TOOL-LINT-REPL
   s" tools/aot-lint-test.f" GSI-INCLUDE
   s" tools/signature-lint-test.f" GSI-INCLUDE
   s" tools/checked-boundary-lint-test.f" GSI-INCLUDE
   s" tools/reserved-name-lint-test.f" GSI-INCLUDE
   s" tools/duplicate-definition-lint-test.f" GSI-INCLUDE
   s" tools/bundle-lib-test.f" GSI-INCLUDE
   s" tools/json-only-test.f" GSI-INCLUDE ;

: GSI-TOOL-TYPED-SETUP ( -- )
   GSI-TOOL-SETUP
   s" tools/typed-local-diff-lint-core.f" GSI-TOOL-SETUP-FILE ;

: GSI-TOOL-TYPED-BODY ( -- )
   s" tools/typed-local-diff-lint-test.f" GSI-INCLUDE ;

: GSI-TOOL-REPAIR ( -- )
   s" stdlib/tool-repair" GSI-GROUP-PAR GSI-GROUP-HEADER
   GSI-TOOL-REPAIR-SETUP
   GSI-TOOL-REPAIR-BODY ;

package TOOL-SEMANTICS
public

: DOC ( -- )
   s" stdlib/tool-doc" GSI-GROUP-PAR GSI-GROUP-HEADER
   SETUP
   BODY ;

;package

: GSI-TOOL-LINT-PHASE ( -- )
   s" stdlib/tool-lints" GSI-GROUP-PAR GSI-GROUP-HEADER
   GSI-TOOL-LINT-SETUP
   GSI-TOOL-LINT-BODY ;

: GSI-TOOL-TYPED ( -- )
   s" stdlib/tool-typed-local" GSI-GROUP-PAR GSI-GROUP-HEADER
   GSI-TOOL-TYPED-SETUP
   GSI-TOOL-TYPED-BODY ;

package TOOL-SEMANTICS
public

: RUN ( -- )
   s" stdlib/tool-semantics" GSI-GROUP-SEQ GSI-GROUP-HEADER
   s" stdlib/tool-repair" GSI-GROUP-SEQ GSI-GROUP-HEADER
   GSI-TOOL-REPAIR-SETUP
   GSI-TOOL-REPAIR-BODY
   s" stdlib/tool-doc" GSI-GROUP-SEQ GSI-GROUP-HEADER
   SETUP
   BODY
   s" stdlib/tool-lints" GSI-GROUP-SEQ GSI-GROUP-HEADER
   GSI-TOOL-LINT-SETUP
   GSI-TOOL-LINT-BODY
   s" stdlib/tool-typed-local" GSI-GROUP-SEQ GSI-GROUP-HEADER
   GSI-TOOL-TYPED-SETUP
   GSI-TOOL-TYPED-BODY ;

;package

: GSI-LINT-TOOLS-SETUP ( -- )
   GSI-SETUP!
   GSI-TOOL-BASE
   s" tools/repl-lint-core.f" GSI-REQUIRE
   s" tools/dot-dep-lint-core.f" GSI-REQUIRE
   s" tools/maki-dep-lint-core.f" GSI-REQUIRE
   s" tools/namespace-lint-core.f" GSI-REQUIRE
   s" tools/error-code-lint-core.f" GSI-REQUIRE
   GSI-TEST! ;

: GSI-LINT-TOOLS ( -- )
   s" stdlib/lint-tools" GSI-GROUP-PAR GSI-GROUP-HEADER
   GSI-LINT-TOOLS-SETUP
   s" test/gate-stdlib-lint-tools.f" included ;

: GSI-TEST-SETUP ( -- )
   GSI-SETUP!
   GSI-TEST-READY? 0= if
      s" lib/test.f" GSI-REQUIRE
      GSI-TEST-READY!
   then
   GSI-TEST! ;

: GSI-TAIL-FAST-SETUP ( -- )
   GSI-TEST-SETUP
   GSI-TOOL-BASE-READY? 0= if s" lib/date.f" GSI-REQUIRE then
   s" lib/property.f" GSI-REQUIRE
   GSI-TEST! ;

: GSI-TAIL-FAST ( -- )
   s" stdlib/tail-fast" GSI-GROUP-SEQ GSI-GROUP-HEADER
   GSI-TAIL-FAST-SETUP
   s" lib/test/assert-test.f" GSI-INCLUDE
   s" lib/test/suite-test.f" GSI-INCLUDE
   s" lib/test/snap-test.f" GSI-INCLUDE
   s" lib/test/record-test.f" GSI-INCLUDE
   s" lib/property-test.f" GSI-INCLUDE
   s" tools/stdlib-date-test.f" GSI-INCLUDE
   s" tools/spawn-emitter-test.f" GSI-INCLUDE
   s" tools/c-call-emitter-test.f" GSI-INCLUDE
   s" test/lit-emit-size-test.f" GSI-INCLUDE
   s" tools/signature-scan-emitter-test.f" GSI-INCLUDE
   s" tools/compiler-dispatch-test.f" GSI-INCLUDE ;

\ This group owns its own member list, so it owns a package, the same way
\ TAIL-PROCESS below already does. Scheduling lib/json-read-perf-contract-test.f
\ is the change that made the list move; the group moved with it rather than
\ keeping a raw global name for a definition that is still being edited. Both
\ dispatchers (test/run-worker-stdlib.f and test/gate-runner-lib.f) now call
\ TAIL-PURE:RUN, so the two entry points cannot schedule different members.
package TAIL-PURE

private

\ Two members of this group reach the clang reference column, and they run as
\ forked children. The image that column calls into is mapped by test/run.f in
\ the process the gate was EXEC'd into, before any fork; this group runs inside
\ a forked worker and so cannot map it either (tools/codegen-compare-cc.f, THE
\ OWNER OF THE TREE). Nothing to do here: the mapping arrives by inheritance.
: SETUP ( -- )
   GSI-TEST-SETUP
   s" lib/json-write.f" GSI-REQUIRE
   GSI-TEST! ;

public

: RUN ( -- )
   s" stdlib/tail-pure" GSI-GROUP-PAR GSI-GROUP-HEADER
   SETUP
   GSI-FORK-RESET
   s" lib/json-write-test.f" GSI-FORK-INCLUDE
   s" lib/json-read-test.f" GSI-FORK-INCLUDE
   s" lib/json-read-perf-contract-test.f" GSI-FORK-INCLUDE
   s" lib/memory-test.f" GSI-FORK-INCLUDE
   s" lib/vector-test.f" GSI-FORK-INCLUDE
   s" lib/byte-buffer-test.f" GSI-FORK-INCLUDE
   s" test/compiler/ir-id-host.f" GSI-FORK-INCLUDE
   s" test/compiler/ir-id-manifest-host.f" GSI-FORK-INCLUDE
   s" test/compiler/ir-intern-manifest.f" GSI-FORK-INCLUDE
   s" test/compiler/ir-structure-manifest.f" GSI-FORK-INCLUDE
   s" test/compiler/ir-storage-manifest.f" GSI-FORK-INCLUDE
   s" test/compiler/checker-model-manifest.f" GSI-FORK-INCLUDE
   s" test/compiler/insn-manifest.f" GSI-FORK-INCLUDE
   s" test/compiler/reloc-manifest.f" GSI-FORK-INCLUDE
   s" test/compiler/ir-schema.f" GSI-FORK-INCLUDE
   s" test/compiler/ir-op.f" GSI-FORK-INCLUDE
   s" test/compiler/ir-fun.f" GSI-FORK-INCLUDE
   s" test/compiler/ir-build.f" GSI-FORK-INCLUDE
   s" test/compiler/ir-verify.f" GSI-FORK-INCLUDE
   s" test/compiler/target-policy.f" GSI-FORK-INCLUDE
   s" test/compiler/a64-effect.f" GSI-FORK-INCLUDE
   s" test/compiler/ir-arena.f" GSI-FORK-INCLUDE
   s" test/compiler/ir-attr.f" GSI-FORK-INCLUDE
   s" test/compiler/ir-context.f" GSI-FORK-INCLUDE
   s" test/compiler/ir-source.f" GSI-FORK-INCLUDE
   s" test/compiler/ir-symbol.f" GSI-FORK-INCLUDE
   s" test/compiler/ir-type.f" GSI-FORK-INCLUDE
   s" test/compiler/native-tape.f" GSI-FORK-INCLUDE
   s" test/compiler/native-feed.f" GSI-FORK-INCLUDE
   s" test/compiler/native-string.f" GSI-FORK-INCLUDE
   s" test/compiler/native-immediate.f" GSI-FORK-INCLUDE
   s" test/compiler/native-hir.f" GSI-FORK-INCLUDE
   s" test/compiler/native-elaborate.f" GSI-FORK-INCLUDE
   s" test/compiler/asm-package-test.f" GSI-FORK-INCLUDE
   s" test/compiler/native-a64ir.f" GSI-FORK-INCLUDE
   s" test/compiler/native-select.f" GSI-FORK-INCLUDE
   s" test/compiler/native-regalloc.f" GSI-FORK-INCLUDE
   s" test/compiler/native-emit.f" GSI-FORK-INCLUDE
   s" test/compiler/native-publish.f" GSI-FORK-INCLUDE
   \ The terminator that does not return. It is listed here as well as in
   \ test/gate-stdlib-cases.f for the reason given a few lines below: only this
   \ list is what test/run.f actually forks.
   s" test/compiler/native-trap.f" GSI-FORK-INCLUDE
   s" test/compiler/native-migrate.f" GSI-FORK-INCLUDE
   \ The recorder's two ceilings beside it, and the 851-byte definition the old
   \ fixed one refused. Listed here as well as in test/gate-stdlib-cases.f for
   \ the reason given a few lines below: only this list is what test/run.f
   \ actually forks.
   s" test/compiler/native-recorder.f" GSI-FORK-INCLUDE
   \ A quotation through the whole chain and running. It follows the migration
   \ entry because every case is a definition the migration published, and it is
   \ listed here as well as in test/gate-stdlib-cases.f for the reason given
   \ below: only this list is what test/run.f actually forks.
   s" test/compiler/native-quot.f" GSI-FORK-INCLUDE
   \ And what a program does with one: `is` binding it to a deferred word. Same
   \ reason for being in both lists.
   s" test/compiler/native-defer.f" GSI-FORK-INCLUDE
   \ And `execute`. Same reason for being in both lists.
   s" test/compiler/native-exec.f" GSI-FORK-INCLUDE
   s" test/compiler/native-clobber.f" GSI-FORK-INCLUDE
   s" test/compiler/native-inline.f" GSI-FORK-INCLUDE
   \ Which bytes of the code arena a reclamation may hand back. It is listed
   \ here as well as in test/gate-stdlib-cases.f for the reason given below:
   \ only this list is what test/run.f actually forks.
   s" test/code-reclaim.f" GSI-FORK-INCLUDE
   \ Carrying a migration back to callers that already exist, and every reason a
   \ move is refused. It belongs after the publication, clobber and inline leaves
   \ for the reason test/gate-stdlib-cases.f gives: what decides whether a site
   \ may be moved is the row the publication seam recorded, and whether there is
   \ a site to move at all is the inline rule's decision. It is listed here as
   \ well as there because only this list is what test/run.f actually forks - a
   \ suite present only in gate-stdlib-cases.f is reachable by the ALL slice,
   \ which no phase of test/run.f runs, and that is how this file's clobber case
   \ sat red on master while its two neighbours were repaired.
   s" test/compiler/native-reach.f" GSI-FORK-INCLUDE
   s" test/compiler/native-chain.f" GSI-FORK-INCLUDE
   s" test/compiler/native-dead-path.f" GSI-FORK-INCLUDE
   \ The data-stack aliasing witness, listed here as well as in
   \ test/gate-stdlib-cases.f for the reason given above: only this list is what
   \ test/run.f actually forks.
   s" test/compiler/native-dstack-alias.f" GSI-FORK-INCLUDE
   \ The three tag-dispatch forms through the whole chain. It follows the trap
   \ and dead-path leaves because it is their first source-level consumer, and it
   \ is listed here as well as in test/gate-stdlib-cases.f for the reason given
   \ above: only this list is what test/run.f actually forks.
   s" test/compiler/native-match.f" GSI-FORK-INCLUDE
   s" test/compiler/native-rename-rows.f" GSI-FORK-INCLUDE
   \ Loading a whole value out of memory and storing one back, at the width the
   \ checker certified. It follows the rename leaf because its loads produce the
   \ bundles that leaf moves, and it is listed here as well as in
   \ test/gate-stdlib-cases.f for the reason given above: only this list is what
   \ test/run.f actually forks.
   s" test/compiler/native-wide-mem.f" GSI-FORK-INCLUDE
   s" test/compiler/native-vocab.f" GSI-FORK-INCLUDE
   \ The instrument a tail-call lane decides with: a routine's calls and the way
   \ it leaves, read off the emitted code. It belongs beside the chain's own
   \ leaves because one of its fixtures is migrated through the chain.
   s" test/compiler/codegen-tail-probe.f" GSI-FORK-INCLUDE
   \ And the call a routine leaves through while its own names are still
   \ standing, measured with that instrument: the checked accessor's
   \ guard-then-convert shape and the checked constructor's validate-then-MAKE
   \ shape, each against the engine's own compilation. It is listed here as well
   \ as in test/gate-stdlib-cases.f for the reason given above: only this list is
   \ what test/run.f actually forks.
   s" test/compiler/native-tail.f" GSI-FORK-INCLUDE
   \ The multiply-add the chain writes, against the two instructions it replaces,
   \ differentially and to the ends of the signed range.
   s" test/compiler/native-combine.f" GSI-FORK-INCLUDE
   \ The counted loops the chain now answers instead of running, against the same
   \ loops really run by the engine's emitter, plus the eleven shapes it must
   \ refuse. It sits beside the combine leaf because it is the other module-in,
   \ module-out rewrite and its fixtures go through the same migration entry.
   s" test/compiler/native-loop.f" GSI-FORK-INCLUDE
   \ And the plain `do` beside it, for the same reason: the same counted loop
   \ through the same migration entry, with both openers run against the engine
   \ so the limit that equals the start tells them apart.
   s" test/compiler/native-do.f" GSI-FORK-INCLUDE
   \ And `j` beside it, for the same reason and through the same entry: the
   \ index of the counted loop one frame out, measured against the engine's own
   \ `j` because the reader stages no operation and only the answer says which
   \ frame it found.
   s" test/compiler/native-j.f" GSI-FORK-INCLUDE
   \ And the two loop words that landed beside it, listed here as well as in
   \ test/gate-stdlib-cases.f for the reason given above: only this list is what
   \ test/run.f actually forks. `again` closes a loop with no exit and is
   \ measured through what it returns and what it throws; `leave` leaves a
   \ counted loop from the middle and is measured under both openers.
   s" test/compiler/native-again.f" GSI-FORK-INCLUDE
   s" test/compiler/native-leave.f" GSI-FORK-INCLUDE
   \ And `catch` beside them, for the same reason and with the same measurement:
   \ the caught bodies are run against the engine, so the window the chain keeps
   \ across the call is held against the cells the engine really restores.
   s" test/compiler/native-catch.f" GSI-FORK-INCLUDE
   \ And the return-stack transfers, beside both loop leaves for the same reason:
   \ a parked value crosses a loop edge, a join and a call through the same
   \ machinery the data values do, and every case is differential against the
   \ engine through the same migration entry.
   s" test/compiler/native-rstack.f" GSI-FORK-INCLUDE
   \ And the locals groups inside a control structure, beside all three for the
   \ same reason again: a name bound in an arm or a loop body crosses the joins,
   \ the loop edges and the calls those leaves measure, it compiles to no
   \ instruction of its own, and every case goes through the same migration entry.
   s" test/compiler/native-locals-scope.f" GSI-FORK-INCLUDE
   \ And the scope a quotation BODY is built in, beside both: it is where the
   \ catch shape and the locals groups meet, and its cases are differentials
   \ through the same migration entry with every name weighted so an exchange
   \ answers a different number.
   s" test/compiler/native-quot-scope.f" GSI-FORK-INCLUDE
   \ And the instrument the combining lane decides with, beside it for the same
   \ reason: it reads emitted code through that probe's walk, and its rows are
   \ routines the chain really compiled.
   s" test/compiler/codegen-combine-inventory.f" GSI-FORK-INCLUDE
   \ And the instrument the hoisting lane decides with, beside it for the same
   \ reason again: the loops of a routine and the work inside them that does not
   \ depend on the turn, read off the same walk over routines the chain really
   \ compiled.
   s" test/compiler/codegen-loop-inventory.f" GSI-FORK-INCLUDE
   \ And the branch shapes, read off the same walk over routines the chain really
   \ compiled: the branches that go to another branch, and the ones that reach
   \ the instruction already after them.
   s" test/compiler/codegen-branch-inventory.f" GSI-FORK-INCLUDE
   \ And the third instrument built on that same walk: the instructions that
   \ exist only to move arguments and results through the caller's data stack,
   \ split by where they sit. It belongs beside its two named siblings because
   \ all three read emitted code through one walk over routines the chain really
   \ compiled, and because its near-miss cases are decided by the register and
   \ immediate decoders the other two own.
   s" test/compiler/codegen-callsite-inventory.f" GSI-FORK-INCLUDE
   \ Runs the exact half of the code generator comparison - bytes, computed
   \ values, the two generators head to head, the committed table's structure.
   \ The timing column is left out here because this group runs its members in
   \ parallel; see the note on the codegen-compare entry in
   \ test/gate-stdlib-cases.f, and run the timed check by hand.
   s" tools/codegen-compare-test.f" GSI-FORK-INCLUDE
   \ And the third column beside it: the symbol reader the reference column's
   \ bytes come out of, the chain's own committed baseline, and the twins
   \ themselves on the real corpora. No assertion in it reads a clock either.
   s" tools/codegen-compare-clang-test.f" GSI-FORK-INCLUDE
   \ And the reader that ends the comparison's one hand-kept duplicate: the
   \ canonical corpus source, read structurally, so both code generators compile
   \ one text. Its fixtures are sources built to fool a text matcher and it
   \ reads no clock, so it belongs in this parallel group with its two
   \ neighbours.
   s" tools/judge/src-test.f" GSI-FORK-INCLUDE
   \ And the judged table that reader feeds: both code generators over one text,
   \ clang beside them, and the committed artifact. Its chain column holds a
   \ refusal CODE where the chain declined a subject, measured every run, so it
   \ reads no clock either and belongs in this group.
   s" tools/judge-test.f" GSI-FORK-INCLUDE
   \ And the differential oracle: generated straight-line programs through both
   \ code generators from one text, on the ends of the signed range and on
   \ seeded inputs. Its seed is a constant and its assertions are counts, so it
   \ reads no clock and belongs in this group with its neighbours.
   s" tools/judge-fuzz-test.f" GSI-FORK-INCLUDE
   \ Runs the exact half of the end-to-end workload measurement - the engine's
   \ call-or-copy rule read off compiled code, each arm's wiring to its own code
   \ generator's word, and the answers the two arms compute. The deltas are
   \ timings and are left out here for the same reason; run
   \ bin/hb --load tools/codegen-workload.f by hand.
   s" tools/codegen-workload-test.f" GSI-FORK-INCLUDE
   s" test/pointer-storage-test.f" GSI-FORK-INCLUDE
   s" test/ptr-elem-test.f" GSI-FORK-INCLUDE
   s" test/typed-storage-test.f" GSI-FORK-INCLUDE
   s" test/raw-storage-load-seal-test.f" GSI-FORK-INCLUDE
   s" lib/fs-test.f" GSI-FORK-INCLUDE
   s" tools/bootstrap-codegen-test.f" GSI-FORK-INCLUDE
   s" tools/asm-src-test.f" GSI-FORK-INCLUDE
   s" tools/asm-checked-test.f" GSI-FORK-INCLUDE
   s" test/drec-shape-test.f" GSI-FORK-INCLUDE
   s" tools/image-bytes-test.f" GSI-FORK-INCLUDE
   s" tools/codegen-role-test.f" GSI-FORK-INCLUDE
   s" test/icode-fixup-test.f" GSI-FORK-INCLUDE
   s" tools/aot-section-reach-lint-test.f" GSI-FORK-INCLUDE
   s" test/engine-size-test.f" GSI-FORK-INCLUDE
   s" tools/size-report-test.f" GSI-FORK-INCLUDE
   s" tools/ddc-verify-test.f" GSI-FORK-INCLUDE
   s" tools/ddc-scheduled-test.f" GSI-FORK-INCLUDE
   s" test/gate-size-attribution-test.f" GSI-FORK-INCLUDE
   s" tools/include-events-test.f" GSI-FORK-INCLUDE
   s" tools/source-discovery-test.f" GSI-FORK-INCLUDE
   s" tools/event-closure-test.f" GSI-FORK-INCLUDE
   s" lib/unicode/class-test.f" GSI-FORK-INCLUDE
   s" tools/unicode/class-tool-test.f" GSI-FORK-INCLUDE
   s" tools/unicode/class-verify-main.f" GSI-FORK-INCLUDE
   GSI-FORK-DRAIN ;

;package

: GSI-TAIL-RUNNER ( -- )
   s" stdlib/tail-runner" GSI-GROUP-SEQ GSI-GROUP-HEADER
   GSI-TEST-SETUP
   s" lib/test/runner-test.f" GSI-INCLUDE ;

: GSI-TAIL-BUILD ( -- )
   s" stdlib/tail-build" GSI-GROUP-SEQ GSI-GROUP-HEADER
   GSI-TEST-SETUP
   s" lib/build-test.f" GSI-INCLUDE ;

package TAIL-PROCESS

public

: RUN ( -- )
   s" stdlib/tail-process" GSI-GROUP-PAR GSI-GROUP-HEADER
   GSI-TEST-SETUP
   GSI-SETUP!
   s" lib/test/subject.f" GSI-REQUIRE
   s" test/tail-ratchet.f" GSI-REQUIRE
   GSI-TEST!
   GSI-FORK-RESET
   s" tools/hb-cli-contracts-test.f" GSI-FORK-INCLUDE
   s" test/seal.f" GSI-FORK-INCLUDE
   s" test/require-cap-test.f" GSI-FORK-INCLUDE
   s" test/seal-absence.f" GSI-FORK-INCLUDE
   s" test/seal-package.f" GSI-FORK-INCLUDE
   s" test/engine-error-package.f" GSI-FORK-INCLUDE
   s" test/catch-frame.f" GSI-FORK-INCLUDE
   \ test/pre-trust-defer.f is deliberately NOT fork-included: four child-engine
   \ boots (~1s) measured over the fast-tier budget. It runs in the TAIL slice
   \ instead - SUITE-TAIL? in test/gate-stdlib-lib.f selects its label, and
   \ phase 4 of test/run-lib.f spawns that slice. The earlier wording here said
   \ "the standalone stdlib gate runs its registered suite", which named no
   \ scheduled runner at all: the only slice that reaches an unselected label is
   \ the bare (ALL) one, and no phase of test/run.f has ever run it.
   s" test/export-package.f" GSI-FORK-INCLUDE
   \ Forked, not in-process: every case evaluates a source string that defines
   \ global words, and the suite carries the same #FAIL/#CASE/T= vocabulary its
   \ nearest sibling test/using-test.f does. Sharing an in-process group with
   \ another suite that owns those names throws 78 before the first case runs.
   s" test/trust-row-test.f" GSI-FORK-INCLUDE
   s" test/gate-runner-entry-test.f" GSI-FORK-INCLUDE
   s" lib/process-test.f" GSI-FORK-INCLUDE
   s" lib/process-command-test.f" GSI-FORK-INCLUDE
   s" lib/process-pty-handle-test.f" GSI-FORK-INCLUDE
   \ The rest of the process, tasking and standalone-load family, which had been
   \ registered in test/gate-stdlib-cases.f under labels no slice selects. Each
   \ one forks, spawns a child engine, or drives a pty, so it belongs with the
   \ three process members above rather than in a pure group: the fork isolates
   \ its signal handlers, its watchers and its child engines from the runner.
   s" test/atomics-smoke.f" GSI-FORK-INCLUDE
   s" test/run-in-stack-smoke.f" GSI-FORK-INCLUDE
   s" test/getpid-smoke.f" GSI-FORK-INCLUDE
   s" test/proc-watch-smoke.f" GSI-FORK-INCLUDE
   s" test/proc-signal-smoke.f" GSI-FORK-INCLUDE
   s" lib/process-fork-test.f" GSI-FORK-INCLUDE
   s" test/process-pty-io-smoke.f" GSI-FORK-INCLUDE
   \ lib/task-test.f is deliberately NOT here: pthread tasking does not survive a
   \ gate-pool fork. It runs in the tail slice, which spawns a fresh process per
   \ suite; see the note on its registration in test/gate-stdlib-cases.f.
   s" tools/standalone-load-test.f" GSI-FORK-INCLUDE
   s" test/lint-cli-standalone-load.f" GSI-FORK-INCLUDE
   s" tools/object-image-test.f" GSI-FORK-INCLUDE
   s" test/gate-env-stdin-tty-test.f" GSI-FORK-INCLUDE
   \ engine-gate negative regressions: these lived only in gate-stdlib-cases.f
   \ suites whose labels no slice selects, so the full runner never executed
   \ them (gaps audit 2026-07-14). Forked includes isolate their hook installs
   \ and child spawns.
   s" test/internal-word-gate.f" GSI-FORK-INCLUDE
   s" test/underdepth-gate.f" GSI-FORK-INCLUDE
   s" test/match-factor-pin.f" GSI-FORK-INCLUDE
   s" test/immediate-model-test.f" GSI-FORK-INCLUDE
   s" test/top-row-warn-test.f" GSI-FORK-INCLUDE
   s" test/xt-effect-test.f" GSI-FORK-INCLUDE
   s" test/xt-cell-test.f" GSI-FORK-INCLUDE
   s" test/snapshot-xt-cell-decl.f" GSI-FORK-INCLUDE
   \ Forked: every case evaluates a definition into the global wordlist and one
   \ opens a package, so a shared in-process group would meet them twice.
   s" test/does-clause-record.f" GSI-FORK-INCLUDE
   s" test/effect-read-api-test.f" GSI-FORK-INCLUDE
   s" test/create-axiom-test.f" GSI-FORK-INCLUDE
   s" test/checker-assert-test.f" GSI-FORK-INCLUDE
   s" test/checker-dead-path-suite.f" GSI-FORK-INCLUDE
   s" test/checker-verify-pkg-scope.f" GSI-FORK-INCLUDE
   s" test/checker-replay-pkg-state.f" GSI-FORK-INCLUDE
   s" test/prim-link-test.f" GSI-FORK-INCLUDE
   s" test/verify-prim-test.f" GSI-FORK-INCLUDE
   GSI-FORK-DRAIN ;

;package

package STDLIB-INLINE

public

: GSI-LINT-LIBS-CORE ( -- )
   s" stdlib/lint-libs/core" GSI-GROUP-PAR GSI-GROUP-HEADER
   GSI-TEST-SETUP
   GSI-FORK-RESET
   s" lib/string-test.f" GSI-FORK-INCLUDE
   s" lib/utf8-scalar-test.f" GSI-FORK-INCLUDE
   s" lib/ffi-abi-test.f" GSI-FORK-INCLUDE
   s" lib/ieee754-test.f" GSI-FORK-INCLUDE
   s" lib/float32-test.f" GSI-FORK-INCLUDE
   s" lib/float32-buffer-test.f" GSI-FORK-INCLUDE
   s" lib/array-test.f" GSI-FORK-INCLUDE
   s" lib/table-test.f" GSI-FORK-INCLUDE
   s" lib/codegen-test.f" GSI-FORK-INCLUDE
   s" lib/regex-test.f" GSI-FORK-INCLUDE
   s" lib/map-test.f" GSI-FORK-INCLUDE
   \ The rest of the library's own unit tests. Every one of them was registered
   \ in test/gate-stdlib-cases.f under a label no slice predicate selects, so the
   \ only slice that could have reached them is the bare (ALL) one, which no
   \ phase of test/run.f runs - they had been green-by-assumption rather than
   \ green. They are the same kind as their neighbours above (one library file,
   \ no child engine, under a second and a half each), so they join the same
   \ parallel fork group.
   s" lib/ffi-test.f" GSI-FORK-INCLUDE
   s" lib/float-test.f" GSI-FORK-INCLUDE
   s" lib/fmath-test.f" GSI-FORK-INCLUDE
   s" lib/fmt-test.f" GSI-FORK-INCLUDE
   s" lib/sort-test.f" GSI-FORK-INCLUDE
   s" lib/stats-test.f" GSI-FORK-INCLUDE
   s" lib/hashmap-test.f" GSI-FORK-INCLUDE
   s" lib/prelude-test.f" GSI-FORK-INCLUDE
   s" lib/adt/option-test.f" GSI-FORK-INCLUDE
   s" lib/adt/result-test.f" GSI-FORK-INCLUDE
   s" lib/cad-num-arithmetic-test.f" GSI-FORK-INCLUDE
   s" lib/engine-id-test.f" GSI-FORK-INCLUDE
   s" lib/object-cache-test.f" GSI-FORK-INCLUDE
   s" lib/object-index-test.f" GSI-FORK-INCLUDE
   s" lib/object-resolve-test.f" GSI-FORK-INCLUDE
   s" lib/object-link-test.f" GSI-FORK-INCLUDE
   s" lib/layout/box-test.f" GSI-FORK-INCLUDE
   s" lib/test/src-shape-test.f" GSI-FORK-INCLUDE
   GSI-FORK-DRAIN ;

: GSI-LINT-LIBS-PTX ( -- )
   s" stdlib/lint-libs/ptx" GSI-GROUP-SEQ GSI-GROUP-HEADER
   GSI-TEST-SETUP
   s" lib/ptx/header-test.f" GSI-INCLUDE
   s" lib/ptx/kernel-abi-test.f" GSI-INCLUDE
   s" lib/ptx/kernel-manifest-test.f" GSI-INCLUDE
   s" lib/ptx/launch-test.f" GSI-INCLUDE
   s" lib/ptx/rep-test.f" GSI-INCLUDE
   s" lib/ptx/mint-test.f" GSI-INCLUDE
   s" lib/ptx/tile-test.f" GSI-INCLUDE
   s" lib/ptx/tile-loop-test.f" GSI-INCLUDE
   s" lib/ptx/tile-smem-test.f" GSI-INCLUDE
   s" lib/ptx/tile-acc-test.f" GSI-INCLUDE
   s" lib/ptx/gemm-checked-test.f" GSI-INCLUDE
   s" lib/ptx/attention-checked-test.f" GSI-INCLUDE
   s" lib/ptx/attention-roles-test.f" GSI-INCLUDE
   s" lib/ptx/tile-v4-test.f" GSI-INCLUDE
   s" lib/ptx/tile-v4a-test.f" GSI-INCLUDE
   s" lib/ptx/tile-pipe-test.f" GSI-INCLUDE
   s" lib/ptx/cpp-pipe-step-test.f" GSI-INCLUDE
   s" lib/ptx/cpp-slot-test.f" GSI-INCLUDE
   s" lib/ptx/collective-test.f" GSI-INCLUDE
   \ The uniformity and block-uniform barrier model, beside the collectives
   \ because a collective is what a block-uniform barrier orders. Its claims are
   \ positive certifications plus their paired rejections, so it belongs in this
   \ group rather than the negative one, which owns whole files of refusals.
   s" lib/ptx/uniform-barrier-test.f" GSI-INCLUDE
   s" lib/ptx/cg-collective-test.f" GSI-INCLUDE
   s" lib/ptx/cg-activation-test.f" GSI-INCLUDE
   s" lib/ptx/autograd-test.f" GSI-INCLUDE
   s" lib/ptx/ir-test.f" GSI-INCLUDE
   s" lib/ptx/opt-ir-test.f" GSI-INCLUDE
   s" lib/ptx/opt-test.f" GSI-INCLUDE
   s" lib/ptx/ad-test.f" GSI-INCLUDE
   s" lib/ptx/ad-dag-test.f" GSI-INCLUDE
   s" lib/ptx/ad-dag-eval-test.f" GSI-INCLUDE
   s" lib/ptx/ad-saved-test.f" GSI-INCLUDE ;

: GSI-LINT-LIBS-PTX-NEG ( -- )
   s" stdlib/lint-libs/ptx-neg" GSI-GROUP-SEQ GSI-GROUP-HEADER
   GSI-TEST-SETUP
   s" lib/ptx/rep-neg-test.f" GSI-INCLUDE
   s" lib/ptx/mint-neg-test.f" GSI-INCLUDE
   s" lib/ptx/tile-loop-neg-test.f" GSI-INCLUDE
   s" lib/ptx/tile-smem-neg-test.f" GSI-INCLUDE
   s" lib/ptx/tile-acc-neg-test.f" GSI-INCLUDE
   s" lib/ptx/tile-v4a-neg-test.f" GSI-INCLUDE
   s" lib/ptx/tile-pipe-neg-test.f" GSI-INCLUDE
   s" lib/ptx/cpp-slot-neg-test.f" GSI-INCLUDE
   s" lib/ptx/cg-mma-slot-neg-test.f" GSI-INCLUDE
   s" lib/ptx/gemm-checked-neg-test.f" GSI-INCLUDE
   s" lib/ptx/attention-checked-neg-test.f" GSI-INCLUDE
   \ The gradient-extent refusals: a mismatched gradient span must not certify.
   \ Same kind as the rest of this group - a file whose subject is what the
   \ checker rejects - so it belongs here and not with the positive tiles.
   s" lib/ptx/autograd-neg-test.f" GSI-INCLUDE ;

: GSI-LINT-LIBS-PTX-TOOL ( -- )
   s" stdlib/lint-libs/ptx-toolchain" GSI-GROUP-SEQ GSI-GROUP-HEADER \ ( -- )
   GSI-TEST-SETUP \ ( -- )
   s" lib/ptx/toolchain-test.f" GSI-INCLUDE \ ( -- )
   s" tools/ptx/profile-test.f" GSI-INCLUDE \ ( -- )
   s" tools/ptx/bench-test.f" GSI-INCLUDE \ ( -- )
   s" tools/ptx/saxpy-test.f" GSI-INCLUDE \ ( -- )
   s" tools/ptx/kernel-export-test.f" GSI-INCLUDE \ ( -- )
   s" tools/ptx/perf-registry-test.f" GSI-INCLUDE \ ( -- )
   s" tools/ptx/autotune-test.f" GSI-INCLUDE \ ( -- )
   s" tools/ptx/perf-compare-test.f" GSI-INCLUDE \ ( -- )
   s" tools/ptx/perf-regress-test.f" GSI-INCLUDE \ ( -- )
   s" tools/kernel-perf-lint-test.f" GSI-INCLUDE \ ( -- )
   s" tools/ptx/fusion-emit-test.f" GSI-INCLUDE \ ( -- )
   s" tools/ptx/device-gold-test.f" GSI-INCLUDE \ ( -- )
   s" tools/ptx/cuda-scope-leak-proof-test.f" GSI-INCLUDE \ ( -- ) host-only CUDA-lifecycle leak proof
   s" tools/ptx/attention-bench-test.f" GSI-INCLUDE ; \ ( -- )
\ NOTE: the device/bench tools (bandwidth-lib-test, fusion-compare, gemm-bench, attention-bench)
\ run only in the ptx-toolchain suite of the MANUAL lint-libs slice
\ (`bin/hb --load test/gate-stdlib.f -- lint-libs`, a documented merge gate -
\ test/run.f does NOT schedule the spawned slices), where they compile-check +
\ device-SKIP in a fresh image. Loaded into the resident full-runner image they
\ SIGBUS, so the inprocess list carries the unit tests + the substantive perf
\ regression scan. That scan runs via perf-regress-test.f (an argv-free checked
\ fixture: committed-registry PERF:LOAD + PERF:SCAN); the CLI tools/ptx/perf-regress.f
\ resolves its registry path from ambient SCRIPT-ARGV, so in the resident image it
\ would mis-read the harness argv as a path - it is spawn-only, run in a fresh
\ image with clean argv. The spawned list stays a superset. Retire the duplication
\ + give the bench compile-checks a scheduled runner per
\ habu-derive-inprocess-spawned-a54e760d.

: GSI-LINT-ARTIFACTS-FAST ( -- )
   s" stdlib/lint-artifacts/fast" GSI-GROUP-SEQ GSI-GROUP-HEADER
   GSI-TEST-SETUP
   s" tools/lint/text-foundation-test.f" GSI-INCLUDE
   s" tools/json-file-test.f" GSI-INCLUDE
   s" tools/sha256-file-test.f" GSI-INCLUDE
   s" lib/content-key-test.f" GSI-INCLUDE
   s" test/run-closure-lint-test.f" GSI-INCLUDE
   s" test/run-result-cache-test.f" GSI-INCLUDE
   s" test/run-rerun-failed-test.f" GSI-INCLUDE
   s" test/golden-test.f" GSI-INCLUDE
   s" tools/diagnose-hb-test.f" GSI-INCLUDE
   s" lib/object-test.f" GSI-INCLUDE ;

;package

package CHECK-CLI-GATE
public
s" RUN" get-current REQUIRE-WORD
s" REQUIRE-WORD" get-current REJECT-WORD
s" REJECT-WORD" get-current REJECT-WORD
private
s" RUN" get-current REJECT-WORD
s" GSI-CHECK-CLI" 0 REJECT-WORD
;package

' CHECK-CLI-GATE:RUN drop
