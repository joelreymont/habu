STDLIB-GATE:MAIN

using TEST

SUITE shadow-lint
   tools/lint/shadow-lint.f
   tools/lint/shadow-lint-test.f
;SUITE

SUITE clobber-lint
   tools/lint/clobber-lint.f
;SUITE

SUITE clobber-lint-fixtures
   tools/lint/clobber-lint-test.f
;SUITE

SUITE repl-lint
   tools/repl-lint.f
;SUITE

SUITE ptx-emitter-lint
   tools/lint/ptx-emitter-lint.f
   tools/lint/ptx-emitter-lint-test.f
;SUITE

SUITE stdin-closure-lint
   tools/stdin-closure-lint.f
;SUITE

SUITE gate-stats
   test/gate-stats-test.f
;SUITE

SUITE dot-dep-lint
   tools/dot-dep-lint.f
;SUITE

SUITE dot-dep-lint-fixtures
   tools/dot-dep-lint-test.f
;SUITE

SUITE maki-dep-lint
   tools/maki-dep-lint.f
;SUITE

SUITE maki-dep-lint-fixtures
   tools/maki-dep-lint-test.f
;SUITE

SUITE namespace-lint
   tools/namespace-lint.f
;SUITE

SUITE namespace-lint-fixtures
   tools/namespace-lint-test.f
;SUITE

SUITE package-diff-lint-fixtures
   tools/package-diff-lint-test.f
;SUITE

SUITE error-code-lint
   tools/error-code-lint.f
;SUITE

SUITE error-code-lint-fixtures
   tools/error-code-lint-test.f
;SUITE

\ The scheduling closure over this very file: every registration below must be
\ reachable by a slice predicate in test/gate-stdlib-lib.f or by a gate fork
\ list under test/. The tool derives both sides from those real sources through
\ the shared lexer, so a registration nobody scheduled reds the gate instead of
\ sitting dark. It runs in the lint-tools body beside the other lint entries.
SUITE schedule-lint
   tools/lint/schedule-lint.f
   tools/lint/schedule-lint-test.f
;SUITE

SUITE text-foundation-fixtures
   tools/lint/text-foundation-test.f
;SUITE

SUITE lint-def-fixtures
   tools/lint/def-test.f
;SUITE

SUITE lint-intern-set
   tools/lint/set-test.f
;SUITE

SUITE diff-parser
   tools/lint/diff-test.f
;SUITE

SUITE diff-frame-codec
   tools/lint/diff-frame-test.f
;SUITE

SUITE json-file-cursor
   tools/json-file-test.f
;SUITE

SUITE imgdump-compare
   tools/imgdump-test.f
;SUITE

SUITE imagedisasm-tool
   tools/imagedisasm-test.f
;SUITE

\ The two tools that REPORT over the tree rather than judging one file, so they
\ share the suite that has no skip flag: a report nobody scheduled is a report
\ nobody reads. The native-chain census belongs here and not with the lints - it
\ makes no findings and passes no verdict, it measures how much of the tree the
\ chain can compile, and its own suite label would need the slice tables in
\ test/gate-stdlib-lib.f and test/gate-runner-lib.f to name it.
SUITE tool-boundary-aot-call
   tools/aot-call-report-test.f
   tools/chain-census-test.f
;SUITE

SUITE tool-boundary-check-repair
   tools/check-all-errors-test.f
   tools/repair-packet-test.f
;SUITE

SUITE tool-boundary-doc-public
   tools/public-signatures-test.f
   tools/public-signatures-bracket-test.f
   tools/repair-schema-doc-test.f
   tools/examples-test.f
;SUITE

SUITE tool-boundary-lints
   tools/repl-lint-test.f
   tools/diag-origin-test.f
   tools/aot-lint-test.f
   tools/signature-lint-test.f
   tools/checked-boundary-lint-test.f
   tools/reserved-name-lint-test.f
   tools/duplicate-definition-lint-test.f
   tools/bundle-lib-test.f
   tools/json-only-test.f
;SUITE

SUITE tool-boundary-typed-local
   tools/typed-local-diff-lint-test.f
;SUITE

SUITE check-cli-boundary
   tools/check-test.f
;SUITE

SUITE streaming-sha256
   tools/sha256-file-test.f
;SUITE

SUITE content-key-cache
   lib/content-key-test.f
;SUITE

SUITE engine-identity
   lib/engine-id-test.f
;SUITE

SUITE compiler-ir-id
   test/compiler/ir-id-host.f
;SUITE

SUITE compiler-ir-id-manifest
   test/compiler/ir-id-manifest-host.f
;SUITE

SUITE compiler-ir-intern-manifest
   test/compiler/ir-intern-manifest.f
;SUITE

\ The other four proof manifests, registered here beside the two above for the
\ same reason those two are: a manifest is the cheap resident half of a proof
\ family - it asserts the obligation rows and their schema without spawning Rocq,
\ so it belongs in the standalone gate as well as in the resident fork list,
\ where all six already sit. Only ir-id and ir-intern had rows; the other four
\ were fork-only, which meant `test/gate-stdlib.f` run on its own checked two
\ manifests out of six. The proof gates they belong to stay cases-only and say
\ why further down.
SUITE compiler-ir-structure-manifest
   test/compiler/ir-structure-manifest.f
;SUITE

SUITE compiler-ir-storage-manifest
   test/compiler/ir-storage-manifest.f
;SUITE

SUITE compiler-checker-model-manifest
   test/compiler/checker-model-manifest.f
;SUITE

SUITE compiler-insn-manifest
   test/compiler/insn-manifest.f
;SUITE

SUITE compiler-reloc-manifest
   test/compiler/reloc-manifest.f
;SUITE

SUITE compiler-ir-schema
   test/compiler/ir-schema.f
;SUITE

SUITE compiler-ir-op
   test/compiler/ir-op.f
;SUITE

SUITE compiler-ir-fun
   test/compiler/ir-fun.f
;SUITE

SUITE compiler-ir-build
   test/compiler/ir-build.f
;SUITE

SUITE compiler-ir-verify
   test/compiler/ir-verify.f
;SUITE

SUITE compiler-ir-arena
   test/compiler/ir-arena.f
;SUITE

SUITE compiler-ir-attr
   test/compiler/ir-attr.f
;SUITE

SUITE compiler-ir-context
   test/compiler/ir-context.f
;SUITE

SUITE compiler-ir-source
   test/compiler/ir-source.f
;SUITE

SUITE compiler-ir-starve
   test/compiler/ir-starve-test.f
;SUITE

SUITE compiler-ir-symbol
   test/compiler/ir-symbol.f
;SUITE

SUITE compiler-ir-type
   test/compiler/ir-type.f
;SUITE

SUITE compiler-native-tape
   test/compiler/native-tape.f
;SUITE

SUITE compiler-native-feed
   test/compiler/native-feed.f
;SUITE

SUITE compiler-native-string
   test/compiler/native-string.f
;SUITE

\ The reader that makes the comparison's two columns compile ONE text. The
\ comparison used to keep every subject twice - a real definition in the corpus
\ file and a hand-retyped string literal beside it - and two texts that are
\ supposed to be the same program is a claim nothing checked. Its fixtures are
\ sources built to fool a reader that searched for text: the definition hidden
\ in a comment, in a string, the `;]` that closes a quotation rather than the
\ definition, the corpus name written in the signature instead of the body, and
\ the name defined twice. The last group runs the shipped file entry over a real
\ corpus in this repository and pins the derived text against the program the
\ retyped column carried. Nothing it asserts reads a clock.
SUITE judge-src
   tools/judge/src-test.f
;SUITE

\ The reference column's symbol reader, which is where every `clang` cell of the
\ judged artifact comes from. It is driven through CODEGEN-MACHO:LOAD-FROM - the
\ word the judge's own pass calls, with the two listings as parameters - over
\ listings built to fool it: a non-external symbol whose linkage word ENDS in
\ the word a substring match would find, a symbol whose NAME is that word, a
\ symbol in another section of the same segment, symbols in nm's name order
\ rather than address order, and a section line naming __text in the wrong
\ segment. Then the real object is read and its symbol sizes are required to
\ tile its text section exactly. A reader fooled by one line would report a
\ wrong number for every row and nothing else would notice, because the judge
\ would commit that column and then agree with itself about it. A host with no C
\ compiler runs the fixtures and says so about the object; it does not fail.
\ Nothing it asserts reads a clock.
SUITE judge-ref
   tools/judge/ref-test.f
;SUITE

\ And the reader that says which WAY the judged artifact moved. The byte-for-byte
\ check says the tree and the committed file differ and where; it cannot say
\ whether the chain got smaller or bigger, and those are two different events.
\ This member states small tables, renders them through the report the command
\ prints, reads them back and adjudicates: bigger is a regression, smaller is
\ progress, the engine moving either way is a finding, and a row on one side and
\ not the other is a finding. Its fixtures are artifacts built to fool a reader
\ that searched for text - a subject named inside a sentence, a row written
\ twice, a verdict word that is not one, a field missing, a declared count that
\ disagrees, and cost lines under the marker that must never be read as rows.
\ It compiles no corpus and reads no clock.
SUITE judge-base
   tools/judge/base-test.f
;SUITE

\ And the judged table itself: every subject of a corpus compiled through both
\ code generators from ONE text, with clang beside them, and the committed
\ artifact that says what this tree emits. It runs the same words
\ `bin/hb --load tools/judge.f -- --check` drives, so what passes here is what
\ that command does. What it pins that the old comparison could not: every
\ subject the chain declines is checked against the CODE the compiler refused it
\ with, measured this run, rather than against a list of their names, and a
\ refusal for a reason nobody has named fails the member. Nothing it asserts
\ reads a clock.
SUITE judge
   tools/judge-test.f
;SUITE

\ And the differential oracle beside it: straight-line integer programs nobody
\ wrote, generated from a CONSTANT seed, compiled by both code generators from
\ one text and required to answer the same cell on the ends of the signed range
\ and on generated inputs. It runs a small fixed number of the same programs the
\ hand-run sweep `bin/hb --load tools/judge-fuzz.f` runs, in the same order, so
\ this member is a prefix of that sweep. It also proves the comparison can SEE a
\ difference, by handing the two columns two texts that differ by one literal.
\ The seed is a constant and no assertion is a duration, so it reads no clock.
SUITE judge-fuzz
   tools/judge-fuzz-test.f
;SUITE

\ The fork question, which used to be a third member of the registration above
\ and was the one member nothing scheduled. It is its own registration because it
\ needs a runner the other two do not: its first claim is that PROC-FORK:CHILD?
\ is FALSE in the process that maps the reference column, so it has to BE the
\ forking process. Put in the tail-pure fork group it runs as a forked child
\ already and that claim inverts (measured: expected false got true). The tail
\ slice spawns one fresh process per suite, which is the shape it asks for.
SUITE codegen-fork-reference
   test/codegen-fork-reference-test.f
;SUITE

\ The end-to-end workload measurement, and the same division of labour. Its
\ member checks the facts the measurement's numbers would be meaningless
\ without: the engine's own call-or-copy rule, read off compiled code and pinned
\ REASON BY REASON - a straight-line body at the size limit and one instruction
\ over it, on both sides of the two separate size tests C-CALL makes; a patched
\ return slot against an unpatched one compiled from the same six instructions;
\ and one body per refusal class whose only unmovable instruction is of that
\ class; that each arm's driver enters ITS OWN code generator's word and not the
\ other one's - the mutation a two-arm timing turns on, because an after-arm
\ still calling the before-arm's record reports a delta of nothing and looks
\ healthy; that the two arms of every workload compute the same pinned answer;
\ that the two arms of one workload body come out the same number of bytes,
\ because a name lives in a dictionary record and not in a compiled body; and
\ that a row's delta keeps its sign while a row's two columns keep their arms.
\
\ NO ASSERTION THE MEMBER RUNS READS A CLOCK, for the reason the entry above
\ gives. It does measure a small family of rows of its own, through the store
\ every reported row goes through, but it reads their ANSWERS, their kinds and
\ the shape of their runs, never their times. The deltas themselves - what the
\ new code generator is worth to a program, and which of them clear the bar
\ their own family's null draws set - are printed by
\ bin/hb --load tools/codegen-workload.f, run by hand on a quiet machine. The
\ one claim that genuinely needs two arms to have taken measurably different
\ times - that the column a row calls old holds the arm handed to it as old - is
\ in tools/codegen-workload-timed-test.f, run by hand beside that entry and
\ listed in no suite, exactly as tools/judge-timed.f is. The
\ member is mirrored into the resident stdlib/tail-pure fork group, so it is
\ scheduled rather than run by hand.
SUITE codegen-workload
   tools/codegen-workload-test.f
;SUITE

\ Where the register allocator's spill wall is, measured through the real
\ migration entry. It is its own member because it migrates definitions of its
\ own and requires the fourth corpus and that corpus's migration to get a
\ callee - CODEGEN-CORPUS4:C-LONG for the engine's arm and C-LONG-N for the
\ chain's. No assertion it makes reads a clock - every one of them is a throw
\ code from the chain - so scheduling it schedules no flake. It runs in the
\ proof slice with the parity gates: at 58s through the real runner it is a
\ minute-scale member, not a fast-tier one.
SUITE codegen-spill-probe
   tools/codegen-spill-probe.f
;SUITE

SUITE compiler-native-hir
   test/compiler/native-hir.f
;SUITE

SUITE compiler-native-elaborate
   test/compiler/native-elaborate.f
;SUITE

SUITE compiler-asm-package
   test/compiler/asm-package-test.f
;SUITE

SUITE compiler-native-a64ir
   test/compiler/native-a64ir.f
;SUITE

\ The typed ARM64 routine-effect schema, next to the a64 lowering it constrains.
\ It was fork-only, so the register bounds it pins were unchecked in a standalone
\ gate run.
SUITE compiler-a64-effect
   test/compiler/a64-effect.f
;SUITE

\ The target/policy binding: src/compiler/digest.f, target.f, numeric-policy.f
\ and binding.f through their public words. It is the acceptance suite
\ habu-bind-compiler-target-b3dfa307 is answered by, and a suite that answers a
\ dot has to be reachable by name in the registry, not only inside a fork list -
\ that missing row is what blocked the dot.
SUITE compiler-target-policy
   test/compiler/target-policy.f
;SUITE

SUITE compiler-native-select
   test/compiler/native-select.f
;SUITE

SUITE compiler-native-regalloc
   test/compiler/native-regalloc.f
;SUITE

SUITE compiler-native-emit
   test/compiler/native-emit.f
;SUITE

\ The publication seam: what a republished dictionary record holds, and what the
\ seam refuses. It runs before the chain's own end-to-end suite because a
\ republication it got wrong would show up there as a word that computes the
\ wrong thing several stages away from the cause.
SUITE compiler-native-publish
   test/compiler/native-publish.f
;SUITE

\ The terminator that does not return: the family table and the one shared
\ routine a compiled trap branches to, the exit-block rule every pass re-derives,
\ and the length the seam records for a routine that leaves by branching. It runs
\ after the publication seam because its last case publishes a trapping routine
\ over a word and calls it in a child, which is the whole path in one measurement.
SUITE compiler-native-trap
   test/compiler/native-trap.f
;SUITE

\ The production entry: a definition the engine compiles, recompiled by the
\ chain and republished under its own name, plus what happens to a word the
\ chain cannot compile.
SUITE compiler-native-migrate
   test/compiler/native-migrate.f
;SUITE

\ The same entry taking its definition off the input stream instead of out of a
\ string: where the engine's own reader says the definition ended, and the byte
\ the interpreter is put back at. It runs beside the migration entry because the
\ tape, the elaboration and the publication below it are the same ones.
SUITE compiler-native-stream
   test/compiler/native-stream.f
;SUITE

\ The two ceilings that entry opens a recording unit with, neither of which is a
\ number this tree picks any more: the byte ceiling is the engine's own body
\ capture and the tape is sized from the source. It runs beside the migration
\ entry because both cases go through it, and it ends with the 851-byte
\ definition the old 512-byte ceiling refused, run against the engine.
SUITE compiler-native-recorder
   test/compiler/native-recorder.f
;SUITE

\ A quotation, through the whole chain and running: the body compiled as a second
\ function of the same emission, the Adr that names it decoded out of the
\ published bytes, and the address executed. It runs after the migration entry
\ because every case is a definition the migration published.
SUITE compiler-native-quot
   test/compiler/native-quot.f
;SUITE

\ What `is` becomes: the quotation bound to a deferred word through the engine's
\ own store-and-declare primitive, the branch decoded out of the published
\ bytes, and the deferred word dispatching to the body afterwards. It runs after
\ the migration entry for the same reason the quotation suite does.
SUITE compiler-native-defer
   test/compiler/native-defer.f
;SUITE

\ And what `execute` becomes: a call to the engine's own execute with the arity
\ the quotation's certified effect states, over both paths a quotation reaches
\ one by, plus the library's own multishot site re-compiled and run.
SUITE compiler-native-exec
   test/compiler/native-exec.f
;SUITE

\ What a published routine destroys, and what a call site does with the answer.
\ It runs after the migration entry because the measurement it makes is over two
\ words the migration published.
SUITE compiler-native-clobber
   test/compiler/native-clobber.f
;SUITE

\ The body of a small routine, recorded when it is published and copied into
\ every later caller instead of being called. It runs after the migration entry
\ for the reason the clobber suite does: what it measures is the code of words
\ the migration published.
SUITE compiler-native-inline
   test/compiler/native-inline.f
;SUITE

\ Which bytes of the code arena a reclamation may hand back. It runs after the
\ publication seam and the two address-keyed records because the case that says
\ a floor is wrong needs a republished routine to be wrong ABOUT, and the case
\ that says the floor still moves reads the publication log row that goes with
\ the routine it gives back.
SUITE code-reclaim
   test/code-reclaim.f
;SUITE

\ Carrying a migration back to the callers that were compiled before it: the
\ call instructions already in the image, moved onto the routine the chain
\ published, and every reason a move is refused. It runs after the publication
\ and clobber suites because what decides whether a site may be moved at all is
\ the row the publication seam recorded for the routine it is moved onto.
SUITE compiler-native-reach
   test/compiler/native-reach.f
;SUITE

\ Reading a published routine's calls and its exit off the emitted code: the
\ instrument a tail-call lane decides with. It runs beside the reach suite
\ because both consume src/compiler/native/branch.f, the chain's one reader of a
\ branch displacement.
SUITE compiler-codegen-tail-probe
   test/compiler/codegen-tail-probe.f
;SUITE

\ The call a routine leaves through while its own names are still standing - the
\ checked accessor's guard-then-convert shape and the checked constructor's
\ validate-then-MAKE shape, which between them are most of what the tree writes.
\ Every case is differential against the engine's own compilation, because what a
\ tail site leaves unpublished cannot be seen in the shape of the code and only
\ the ANSWER separates a site that dropped what it needed from one that did not.
\ It runs beside the tail probe because it uses that instrument to say the branch
\ is really there.
SUITE compiler-native-tail
   test/compiler/native-tail.f
;SUITE

\ The multiply-add the chain writes, held against the two instructions it
\ replaces: the same source compiled by the engine's emitter, which never fuses,
\ and by the chain, which does, run against each other to the ends of the signed
\ range. It runs beside the chain's own leaves because its fixtures go through
\ the production migration entry.
SUITE compiler-native-combine
   test/compiler/native-combine.f
;SUITE

\ The counted loops the chain now answers instead of running: the same source
\ compiled by the engine's emitter, which really runs every turn, and by the
\ chain, which does not, run against each other - including three trip counts no
\ loop could be run at. Its other half is the eleven shapes the pass must refuse,
\ each of which still has to hold its loop in the emitted code. It runs beside
\ the combine suite because it is the other module-in, module-out rewrite and its
\ fixtures go through the same production migration entry.
SUITE compiler-native-loop
   test/compiler/native-loop.f
;SUITE

\ The plain `do`, which is `?do` without the test that skips an empty loop. Every
\ case runs both openers against the engine's own compilation of the same text,
\ because the one pair that tells them apart is the limit that equals the start -
\ one turn against none. It runs beside the loop suite because it is the same
\ counted loop measured through the same migration entry.
SUITE compiler-native-do
   test/compiler/native-do.f
;SUITE

\ `j`, the index of the counted loop one frame further out. It runs beside the
\ plain `do` because it is the same counted loop measured through the same
\ migration entry, and every case is differential for a reason of its own: `j`
\ stages no operation, so a chain that answered with the inner loop's index, the
\ outermost loop's, or an enclosing `if`'s frame emits exactly the same code and
\ only the ANSWER tells them apart.
SUITE compiler-native-j
   test/compiler/native-j.f
;SUITE

\ `begin … again`, the third closer of a `begin` loop and the one whose loop has
\ no exit. Its cases run the loop against the engine's own compilation of the
\ same text - through an `exit` where the word returns and through the code it
\ throws where it does not - so the number of turns is compared and not only the
\ shape. It runs beside the `do` and `j` suites because it is another loop word
\ the dialect gained and it goes through the same migration entry.
SUITE compiler-native-again
   test/compiler/native-again.f
;SUITE

\ `leave`, which leaves a counted loop from the middle. Every case runs both
\ openers against the engine, because the pair that tells them apart is the
\ limit equal to the start - the one turn a `leave` can fire on under `do` and
\ cannot under `?do`. It runs beside the again suite for the same reason.
SUITE compiler-native-leave
   test/compiler/native-leave.f
;SUITE

\ The return-stack transfers, which compile to nothing at all: `>r` moves a value
\ id between two COMPILE-TIME vectors and emits no instruction. Every case is
\ differential against the engine's own compilation, because nothing about the
\ emitted code says which vector a value came from and only the answer does. It
\ runs beside the loop suites because its hardest seams are theirs - a parked
\ value crosses a join, a loop edge and a call by the same machinery the data
\ values do.
SUITE compiler-native-rstack
   test/compiler/native-rstack.f
;SUITE

\ `catch`, which runs a quotation and comes back either way. Its cases run the
\ caught bodies against the engine's own compilation of the same text, because
\ what has to be proved is an ANSWER and not a shape: the engine puts the stack
\ back to its DEPTH on a throw and never to its CONTENTS, so a chain that kept
\ the caught window in a register answers the value the site had before and
\ every block count still agrees. It runs beside the leave suite because it is
\ the other control word the dialect gained and it goes through the same
\ migration entry.
SUITE compiler-native-catch
   test/compiler/native-catch.f
;SUITE

\ The locals groups that open and close INSIDE a control structure, which compile
\ to nothing at all in the same way the return-stack transfers do: `:}` moves
\ value ids into named slots and emits no instruction, so only the answer says
\ which name a slot held. Every case is differential against the engine's own
\ compilation for that reason, and it runs beside the loop and return-stack
\ suites because its seams are theirs - a name crosses a join, a loop edge, an
\ arm and a call by the machinery those two already measure, and the two
\ re-resolution rows prove a name out of scope is the WORD or the loop INDEX the
\ engine gives it rather than a refusal.
SUITE compiler-native-locals-scope
   test/compiler/native-locals-scope.f
;SUITE

\ The scope a quotation BODY is built in, which is nobody's but its own. It runs
\ beside the catch and locals suites because it is their intersection: the
\ production catch shape is a definition with a group around a body that calls,
\ and until the body stopped inheriting the enclosing routine's local scope that
\ shape could not be compiled at all. Every case is differential against the
\ engine's own compilation, and every name is weighted with its own odd factor,
\ because what a body carried wrongly across its call comes back as a wrong
\ NUMBER rather than as a wrong shape.
SUITE compiler-native-quot-scope
   test/compiler/native-quot-scope.f
;SUITE

\ Counting the instruction PAIRS one three-source instruction would replace,
\ which is the measurement the combining lane decides what to build from. It
\ runs beside the tail probe because it reads emitted code through that tool's
\ walk, and its classifiers are held against the shipped encoders in
\ src/arch/arm64/asm.f - including the multiply-add alias that makes MUL and
\ MADD the same word.
SUITE compiler-codegen-combine-inventory
   test/compiler/codegen-combine-inventory.f
;SUITE

\ Counting the LOOPS the chain's compilations hold and the work inside them that
\ does not depend on the turn, which is the measurement the hoisting lane decides
\ what to build from. It runs beside the combining inventory because it reads
\ emitted code through the same walk and borrows that tool's register-field
\ decoders. Its structural cases are built from the routine that caught the bug
\ the first version had - a return block laid out inside a loop's span, whose
\ backward branch is an ordinary forward edge.
SUITE compiler-codegen-loop-inventory
   test/compiler/codegen-loop-inventory.f
;SUITE

\ Counting the branches that go to another branch, which is what the collapse
\ lane removes, and the branches that reach the instruction already after them,
\ which is what the fall-through rule still misses across a block that emits
\ nothing. The suite is built around the four ways such a reader answers
\ confidently and wrongly - a call read as a branch, a walk past the end of a
\ routine that leaves by one, a branch followed out of the routine, and a
\ conditional target called a chain - each pinned by a row that really is that
\ shape and asserts it before asking for the count.
SUITE compiler-codegen-branch-inventory
   test/compiler/codegen-branch-inventory.f
;SUITE

\ Counting the instructions that exist only to move arguments and results through
\ the caller's data stack, split by WHERE they sit: beside a call, which a
\ register calling convention could remove, against the routine's own entry and
\ exit, which it could not while every published routine is a dictionary record
\ the engine can enter. It runs beside the other two inventories because it reads
\ emitted code through the same walk and borrows the same register and immediate
\ decoders. Its near-miss cases are the ones that matter: the frame's own
\ adjustment is the same instruction form over register 31, and the scaled load
\ sits at the same registers and offset as the unscaled one.
SUITE compiler-codegen-callsite-inventory
   test/compiler/codegen-callsite-inventory.f
;SUITE

\ The native chain's end-to-end run: source text through the real compile path
\ to executed machine code. It runs after the leaves it composes, so a red here
\ with green leaves means the leaves disagree with each other.
SUITE compiler-native-chain
   test/compiler/native-chain.f
;SUITE

SUITE native-dead-path
   test/compiler/native-dead-path.f
;SUITE

\ What the data-stack residency answers when a checked program stores through an
\ address that lands in the routine's own slots. It runs after the chain leaves
\ because it executes migrated bodies on a data stack of its own, which is the
\ end-to-end path those leaves prove one stage at a time.
SUITE native-dstack-alias
   test/compiler/native-dstack-alias.f
;SUITE

\ The three tag-dispatch forms - `MATCH`, `case` and `construct` - from source
\ text to executed machine code, every case comparing the chain's answer with the
\ engine's on the same body. It runs after the trap leaf and the dead-path leaf
\ because it is their first source-level consumer: a dispatch's mismatch edge IS
\ the trap, and an arm that throws IS a dead path.
SUITE compiler-native-match
   test/compiler/native-match.f
;SUITE

\ A rename is a permutation of the compile-time value vector, and that vector
\ counts CELLS while the language counts VALUES. This suite is the differential
\ that holds the two together over a value wider than a cell: every case states
\ one body twice, once for the engine and once for the chain, and compares what
\ they leave rather than what anybody expected them to leave.
SUITE compiler-native-rename-rows
   test/compiler/native-rename-rows.f
;SUITE

\ Getting a value wider than a cell ONTO that vector and back into memory, which
\ is the other half of the same story: `@` through a pointer to a multi-cell
\ family reads the whole value and `!` writes it, at the width the checker
\ certified. It runs after the rename leaf because its loads produce exactly the
\ bundles a rename moves, and after the match leaf because one of its cases feeds
\ a dispatch out of memory. The cell ORDER is measured by crossing the two
\ compilers - what one writes, the other reads back.
SUITE compiler-native-wide-mem
   test/compiler/native-wide-mem.f
;SUITE

\ The same end-to-end run over the comparison and bitwise vocabulary, word by
\ word, each answer compared with the interpreted word's. It runs beside the
\ chain suite because it is the same path with a wider source vocabulary.
SUITE compiler-native-vocab
   test/compiler/native-vocab.f
;SUITE

\ The identity parity gate compiles formal/Common with the Rocq proof assistant
\ and spawns child engines, so it runs in the PROOF slice - SUITE-PROOF? in
\ test/gate-stdlib-lib.f selects it, and phase 40 of test/run-lib.f spawns that
\ slice - and is not mirrored into the resident fast tier. The seven proof gates
\ and the spill probe below share that slice; it is scheduled first in the early
\ order because the instruction gate is the run's long pole.
\
\ The earlier wording on these eight entries said they "run here in the
\ standalone stdlib gate", which named no runner: the only slice that reaches a
\ label no predicate selects is the bare (ALL) one, and no phase of test/run.f
\ has ever run it. They were unscheduled, not deferred.
SUITE compiler-ir-id-proof
   test/compiler/ir-id-proof-host.f
;SUITE

\ The interning parity gate compiles formal/Common/Interning.v with the Rocq
\ proof assistant for the same reason, so it runs in the proof slice alongside
\ its sibling.
SUITE compiler-ir-intern-proof
   test/compiler/ir-intern-proof.f
;SUITE

\ The structure parity gate compiles formal/Common/Structure.v with the Rocq
\ proof assistant for the same reason, so it runs in the proof slice alongside
\ its two siblings.
SUITE compiler-ir-structure-proof
   test/compiler/ir-structure-proof.f
;SUITE

\ The storage and lifetime parity gate compiles formal/Common/Storage.v with the
\ Rocq proof assistant for the same reason, so it runs in the proof slice
\ alongside its two siblings.
SUITE compiler-ir-storage-proof
   test/compiler/ir-storage-proof.f
;SUITE

\ The checker model parity gate compiles formal/Common/Effects.v and
\ formal/Common/Control.v with the Rocq proof assistant for the same reason, so
\ it runs in the proof slice alongside its three siblings.
SUITE checker-model-proof
   test/compiler/checker-model-proof.f
;SUITE

\ The snapshot relocation parity gate compiles formal/Common/Reloc.v with the
\ Rocq proof assistant for the same reason, so it runs in the proof slice
\ alongside its siblings.
SUITE compiler-reloc-proof
   test/compiler/reloc-proof.f
;SUITE

\ The instruction-encoding parity gate compiles formal/Common/Insn.v with the
\ Rocq proof assistant and spawns child engines for the encodings the shipped
\ assembler refuses by ending the process, so it runs in the proof slice
\ alongside its four siblings. It is the slice's long pole and it has the least
\ headroom of any suite in the gate: 99543ms quiescent against a 120000ms nominal
\ wall. That wall used to be a fixed constant and this suite is what proved the
\ constant wrong - on a host running a second gate beside this one it timed out
\ at 120145ms, a 21 percent stretch that the load factor exists to absorb.
\ STDLIB-GATE:SUITE-TIMEOUT-MS now derives from that nominal through
\ lib/test/budget.f, and test/run-lib.f hands every spawned phase the
\ HB_LOAD_PCT the resident phases already had. The slice still gets a phase to
\ itself and still starts first, because it remains the run's long pole.
SUITE compiler-insn-proof
   test/compiler/insn-proof.f
;SUITE

SUITE raw-storage-load-seal
   test/raw-storage-load-seal-test.f
;SUITE

SUITE object-record-codec
   lib/object-test.f
;SUITE

SUITE object-cache-store
   lib/object-cache-test.f
;SUITE

SUITE object-source-index
   lib/object-index-test.f
;SUITE

SUITE object-source-resolver
   lib/object-resolve-test.f
;SUITE

SUITE object-link-symbols
   lib/object-link-test.f
;SUITE

SUITE object-image-writer
   tools/object-image-test.f
;SUITE

SUITE tasking-primitive-smoke
   test/atomics-smoke.f
   test/run-in-stack-smoke.f
;SUITE

SUITE getpid-primitive-smoke
   test/getpid-smoke.f
;SUITE

SUITE proc-watch-primitive-smoke
   test/proc-watch-smoke.f
;SUITE

SUITE proc-signal-primitive-smoke
   test/proc-signal-smoke.f
;SUITE

SUITE process-fork-wrappers
   lib/process-fork-test.f
;SUITE

SUITE proc-pty-io-supervisor-smoke
   test/process-pty-io-smoke.f
;SUITE

SUITE engine-candidate-resolver
   test/engine-candidate-test.f
;SUITE

\ CPU tasking over pthread. It runs in the tail slice, which spawns a fresh
\ process per suite, because it does not survive a gate-pool FORK: on one image,
\ loaded in-process it is green, and forked through GT-POOL-START-FORK from the
\ same image it dies (rc 75 in the minimal probe, SIGSEGV in the gate's
\ tail-process group). Creating pthreads in a forked child of the pool worker is
\ the difference; dot habu-task-pthreads-die-4fea8480 owns the root cause.
SUITE tasking-threads
   lib/task-test.f
;SUITE

SUITE string-helpers
   lib/string-test.f
;SUITE

SUITE utf8-scalar
   lib/utf8-scalar-test.f
;SUITE

SUITE ffi-abi
   lib/ffi-abi-test.f
;SUITE

SUITE ffi-cabi
   lib/ffi-test.f
;SUITE

SUITE float-parse
   lib/float-test.f
   lib/fmath-test.f
;SUITE

SUITE ieee-float32
   lib/ieee754-test.f
   lib/float32-test.f
   lib/float32-buffer-test.f
;SUITE

SUITE fmt-numbers
   lib/fmt-test.f
;SUITE

SUITE float-sort
   lib/sort-test.f
;SUITE

SUITE float-stats
   lib/stats-test.f
;SUITE

SUITE hashmap
   lib/hashmap-test.f
;SUITE

SUITE prelude
   lib/prelude-test.f
;SUITE

SUITE array-helpers
   lib/array-test.f
;SUITE

SUITE adt-option
   lib/adt/option-test.f
;SUITE

SUITE adt-result
   lib/adt/result-test.f
;SUITE

SUITE cad-num-arithmetic
   lib/cad-num-arithmetic-test.f
;SUITE

SUITE table-stdlib
   lib/table-test.f
;SUITE

SUITE regex-stdlib
   lib/regex-test.f
;SUITE

SUITE map-stdlib
   lib/map-test.f
;SUITE

SUITE codegen-stdlib
   lib/codegen-test.f
;SUITE

SUITE unicode-class-runtime
   lib/unicode/class-test.f
;SUITE

SUITE unicode-class-tools
   tools/unicode/class-tool-test.f
;SUITE

SUITE unicode-class-exhaustive
   tools/unicode/class-verify-main.f
;SUITE

SUITE ptx-stdlib
   lib/ptx/header-test.f
   lib/ptx/kernel-abi-test.f
   lib/ptx/kernel-manifest-test.f
   lib/ptx/launch-test.f
   lib/ptx/rep-test.f
   lib/ptx/mint-test.f
   lib/ptx/tile-test.f
   lib/ptx/tile-loop-test.f
   lib/ptx/tile-smem-test.f
   lib/ptx/tile-acc-test.f
   lib/ptx/gemm-checked-test.f
   lib/ptx/attention-checked-test.f
   lib/ptx/attention-roles-test.f
   lib/ptx/tile-v4-test.f
   lib/ptx/tile-v4a-test.f
   lib/ptx/tile-pipe-test.f
   lib/ptx/cpp-pipe-step-test.f
   lib/ptx/cpp-slot-test.f
   lib/ptx/collective-test.f
   lib/ptx/cg-collective-test.f
   lib/ptx/cg-activation-test.f
   lib/ptx/autograd-test.f
   lib/ptx/ir-test.f
   lib/ptx/ad-test.f
   lib/ptx/ad-dag-test.f
   lib/ptx/ad-dag-eval-test.f
   lib/ptx/ad-saved-test.f
   lib/ptx/sentinel-test.f
   lib/ptx/cuda-driver-test.f
   lib/ptx/cuda-scope-test.f
   lib/ptx/ad-gen-test.f
   src/arch/ptx/vjp-test.f
;SUITE

SUITE ptx-rep-neg
   lib/ptx/rep-neg-test.f
;SUITE

SUITE ptx-mint-neg
   lib/ptx/mint-neg-test.f
;SUITE

SUITE ptx-tile-loop-neg
   lib/ptx/tile-loop-neg-test.f
;SUITE

SUITE ptx-tile-smem-neg
   lib/ptx/tile-smem-neg-test.f
;SUITE

SUITE ptx-tile-acc-neg
   lib/ptx/tile-acc-neg-test.f
;SUITE

SUITE ptx-tile-v4a-neg
   lib/ptx/tile-v4a-neg-test.f
;SUITE

SUITE ptx-tile-pipe-neg
   lib/ptx/tile-pipe-neg-test.f
;SUITE

SUITE ptx-cpp-slot-neg
   lib/ptx/cpp-slot-neg-test.f
   lib/ptx/cg-mma-slot-neg-test.f
;SUITE

SUITE ptx-gemm-checked-neg
   lib/ptx/gemm-checked-neg-test.f
;SUITE

SUITE ptx-attention-checked-neg
   lib/ptx/attention-checked-neg-test.f
;SUITE

SUITE ptx-autograd-neg
   lib/ptx/autograd-neg-test.f
;SUITE

SUITE ptx-uniform-barrier
   lib/ptx/uniform-barrier-test.f
;SUITE

SUITE ptx-toolchain
   lib/ptx/toolchain-test.f
   tools/ptx/profile-test.f
   tools/ptx/bench-test.f
   tools/ptx/saxpy-test.f
   tools/ptx/kernel-export-test.f
   tools/ptx/perf-registry-test.f
   tools/ptx/autotune-test.f
   tools/ptx/perf-compare-test.f
   tools/ptx/perf-regress-test.f
   tools/ptx/perf-regress.f
   tools/kernel-perf-lint-test.f
   tools/ptx/bandwidth-lib-test.f
   tools/ptx/mma-exact-lib-test.f
   tools/ptx/autotune-sweep-test.f
   tools/ptx/fusion-emit-test.f
   tools/ptx/device-gold-test.f
   tools/ptx/cuda-scope-leak-proof-test.f
   tools/ptx/attention-bench-test.f
   tools/ptx/fusion-compare.f
   tools/ptx/gemm-bench.f
   tools/ptx/attention-bench.f
   tools/ptx/acc-device-test.f
   tools/ptx/redadd-device-test.f
   tools/ptx/saxpy-v4-tail-device-test.f
   tools/ptx/device-gold.f
   tools/ptx/sum-launch.f
   tools/ptx/softmax-launch.f
   tools/ptx/softmax-gradcheck.f
   tools/ptx/rmsnorm-device-test.f
   tools/ptx/rope-device-test.f
   tools/ptx/layernorm-device-test.f
   tools/ptx/swiglu-device-test.f
   tools/ptx/cuda-launch.f
;SUITE

SUITE-STDIN source-stdlib-stdin DATA
   lib/source-test.f -- stdin
;SUITE

SUITE argv-stdlib-mocks
   lib/argv-test.f
;SUITE

SUITE argv-stdlib-script-args
   lib/argv-test.f -- --json --label NAME --strict-signatures --all-errors
   --strict-boundary -o OUT -- file.f --literal
;SUITE

SUITE test-stdlib
   lib/test/assert-test.f
   lib/test/suite-test.f
   lib/test/snap-test.f
   lib/test/record-test.f
   lib/test/src-shape-test.f
;SUITE

SUITE property-stdlib
   lib/property-test.f
;SUITE

SUITE date-helpers
   tools/stdlib-date-test.f
;SUITE

SUITE spawn-emitter-shape
   tools/spawn-emitter-test.f
;SUITE

SUITE c-call-emitter-shape
   tools/c-call-emitter-test.f
;SUITE

SUITE signature-scan-emitter-shape
   tools/signature-scan-emitter-test.f
;SUITE

SUITE compiler-dispatch-shape
   tools/compiler-dispatch-test.f
;SUITE

SUITE codegen-role
   tools/codegen-role-test.f
;SUITE

SUITE icode-fixup
   test/icode-fixup-test.f
;SUITE

SUITE aot-section-reach
   tools/aot-section-reach-lint-test.f
;SUITE

SUITE engine-size
   test/engine-size-test.f
;SUITE

SUITE tail-pure-fixtures
   lib/json-write-test.f
   lib/json-read-test.f
   lib/json-read-perf-contract-test.f
   lib/memory-test.f
   lib/vector-test.f
   lib/byte-buffer-test.f
   lib/layout/box-test.f
   lib/fs-test.f
   tools/bootstrap-codegen-test.f
   tools/asm-src-test.f
   tools/asm-checked-test.f
   tools/image-bytes-test.f
;SUITE

SUITE stdlib-source-default
   lib/source-test.f
;SUITE

SUITE stdlib-process-fixtures
   tools/hb-cli-contracts-test.f
   tools/standalone-load-test.f
   test/lint-cli-standalone-load.f
   lib/process-test.f
   lib/process-command-test.f
   lib/process-pty-handle-test.f
;SUITE

SUITE gate-environment-empty-stdin
   test/gate-env-stdin-tty-test.f
;SUITE

SUITE friend-arena-seal
   test/seal.f
;SUITE

SUITE internal-word-gate
   test/internal-word-gate.f
;SUITE

SUITE immediate-model
   test/immediate-model-test.f
;SUITE

SUITE pointer-storage
   test/pointer-storage-test.f
;SUITE

SUITE ptr-elem
   test/ptr-elem-test.f
;SUITE

SUITE typed-storage
   test/typed-storage-test.f
;SUITE

SUITE typed-storage-structural
   test/typed-storage-structural-test.f
;SUITE

SUITE underdepth-gate
   test/underdepth-gate.f
;SUITE

SUITE top-row-hook
   test/top-row-hook-test.f
;SUITE

SUITE top-row-warn
   test/top-row-warn-test.f
;SUITE

SUITE xt-effect
   test/xt-effect-test.f
;SUITE

SUITE xt-cell
   test/xt-cell-test.f
;SUITE

SUITE effect-read-api
   test/effect-read-api-test.f
;SUITE

SUITE create-axiom
   test/create-axiom-test.f
;SUITE

SUITE checker-assert
   test/checker-assert-test.f
;SUITE

SUITE checker-verify-pkg-scope
   test/checker-verify-pkg-scope.f
;SUITE

SUITE checker-replay-pkg-state
   test/checker-replay-pkg-state.f
;SUITE

SUITE prim-link
   test/prim-link-test.f
;SUITE

SUITE verify-prim
   test/verify-prim-test.f
;SUITE

SUITE checker-scan-index
   test/checker-scan-index-suite.f
;SUITE

SUITE effect-intern
   test/effect-intern-suite.f
;SUITE

SUITE effect-store-census
   test/effect-store-census-test.f
;SUITE

SUITE checker-dead-path
   test/checker-dead-path-suite.f
;SUITE

SUITE checker-rollback-sig-pool
   test/checker-rollback-sig-pool.f
;SUITE

SUITE sig-scope-intake
   test/sig-scope-intake.f
;SUITE

SUITE snapshot-writer
   test/snapshot-writer.f
;SUITE

SUITE stdlib-standalone-load
   test/stdlib-standalone-load.f
;SUITE

SUITE aot-wid-restore
   test/aot-wid-suite.f
;SUITE

SUITE aot-seed-batch
   test/aot-seed-batch-suite.f
;SUITE

SUITE aot-wide-format
   test/aot-wide-format-suite.f
;SUITE

SUITE aot-prelude-band
   test/aot-prelude-band-suite.f
;SUITE

SUITE aot-chain-capture
   test/aot-chain-capture-suite.f
;SUITE

SUITE aot-sig-pool
   test/aot-sig-pool-suite.f
;SUITE

SUITE region-room
   test/region-room-suite.f
;SUITE

SUITE does-clause-record
   test/does-clause-record.f
;SUITE

SUITE friend-arena-absence
   test/seal-absence.f
;SUITE

SUITE sealed-system-package
   test/seal-package.f
;SUITE

SUITE engine-error-package
   test/engine-error-package.f
;SUITE

SUITE pre-trust-defer
   test/pre-trust-defer.f
;SUITE

SUITE snapshot-xt-cell-decl
   test/snapshot-xt-cell-decl.f
;SUITE

SUITE catch-frame
   test/catch-frame.f
;SUITE

SUITE export-keyword-package
   test/export-package.f
;SUITE

SUITE tokstream
   test/tokstream-suite.f
;SUITE

SUITE using-import
   test/using-test.f
;SUITE

SUITE trust-row-refusal
   test/trust-row-test.f
;SUITE

SUITE gate-runner-entry-load
   test/gate-runner-entry-test.f
;SUITE

SUITE load-reject-diag
   test/load-reject-diag-test.f
;SUITE

SUITE dictionary-record-shapes
   test/drec-shape-test.f
;SUITE

SUITE core-prefix-mark
   test/prefix-mark-test.f
;SUITE

SUITE stdlib-runner-fixtures
   lib/test/runner-test.f
;SUITE

SUITE stdlib-build-fixtures
   lib/build-test.f
;SUITE

SUITE build-fixpoint-fixtures
   tools/build-fixpoint-test.f
;SUITE

SUITE boot-pin-fixtures
   test/boot-pin-test.f
;SUITE

SUITE load-argv-contract
   tools/load-argv-test.f
;SUITE

SUITE hb-build-fixtures
   tools/hb-build-test.f
   lib/build-cache-test.f
   lib/codesign-test.f
   tools/hb-build-direct-lints-test.f
;SUITE

SUITE gate-pool
   test/gate-pool-test.f
   test/json-read-perf-phase-test.f
;SUITE

\ The gate's own load factor: that a spawned phase is handed it at all, and that
\ the two per-suite walls are derived from it instead of frozen. It runs in the
\ tail slice, which spawns a fresh process per suite, because it starts the
\ runner for real - TEST:PREPARE reads the process's script arguments - and a
\ forked member of a slice would be handed that slice's arguments instead.
SUITE gate-budget
   test/gate-budget-test.f
;SUITE

package STDLIB-GATE public get-current ;package

package STDLIB-GATE-TEST

constant TARGET-WID

: REQUIRE-FOUND ( n -- )
   0= if E-TBL-BOUNDS throw then ;

: REQUIRE-MISSING ( n -- )
   0= 0= if E-TBL-BOUNDS throw then ;

: RUN ( -- )
   s" MAIN" TARGET-WID search-wl REQUIRE-FOUND
   s" SKIP-SEMANTIC!" TARGET-WID search-wl REQUIRE-FOUND
   s" SUITE-CHECK-CLI?" TARGET-WID search-wl REQUIRE-MISSING
   s" GATE-STDLIB-MAIN" 0 search-wl REQUIRE-MISSING
   s" SUITE-SKIP-TOOL-SEMANTIC!" 0 search-wl REQUIRE-MISSING ;

: ACTION ( -- [ -- ] )
   [: RUN ;] ;

ACTION

;package

execute

RUN

;using

s" PASS: native lint/stdlib test phase" type cr
