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
   test/compiler/ir-id.f
;SUITE

SUITE compiler-ir-id-manifest
   test/compiler/ir-id-manifest.f
;SUITE

SUITE compiler-ir-intern-manifest
   test/compiler/ir-intern-manifest.f
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

SUITE compiler-native-immediate
   test/compiler/native-immediate.f
;SUITE

\ The code generator comparison harness, and what a gate can honestly ask of it.
\ Its member checks the exact half of ALL THREE pinned corpora - the original
\ eleven shapes, the seven surveyed hot words of
\ tools/codegen-compare-corpus2.f, and the ten float words of
\ tools/codegen-compare-corpus3.f, whose whole new column is a gap today -
\ in one run: machine-code bytes, the values each compiled word computes, the
\ head-to-head agreement of the two code generators, and the structure of each
\ committed table. The timing column is deliberately left out
\ of every gate, here and in the resident group below. A cost is a measurement
\ compared with a number recorded on an idle machine, and a loaded host reaches
\ the tolerance band on its own - eight competing processes per core left two
\ per cent of it, sixteen went through it and reported four words that no
\ compiler change had touched. Those measurements, and what the band does and
\ does not buy, are written at the head of tools/codegen-compare-baseline.f.
\ The scheduled run prints one line naming the comparison it did not make, and
\ the timed check is bin/hb --load tools/codegen-compare.f, run by hand before a
\ change that is meant to move the numbers. The member is mirrored into the
\ resident stdlib/tail-pure fork group, so it is scheduled rather than run by
\ hand.
\
\ NO ASSERTION THE MEMBER RUNS READS A CLOCK, and that is deliberate rather than
\ incidental: a cost-direction assertion on the third corpus's T-SUM row failed
\ one scheduled run in ten (dot habu-retire-the-flaky-25a37a74). The claims that
\ are about a cost live in tools/codegen-compare-timed-test.f, which is run by
\ hand on a quiet machine beside the entry above and is listed in no suite for
\ the same reason that entry is not: scheduling it would schedule a flake. What
\ the member pins in their place is the data-stack traffic each column's
\ emitted code makes, row by row, which is exact and moves for one reason.
\ The third column and the second reference join the same member for the same
\ reason: nothing either of them asserts reads a clock. The symbol reader is
\ attacked on listings built to fool it, the chain baseline on fixtures built
\ from a real measurement, and the reference column on the real corpora - where
\ what is checked is that every row has a twin and every twin answers what the
\ engine's word answered. A host without a C compiler runs the first two and
\ says so about the third; it does not fail.
\
\ Both files sit in the resident tail-pure fork list, so this registration is
\ covered file by file rather than by a label nobody selects.
SUITE codegen-compare
   tools/codegen-compare-test.f
   tools/codegen-compare-clang-test.f
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
\ listed in no suite, exactly as tools/codegen-compare-timed-test.f is. The
\ member is mirrored into the resident stdlib/tail-pure fork group, so it is
\ scheduled rather than run by hand.
SUITE codegen-workload
   tools/codegen-workload-test.f
;SUITE

\ Where the register allocator's spill wall is, measured through the real
\ migration entry. It is its OWN member and not one of codegen-compare's,
\ because it migrates definitions of its own and requires the fourth corpus's
\ cases to get a callee: sharing a process with the comparison would leave the
\ comparison measuring the fourth corpus against the third corpus's baseline,
\ which is exactly what happened when it was first listed there. No assertion it
\ makes reads a clock - every one of them is a throw code from the chain - so
\ scheduling it schedules no flake. It runs in the proof slice with the parity
\ gates: at 26s through the real runner it is a minute-scale member, not a
\ fast-tier one.
SUITE codegen-spill-probe
   tools/codegen-spill-probe.f
;SUITE

SUITE compiler-native-hir
   test/compiler/native-hir.f
;SUITE

SUITE compiler-native-elaborate
   test/compiler/native-elaborate.f
;SUITE

SUITE compiler-native-a64ir
   test/compiler/native-a64ir.f
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

\ The production entry: a definition the engine compiles, recompiled by the
\ chain and republished under its own name, plus what happens to a word the
\ chain cannot compile.
SUITE compiler-native-migrate
   test/compiler/native-migrate.f
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

\ The multiply-add the chain writes, held against the two instructions it
\ replaces: the same source compiled by the engine's emitter, which never fuses,
\ and by the chain, which does, run against each other to the ends of the signed
\ range. It runs beside the chain's own leaves because its fixtures go through
\ the production migration entry.
SUITE compiler-native-combine
   test/compiler/native-combine.f
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

\ A rename is a permutation of the compile-time value vector, and that vector
\ counts CELLS while the language counts VALUES. This suite is the differential
\ that holds the two together over a value wider than a cell: every case states
\ one body twice, once for the engine and once for the chain, and compares what
\ they leave rather than what anybody expected them to leave.
SUITE compiler-native-rename-rows
   test/compiler/native-rename-rows.f
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
   test/compiler/ir-id-proof.f
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
   lib/ptx/opt-ir-test.f
   lib/ptx/opt-test.f
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

SUITE snapshot-writer
   test/snapshot-writer.f
;SUITE

SUITE stdlib-standalone-load
   test/stdlib-standalone-load.f
;SUITE

SUITE aot-wid-restore
   test/aot-wid-suite.f
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

SUITE using-import
   test/using-test.f
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
