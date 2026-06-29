---
title: Audit main gate duplicated work
status: closed
priority: 1
issue-type: task
created-at: "\"2026-06-28T19:02:08.158764+02:00\""
closed-at: "2026-06-29T02:55:27.007004+02:00"
close-reason: "research completed and implementation child dots landed: single under-test artifact, warm runner/image cache, dictionary batching, AOT assertion inlining, engine fixture batching, and stdlib tool subprocess collapse. Latest full hot gate: 44.811s internal / 47.81s wall; broader 30s target remains under parent cut-native-gate."
---

Problem: main native gate still repeats expensive work: multiple phases launch bin/hb children that reload common libs/tools, some phases rebuild/check Habu-like artifacts independently, and semantic tests remain at CLI/process boundaries where one loaded process could run the same assertions. Desired direction: build the candidate Habu once into HB_TMP, make it the single Habu-under-test for downstream phases, derive warm checker/tools images once from that candidate/content hash, and split tests into in-process semantic libraries plus thin process-boundary proofs only where CLI/isolation is the invariant. Research task: inspect test/run.f, gate-pool, warm-image, hb-build, build-fixpoint, and long-pole gate slices; quantify duplicate hb launches/builds/load bundles; identify which tests can safely move in-process and which must remain separate processes. Acceptance: produce a code-grounded gate review with file:line findings, a DAG/artifact-cache redesign, risks, and implementation subdots before changing gate behavior.

Research 2026-06-28:
- Fresh full native gate passed at 80656ms. Current tails: stdlib check-cli 55745ms, stdlib tool-boundary 54790ms, engine fixtures 37041ms, checker diagnostics repair 33079ms, engine build 28816ms, dictionary/checker 27254ms, diagnostics undef-primary 27692ms.
- test/run.f:468-492 cold-spawns every top-level phase as `bin/hb --load` with the same common prelude plus phase files. The DAG exists, but each phase reparses the harness and common libs.
- test/gate-stdlib.f:478-530 turns every `TEST-SUITE`/`TEST-TOOL-SUITE` into another `bin/hb` process. The suite batching helped, but the long check/tool slices still spawn boundary children for tests that mostly assert pure tool semantics.
- test/gate-engine.f:246-259 builds `hb-new` inside only the engine-build phase temp root; test/run.f schedules other phases against `bin/hb`, so the candidate is not the single Habu-under-test.
- tools/hb-build-lib.f:376-434 has a maker cache and lock, but HBB still builds/runs the maker as a subprocess and test/gate-build-common.f:153-176 shells out to `tools/aot-call-report.f` and `tools/gate-json-assert.f` even though core libraries exist.
- tools/warm-image-lib.f:190-240 builds warm images by spawning `bin/hb` for public signature export and snapshot; useful, but the current warm images cover inner tools/checker, not the top-level gate runner itself.
- test/gate-dictionary.f and test/gate-diagnostics.f still contain many `GE-HB-RUN-STDIN` and `GE-CHECK-RUN` snippets. These should become batched checked source runs or direct `CHECK-CANDIDATE!`/`CHECK!` calls where the invariant is not process isolation.

Long-term design:
- Build one candidate artifact first: `hb-under-test`, `hb-check-warm`, `hb-tools-warm`, `hb-build-maker-aot`, `hb-build-maker-repl`, all content-keyed from source + seed + target.
- Make `test/run.f` pass `HABU_UNDER_TEST` to all children; default to `bin/hb` only for seed/bootstrap and explicit baseline-contract tests.
- Bake a warm gate-runner image that already has `test/gate-common.f`, `test/gate-stdlib.f`, `test/gate-engine.f`, diagnostics, and build helpers loaded; phase children execute a selected word instead of reparsing harnesses.
- Split tests into semantic libraries and boundary wrappers. In-process semantic tests run in the loaded gate runner; one boundary test per CLI keeps argv/env/stdin/cwd/exit behavior honest.
- Keep process pool only for true isolation and parallelism; remove subprocesses used only as a substitute for calling a checked word.
