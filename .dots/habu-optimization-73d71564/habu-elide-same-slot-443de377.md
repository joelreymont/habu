---
title: Elide same-slot frame reloads in tier 1
status: closed
priority: 2
issue-type: task
created-at: "\"2026-09-16T11:59:50.339658+03:00\""
closed-at: "2026-09-25T09:28:29.176150+02:00"
close-reason: "Independent review accepted the same-block store/reload rule and shared layout/emission predicate. Isolated code shrinks by 4644 bytes net; padding hides standalone file savings. Integrated native generations are byte-identical at 2889847 bytes, engine SHA256 730f69dac961702ea8593685d5f7641df32cf2d8d20725ba996b38f422e63aae. Full native gate passes all 490 suites; real Maki routing/export and negotiation artifacts pass unchanged. Evidence: ~/.cache/tmp/habu-opt-round2/combined/RESULTS.md and ~/.cache/tmp/habu-opt-store-load/RESULTS.md."
---

Problem: the native engine at master 88243cb4 contains 1,519 adjacent SP
store/load pairs with identical offset and register. Removing one instruction
per pair would total 6,076 bytes; that is an arithmetic upper bound, not proven
savings. The original load/store-back problem is already fixed by
`regalloc.f MB-IDENTITY-COPY?` and `spill.f IDENTITY-COPY?`, with existing
`test/compiler/native-identity-spill.f` coverage.

Design: after accepted allocation, omit a load only when its immediate IR
predecessor in the same block is the corresponding store, with identical slot
and physical register. The general and floating frame forms each move eight
bytes at SP without writeback. Keep the store. One pure predicate serves layout
and writing; no cross-block search, skipped intervening operation, raw byte
heuristic, new persistent state, or allocation change.

Failure modes recorded before code: crossing an entry/join; differing register
file, width, base or offset; SP writeback; removing a needed store; intervening
clobber or memory write; layout/source-map disagreement; stale emission state.
Full design: `~/.cache/tmp/habu-opt-store-load/design.md`.

Acceptance: unchanged real compiled-execution and frame/loop/float fixtures;
private frozen native generations with byte identity; actual code and file
deltas including added optimizer code; independent review; integrated full
registry gate and downstream Maki owned by the integrator. No new unit tests.

Ownership: agent opt_checker_scratch, workspace `.jj-ws/opt-store-load`.
Files: `src/compiler/native/emit.f` and this dot. Integrator closes after review
and acceptance. Evidence: `~/.cache/tmp/habu-opt-store-load/`.

Worker verification: frozen executable source 72d2a063 built three native
generations, each exit 0. Generations 2 and 3 are byte-identical, SHA-256
`62a2514b5377c52b6b77b4caf6858397fc1b0ffb32f73ba3f166ebb8df18a675`.
The 12 existing focused suites pass on both generations 1 and 2; generation 2
also passes `test/native-defer-image.f` (capture, fresh boot, recapture and
fresh compilation). No test source changed.

Measured code blob: 1,536,660 to 1,532,016 bytes, a net reduction of 4,644.
Generation 1 prices the added compiler code at 636 bytes; recompiling the same
source with that compiler removes 5,280 code bytes. Encoded metadata grows by
340 bytes, so the unpadded payload shrinks by 4,304. Mach-O padding absorbs this:
the physical file stays 2,972,407 bytes. Existing census pairs fall from 1,519
to 201; the remainder is outside this rule's proven eligibility. Independent
source review accepted the rule and its shared layout/emission predicate.

Integrated with shared effect contents, native generations are byte-identical
at 2,889,847 bytes, SHA-256
`730f69dac961702ea8593685d5f7641df32cf2d8d20725ba996b38f422e63aae`.
Maki's actual native REPL image is 22,687,328 bytes; real routing/geometry and
negotiation checks pass with byte-identical exported PCB artifacts. The combined
file reduction cannot be attributed to this peephole alone. Final registry
acceptance and rejected-run evidence belong to
`~/.cache/tmp/habu-opt-round2/combined/RESULTS.md`.
