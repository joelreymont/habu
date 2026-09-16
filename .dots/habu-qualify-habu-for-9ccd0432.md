---
title: Qualify Habu for release on the guard-page line
status: active
priority: 2
issue-type: task
created-at: "\"2026-09-16T13:02:53.357410+03:00\""
---

Problem: Joel builds Loom, Maki, Tender and Radar on Habu and wants release quality before any further optimization (2026-09-16). Cedar's 2026-09-14 handoff said do not call Habu release-qualified yet. This dot is the checklist; each item names the dot or evidence that closes it. Acceptance, all required: (1) the full gate bin/hb --load test/run.f is green on a byte-fixpoint engine built from the line head, run on a quiet machine, with no FAIL and no TIMEOUT-UNDER-LOAD (as of the 12:46 run two load-induced reds remain to rerun: proc-pty-tty-smoke, native-window-owner); (2) the stage0 recovery chain HABU_ALLOW_BOOTSTRAP=1 HABU_BOOTSTRAP_CHECK_ONLY=1 tools/bootstrap.sh reaches bootstrap check OK on the same head (mirror parity after the guard-page mirror, 85149742); (3) stripped applications: test/gate-aot-positive.f fully green including the wide-fetch programs (habu-fix-hb-build-3bb224a6, the base-relative null cell) and a stripped overflow prints hb: stack bounds exceeded (data) with exit 102; (4) snapshot images: snapshot-writer and the image suites green (done, f7f4e49f); (5) silent traps closed: a typed local that shadows a resolvable word is rejected or warned by name (habu-warn-when-a-8c4d889a), every error code has one owner (done, error-code lint green), the parametric-effect rule is documented in docs/forth.md with the concrete-row idiom kestrel and hazel used; (6) downstream proof: Radar's sixteen harnesses on the engine (kestrel), Tender pinned to the new root (birch), Loom and Maki building on it (rowan), each result recorded here; (7) docs current: docs/forth.md, docs/debugging.md, RESTART.md, LESSONS.md; (8) integration: cedar/compiler-integration and hazel/integration advanced to the head, root bin/hb replaced, willow's switcher pin updated. Should, not must: reproducible builds across directories (habu-bake-prefix-src-1047b604). Deferred until after this closes: habu-inline-trivial-engine-922133ca, habu-capture-the-heap-3b453b61 tables, habu-build-the-internal-4cd07a82, habu-cut-the-gate-3fd6352e, habu-derive-baked-build-d5df4419, habu-store-the-snapshot-203a86a0, habu-measure-the-optimizing-b4b239f1, habu-elide-the-three-a87cf770. Files: none of its own. Verify: the gate log, the bootstrap chain log, the downstream reports. Depends: habu-replace-per-transfer-8523fb98, habu-fix-hb-build-3bb224a6, habu-warn-when-a-8c4d889a. Ownership: hazel line. Claim: agent=hazel workspace=.jj-ws/hazel-guard-pages


Evidence 2026-09-16: (6) Radar: kestrel certified all sixteen Radar harnesses on the guard-page engine (workspace radar-strict at 52508018 + f2fbe08a): 1,769 checks, same counts as on the previous engine; Radar's own rows became BEGIN-STRUCTURE layouts and pointer-returning words became ptr n under the strict parametric rule. Kestrel's repro of the locals-shadowing trap (local text over create TEXT) reproduces on the pinned engine fE too, so it is the known trap, not a regression; the rejection is habu-warn-when-a-8c4d889a in flight.


Evidence 2026-09-16 13:30: (3) stripped applications: test/gate-aot-positive.f fully green on the null-cell engine (both forks; PASS: native hb-build AOT positive tests), stripped-quotation/stripped-sparse-data/internal-word-gate green; the base-relative null cell is dab2c17e on the line. (1) partial: S4 (8acc52fb, 5,832,896 bytes) passes pre-trust-defer, cold-runtime, build-fixpoint-fixtures, error-code-lint (0 findings), stack-guard, snapshot-writer; bootstrap-wide-memory-src stays red until the mirror-fold lane lands. Full chain from dab2c17e started 13:31.


Evidence 2026-09-16 13:45: (1) full gate on F2 built from dab2c17e (5,832,896 bytes): 399 PASS, 1 FAIL = bootstrap-wide-memory-src (mirror parity of the folded emitters, lane in flight); the earlier load timeouts and proc-pty-tty-smoke passed. F1->F2 differed by 17,992 bytes beyond the baked path (host-dependent baked facts, dot habu-derive-baked-build-d5df4419); F3 on F2 pending for the fixpoint.

Fixpoint 2026-09-16 13:47: F3 (built on F2) == F2 modulo the baked tree path; engine 5,832,896 bytes; self-build 101 s on a quiet machine (138 s this morning).

Evidence 2026-09-16 14:21: portable require rows landed (97db883a). Engine
/tmp/hazel-fH1 built from that head on fF3 (129s, 5,832,896 bytes, no build
directory in strings); cold check green: pre-trust-defer, cold-runtime-test,
build-fixpoint-test 208s, bootstrap-wide-memory-src, all rc 0. Opus audit of
the six Sonnet commits filed as dots habu-rebase-only-the-35ab0076,
habu-keep-the-recovery-1111544c, habu-mirror-the-boot-b94d5e54,
habu-test-the-guarded-046a7404, habu-keep-failing-asserts-f5b1b376 (workers
dispatched on f5b95c66); they gate the final full run.

Evidence 2026-09-16 14:46: full gate on /tmp/hazel-fH2 (built from 3b7bbd22
on fH1, 97s, 5,832,896 bytes; cold check green) = 402 PASS, 0 FAIL, exit 0,
497s at load 5-9. First fully green gate of the line; it carries the portable
require rows and the declared-spelling locals. Pending before the final chain:
imgdump --pc, recovery seed fixture + open-token mirror + EMIT-LOC-FIND
mirror, stack fixtures (all in flight), and the byte fixpoint on the final head.

Evidence 2026-09-16 15:26 (final chain on 6406e0b0, all audit lanes landed):
F1 built on fH2 in 96s, F2 built on F1 in 95s, both 5,832,896 bytes and
byte-identical (sha 7e490c6031cc317c): byte fixpoint reached, build
directory no longer baked. Probes on F1: data/return/loop overflow named,
exit 102; interpreter underflow E-UNDERFLOW rc 70; plain run rc 0, trivial
start 22-34 ms. Full gate on F2 in its own tree: 402 PASS, 0 FAIL, exit 0,
462s at 15:26. Stage0 chain (tools/bootstrap.sh) reached "bootstrap check
OK" at 15:06 on the same sources; gforth fixtures bootstrap-engine-stack,
bootstrap-wide-memory, bootstrap-ptr-cell-mark rc 0. Audit dots closed on
this proof. Remaining: integrate (bookmarks, root bin/hb), notify
willow/birch/kestrel for downstream re-certification.
