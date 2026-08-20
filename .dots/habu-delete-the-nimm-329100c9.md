---
title: Delete the NIMM immediate classifier and the PTX text optimizer
status: active
priority: 1
issue-type: task
created-at: "\"2026-08-20T20:53:59.674323+02:00\""
---

User ruling 2026-08-20, hard cut. Both subsystems have zero consumers anywhere in the tree, and zero consumers is the tree's own evidence; version control is the cache for maybe-futures, so nothing is preserved by keeping unreachable source in the working tree. Sized lists come from the verification probe recorded on habu-external-review-2026-5cb4522c's leaf, re-verified name by name in this lane before anything was removed. NIMM: src/compiler/native/immediate.f (261 lines), test/compiler/native-immediate.f (462), the SUITE row in test/gate-stdlib-cases.f, the fork line in test/gate-stdlib-inline-lib.f, the seven E-NIMM-* codes and their -8220..-8239 block in lib/errors.f, and the docs/compiler-ir-design.md prose that named it. PTX text optimizer: lib/ptx/opt.f (303), opt-ir.f (357), opt-test.f (122), opt-ir-test.f (70), their two suite rows and two fork rows, the two tools/ptx/perf-watch.f producer rows, the tools/kernel-perf-lint-test.f fixtures that named them, E-PTX-OPT-OVERFLOW and E-PTX-OPT-SYNTAX, and the design-doc prose. Supersedes habu-give-the-immediate-73cb0a49: the compile-time immediate class re-derives from history if and when it has a named first consumer. Supersedes habu-adjudicate-dormant-ptx-482310bc: the sm_87/sm_121 SASS question travels to the PTXIR2 canonical-boundary work if that ever gets an owner, and the 4-byte render-growth bug dies with the code. Prose is a dependency: every doc mention is rewritten, never dangled.

Claim: agent=prune-2 workspace=.jj-ws/habu-effstore
