---
title: "Campaign C1: finish the compiler and qualify a release"
status: open
priority: 1
issue-type: task
created-at: "2026-09-16T13:54:06.626167+03:00"
---

Problem: PLAN.md is still titled 'Finish Habu's native compiler', docs/bootstrap.md says neither self-rebuild path can replace a working engine, Tender and Radar run only on pinned engine and source pairs, and RESTART.md says not to call Habu release-qualified. Every generated program inherits that doubt. Acceptance: PLAN.md 'Required result' met (optimizing selfbuild and product-hosted rebuild with no retained JIT-built definitions and no JIT fallback), the release checklist in docs/roadmap.md section C1 met, the recovery chain reaching bootstrap check OK from a clean checkout, and Tender, Radar, Loom and Etch building from the released engine with no pinned pair. Children (open): habu-compile-the-tender-8385810f (PLAN.md's speed and handoff acceptance) habu-verify-emitted-images-cf0fbf79 habu-pin-image-abi-f103c7db habu-fix-block-arg-917339e1 habu-freeze-and-model-08e0f69d habu-declare-an-attr-a14961ae habu-teach-the-arm64-d98eec15 . Absorbed on 2026-09-16: 349 dots closed with the reason 'superseded by habu-campaign-c1-finish-1f129a00'; find their text with dot find. Files: PLAN.md, docs/bootstrap.md, docs/roadmap.md section C1. Verify: bin/hb --load test/run.f green on a byte-fixpoint engine; HABU_ALLOW_BOOTSTRAP=1 HABU_BOOTSTRAP_CHECK_ONLY=1 tools/bootstrap.sh; downstream build reports. Depends: none. Ownership: hazel line for integration; leaves as claimed. Claim: unassigned. Absorbed: see the archive entries closed with 'superseded by' this id.
