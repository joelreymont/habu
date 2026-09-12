---
title: Decide how a --build payload rewinds past the ndict seal floor
status: open
priority: 2
issue-type: task
created-at: "2026-09-12T13:16:25.183727+03:00"
---

Problem: BNDSET (src/habu/habu1.f:1285-1288, since 3e29a730) refuses an ndict! below the seal floor with exit 83 when SEAL-NDICT-CELL is set, and the floor is applied at startup unconditionally (EM-SEAL-SEEDED-RUNTIME); a --build payload that rewinds with src/habu/hide.f BFR-HIDE-DICT-FROM-EARLIEST (test/verify-prim-test.f F44, cold differential) or src/habu/prefix-rewind.f PREFIX-REWIND:TO-CORE exits 83 on bin/hb while the pre-floor hb-stdin survives; tools/build-fixpoint.f BF-BOOTSTRAP-STAGE runs bin/hb --build stage2-src and docs/bootstrap.md:120-124 says --build does not apply the ordinary pre-source seal. Acceptance: a stated rule (either --build opens the ndict watermark for its payload, or the rewind goes through seed-ndict!), implemented in the responsible layer with the reason in docs/bootstrap.md; verify-prim F44 green; the fixpoint refresh's --build stage runs. Files: src/habu/habu1.f, src/habu/habu2.f, src/habu/hide.f, src/habu/prefix-rewind.f, docs/bootstrap.md, test/verify-prim-test.f. Verify: verify-prim, tools/build-fixpoint-test.f, test/run.f. Depends: habu-define-the-missing-bd5b3db0 (same build path). Ownership: hazel. Claim: unassigned.
