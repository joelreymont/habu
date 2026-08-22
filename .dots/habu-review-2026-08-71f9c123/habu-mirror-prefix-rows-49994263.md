---
title: mirror prefix rows and keyword rows drift from native
status: open
priority: 2
issue-type: task
created-at: "2026-08-22T22:47:07.251681+02:00"
---

Problem: docs/bootstrap.md's port checklist says the mirror selects the same prefix as native; measured: the mirror's rows (forth.fs:1944-2029, 2301-2322) lack src/core/lower-cert-seal.f (native habu2.f:911), the nine stdlib rows, internal-mark.f, top-row.f, PFX-CHAIN rows and REQUIRE-BOOT-FREEZE; the mirror has no 'export' keyword row (native C-EXPORT 6663/6721) so a future export in SRC_COMMON kills stage0 as undefined (today export appears only in aot-capture.f:183); no S0 depth-floor guard (native EM-COMMENT 6081); no DNAME-MIN-IN/DNAME-INT interpret gates (native 6754-6761); no rec-min-in@ / DNAME-MIN-IN poke beside rec-wide-publish (forth.fs:3754 vs habu2.f:2427); the seal guard uses a 4-name literal list (3572-3575) where native walks RESTAB (2950); FPRIM-WID and OWNER-API-PUB/PRI-WID exist unused so publishing into wid 1/2 is not refused in stage0 (native pins at habu1.f:3464-3465). All inert for the current corpus; none is seen by bootstrap-mirror-lint or bootstrap-codegen-test. Acceptance: each row/gate mirrored or its absence recorded as a named, tested assumption (a fixture that would red if the corpus started needing it); the structural seam lint (habu-structural-lint-for-103677fb) gets keyword rows and prefix rows as two more tables. Files: bootstrap/cg/forth.fs, tools/bootstrap-codegen-test.f, docs/bootstrap.md. Verify: recovery gate; bootstrap-codegen-test. Depends: 9269e3a3. Ownership: mirror. Claim: unassigned.
