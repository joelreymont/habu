---
title: Package SNAP-RESTORE and re-land owner-wid fix
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-23T16:39:17.159278+02:00\""
blocks:
  - habu-prove-migration-neutrality-a4c05e44
---

The strict-B closure slice for habu-fix-owner-wid-e2bc360c, frozen cross-orchestrator (final form after the shape-probe proof and the two-commit ruling). COMMIT 1, the mechanically provable move under the migration-neutrality rule: new package SNAP-RESTORE in src/habu/habu2.f; EM-SNAPSHOT-RESTORE moves as the single definer rename to public SNAP-RESTORE:RUN; EM-SNAPSHOT-VALIDATE-WIDS, EM-SNAPSHOT-COPY-CODE, EM-SNAPSHOT-COPY-DATA, and EM-SNAPSHOT-RX-FLUSH move PRIVATE keeping their full current names so RUN's body stays byte-identical (their removals are unclaimed-green; the sole claimed mapping is EM-STARTUP requalifying to SNAP-RESTORE:RUN); EM-SNAPSHOT-REBASE-DICT stays global (shared by capture and restore); no EM-* forwarding aliases; the bootstrap Gforth mirror is untouched; reopening the package around separated restore sections is permitted if emission order requires. The accepted incident payload a2c4ec40 (both RBASE-VA sites in the validator plus the load-bearing OWE-NAME-COVERS-EXT name-length pin) re-applies byte-identically as in-package edits in the same commit. COMMIT 2 on the now-owned package: shorten the four private tails to VALIDATE-WIDS, COPY-CODE, COPY-DATA, RX-FLUSH updating only in-package calls under the ordinary package-owned gate. NEITHER COMMIT REACHES MASTER ALONE. Dependencies: the accepted migration-neutrality rule landed. Acceptance: body edit to the validator reds E-PACKAGE-OWNERSHIP on the pre-slice tree; the five-move diff passes the gate with zero findings; the re-land passes in-package; then the full incident battery on the final tree - owner-wid child path exit 0, test/gate-stdlib.f pool green twice with zero red phases, plain fixpoint byte-identical twice, per-site revert mutations red independently, name-pin shortening reds the build; both diff lints exit 0; one combined exact-tree master battery at train time. Closes habu-fix-owner-wid-e2bc360c when merged and verified.

Claim: agent=snaprestore workspace=.jj-ws/habu-package-snap-restore
