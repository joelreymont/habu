---
title: Fold or delete the unrun edit-test matrices
status: open
priority: 2
issue-type: task
created-at: "2026-10-22T12:17:10.227582+03:00"
---

Problem: test/compiler/native-build-edit-e2e.f MAIN (:1285-1317) has subcommands kernel, division, writer, engine-artifact, handover, prefix, prefix-facts, emitter-role, emitter-live, emitter-probe, source-copy, legacy, emitter that no gate or driver runs; each repeats PREPARE BASELINE WARM (tree copy + cold + warm engine build), several add cold donor engines (DIVISION-BOUNDARY 10 builds, EMITTER-ROLE-BOUNDARY 8, qualify 9 :778-831). Also implementation mirrors: EARLY-PREFIX-HITS (:456-460), ENGINE-JSON-SUCCESS (:919-934, use CHECK-REPORT). Acceptance: behaviours not covered by the new gate row are folded into one registered chain with one cold comparison; the rest deleted; docs/gate.md updated. Files: test/compiler/native-build-edit-e2e.f, docs/gate.md. Depends: Gate the recompile count dot. Claim: unassigned.
