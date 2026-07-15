---
title: Seal CAD-NUM production authority
status: open
priority: 2
issue-type: task
created-at: "2026-07-13T14:14:22.891286+02:00"
blocks:
  - habu-seal-owners-syntax-63051652
  - habu-migrate-cad-num-cf178e59
  - habu-register-native-repl-f12807aa
---

Full context: CAD-NUM constituent files must remain reopenable during assembly but private mints are not authoritative until final sealing. Fix: add lib/cad-num.f that loads only the completed scalar/arithmetic constituents and permanently seals CAD-NUM; add hostile reopen, undefine, export, qualified publication, private lookup, and raw-mint probes. Acceptance: public validators/arithmetic remain callable; no later constituent is needed; trusted inventory is complete; only sealed lib/cad-num.f may enter production V2; snapshot/AOT/fixpoint preserve protection. Files: lib/cad-num.f, lib/cad-num-seal-test.f, TRUSTED.md. Verify seal hostile matrix, public signatures, bootstrap/fixpoint/full gates. Depends on unified CAD-NUM migration, package-seal syntax, landed checker-path TVK-RAW, and native/REPL definer registration.
