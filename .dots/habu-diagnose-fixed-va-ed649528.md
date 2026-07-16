---
title: Diagnose fixed-VA mmap boot failure on fd 2
status: open
priority: 2
issue-type: task
created-at: "2026-07-16T18:51:33.311261+02:00"
---

From the catchframe integration RCA (2026-07-16): the engine's fixed-VA region mapping failures at boot are SILENT - src/habu/habu2.f EM-MMAP-CODE-REGION / EM-MMAP-DATA-REGION exit-group 78 with no stderr ('0 78 MOVZ, NR-EXIT-GROUP SYS,'). Under host memory pressure a gate child failed to map its fixed 32MB DATA / 2MB code region, died rc 78 before executing any source, and the failure masqueraded as a wrong-rc test failure ('expected 5 got 78') costing an integration-blocking misattribution. Fix: emit a one-line fd-2 diagnostic before the exit ('hb: cannot map fixed data region' / 'hb: cannot map fixed code region', following the LCFCAP/LORPHAN diagnostic shape) in BOTH the native emitters and the stage0 forth.fs mirror (same-shape seed precedent: LCFPUSH cap guard); keep rc 78. Acceptance: a forced mmap failure (e.g. via a fixture that pre-maps the fixed VA or an rlimit harness - investigate what is reliably forcible on macOS/Linux; if not forcibly testable, document the boundary and pin the diagnostic bytes in bootstrap-codegen-test) shows the diagnostic on fd 2 with rc 78; fixpoint x2; wide-memory + recovery green; boundary-spawn-attribution rule in docs/forth.md satisfied. Files: src/habu/habu2.f, bootstrap/cg/forth.fs, test coverage per investigation. Ownership: engine boot diagnostics.
