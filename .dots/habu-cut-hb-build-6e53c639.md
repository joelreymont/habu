---
title: Cut HB build package API
status: open
priority: 1
issue-type: task
created-at: "2026-07-15T23:51:09.181294+02:00"
blocks:
  - habu-split-hb-build-5fc098e2
---

Full context: after all HB-BUILD component extractions, leave hb-build-lib.f as dependency-owning facade plus orchestration; publish only PRESEED-ENTRY!, PRESEED-SEED!, BUILD and MAIN in addition to state and CLI API. Migrate hb-build.f, direct-lints, in-process gate, AOT-positive, REPL, run inventories and white-box tests; rename gate-build-hbb.f to gate-build-inproc.f and eliminate HBB, GB-HBB and HBT-HBB source vocabulary with no aliases. Tests reopen HB-BUILD for private white-box access without nested packages. Acceptance: public BUILD/MAIN resolve; bare short names, every legacy spelling and private MAKER-KEY! reject; focused/full build gates green.
