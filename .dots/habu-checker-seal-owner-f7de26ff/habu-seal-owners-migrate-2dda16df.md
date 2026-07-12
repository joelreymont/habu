---
title: "Seal owners: migrate CAD packages"
status: open
priority: 1
issue-type: task
created-at: "2026-07-12T16:07:39.483252+02:00"
blocks:
  - habu-seal-owners-syntax-63051652
---

Problem: TARGET, TOOLCHAIN, fusion-region, artifact, evidence and store owners remain reopenable after the compiler capability lands. Acceptance: add final sealed-package assembly blocks after all owner constituents load; hostile RAW>ID, RAW>TC, RAW>REGION and private-state reopen or qualified publication reject rc84; public APIs and generated constructors remain callable; tests no longer depend on owner private reopen; monolithic MAKI remains unsealed until decomposed. Files: maki target/toolchain/fusion/artifact/evidence/store owners and focused tests, TRUSTED.md, FILEMAP.md. Verify: exact owner hostile probes, maki/test.f, trust-lint, trusted-inventory strict, AOT/snapshot and full gates. Depends: habu-seal-owners-syntax-63051652.
