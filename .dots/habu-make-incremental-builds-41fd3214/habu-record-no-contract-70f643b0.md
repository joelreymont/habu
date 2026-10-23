---
title: Record no-contract bindings in file frames
status: open
priority: 1
issue-type: task
created-at: "2026-10-22T17:59:02.222596+03:00"
blocks:
  - habu-record-deps-per-1e433205
---

Problem: an engine build's file frames mark 12 files ordered, all binding-contract-unknown (check-hook.f DIAGXT, structure-decl.f and enum-decl.f TYPE-MAY-ARRIVE-XT, xref.f PKG-LIVE-XT, ...): words compiled in the raw prefix before the checker exists have no contract, so the BINDING row has no data and the file stores no object. Acceptance: a binding to a word with no contract records a BINDING row whose data is a fixed no-contract marker; CHECK matches it when the live word still has no contract and misses with 'contract appeared' when it gains one; READER-FLAGS and VALUE rows still guard immediacy and constants; an engine build with HABU_NATIVE_CACHE_DIAG=1 shows zero binding-contract-unknown frames. Files: tools/native-unit-dependency.f (CAPTURE of BINDING, CHECK-BINDING). Verify: native-file-frame e2e case binding a contract-less word, native-file-auto e2e. Depends: frame dot. Claim: unassigned.
