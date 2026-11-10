---
title: "NDICT:TOP-REC repeats the qualified-name lookup two other words hold"
status: open
priority: 4
issue-type: task
created-at: "2026-11-10T14:26:10.084083+02:00"
---

Problem: src/compiler/native/dict.f TOP-REC (added by habu-a-pkg-part-3ae2acf8) finds a PACKAGE:WORD spelling in the top-level namespace as src/core/xref.f TARGET-RECORD and tools/hb-build-core.f WRITER (:267-272) already do: three copies of one lookup. Found by the review of habu-a-pkg-part-3ae2acf8 (/Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/join-tagcheck-review.md, commit 2 finding 3). Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: one word answers a PACKAGE:WORD spelling from the top-level namespace and the three callers use it, or a trace shows why a copy must differ, recorded beside it. Files: src/compiler/native/dict.f, src/core/xref.f, tools/hb-build-core.f. Verify: test/file-objects.f, test/program-diagnostics-test.f. Depends: habu-a-pkg-part-3ae2acf8 landed. Ownership: the native dictionary. Claim: unassigned.
