---
title: Add Maxima reader conformance stage
status: open
priority: 2
issue-type: task
created-at: "2026-04-01T22:06:02.134652+02:00"
blocks:
  - habu-remove-whole-file-4f7c968a
---

Problem: real Maxima sources are not exercised as a dedicated reader stage, so parser bugs surface later as false compiler/runtime failures. Acceptance: manifest-selected Maxima modules parse cleanly with no local source patches. Files: src/reader/parser.zig:179-190,1233-1338; ../maxima/src/nparse.lisp:42-43,169-184; ../maxima/src/float.lisp:92; ../maxima/src/transs.lisp:99. Verify: reader-only stage over manifest modules. Blockers: habu-remove-whole-file-4f7c968a.
