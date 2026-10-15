---
title: Share the engine name compare and hash probe
status: open
priority: 2
issue-type: task
created-at: "2026-10-15T17:16:30.784113+03:00"
blocks:
  - habu-remove-unreachable-dictionary-79228ba2
---

Problem: the folded-name compare loop is emitted 13 times in 11 words (habu1.f EMIT-FIND 5710-5920, WLFIND 4386; habu2.f EMIT-FIND-USED 9915-10055 and others), and the hash probe 17 times in 8 words. A token is hashed up to three times per lookup (LFIND, then LFINDUSED on a miss at habu2.f:10345 and 11781, LFINDSHADOW at habu2.f:10064). Acceptance: one name-equal leaf and one probe leaf used by every lookup; the hash computed once per lookup and passed forward; about -200 lines; engine behavior unchanged; Fable review. Design with the Plan agent before dispatch. Files: src/habu/habu1.f, src/habu/habu2.f. Verify: rebuild bin/hb with tools/native-build.f (docs/bootstrap.md); bin/hb --load test/run.f; tools/two-generation-build.f convergence (engine output changes). Depends: the dictionary-scan fallback dot (removes the scan legs first); carl-final-core chain landed. Ownership: listed files. Claim: unassigned.
