---
title: Reject a file that calls a word whose file it does not require
status: active
priority: 2
issue-type: task
created-at: "2026-11-24T21:55:34.046841+01:00"
---

Problem: docs/forth-card.md section 7 says every file requires its own dependencies, engine-provided files included ("Write it anyway"), but nothing checks it: a require of an engine-provided file is a no-op and a transitive require hides a missing one, so 52 files called string-option.f's words without requiring it (fixed by the commit "Require string-option.f where its words are used"). Acceptance: the load and tools/check.f reject a file that calls a word defined in a file it neither is nor requires, naming the word, its file and the require to add; the tree passes it. Design: .out/designs/design-require.md (direct requires; the boot prefix is the prelude; carried files judged; no facades; types after words). Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-reqcheck.
