---
title: Delete source-substring build assertions
status: open
priority: 2
issue-type: task
created-at: "2026-08-05T10:36:19.673489+02:00"
---

CG-32. tools/bootstrap-codegen-test.f is ~1,646 lines with 259 source-text assertions a comment-only fixture can satisfy; tools/build-fixpoint.f carries an exact-substring unchecked-window audit that comments or spacing can evade. Delete assertions already subsumed by real bootstrap, fixpoint, DDC, and production execution; any unique retained invariant must be observed through the actual compiler/build event or the existing lexer, with hostile comment/string fixtures. No new lint, no manifest.
