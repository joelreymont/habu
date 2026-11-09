---
title: "Report the interpreter's refusals as JSON under --json-errors"
status: open
priority: 2
issue-type: task
created-at: "2026-11-09T15:21:46.461017+02:00"
---

Problem: src/habu/outer.f:374 UNDEFINED (and AMBIGUOUS :377, SHADOWED :381, all through THROW-AT :371) print plain text through ERROR-TEXT:TYPE whatever DIAG-JSON! (src/core/checker.f:4454) set. Since step 5 commit 1 hb-build runs a stripped build in its own process, so `hb --load tools/hb-build.f -- --json-errors app.f -o app` on an app with no MAIN prints 'E-UNDEFINED: MAIN at <out>/hb-build/app-end.f:1' as text; the old maker child's stderr filter dropped non-JSON lines. Acceptance: with DIAG-JSON! true the interpreter's refusals print one JSON diagnostic in src/core/render.f DIAG-JSON's shape (code, message, token, file, line) and no text line; without it the text stays. Files: src/habu/outer.f, src/core/render.f. Verify: an hb-build test case runs --json-errors on a source with no MAIN and on one using an undefined word, and reads one JSON object each. Depends: habu-build-stripped-apps-805dee2b (step 5 commit 1). Ownership: those files. Claim: unassigned.
