---
title: Put src/habu/repl.f in a package
status: open
priority: 3
issue-type: task
created-at: "2026-11-02T18:22:35.290532+02:00"
---

Problem: src/habu/repl.f defines its words at top level with no package (src/habu/repl.f:203 ': INSTALL ( -- )' is a global), against CLAUDE.md 'Every module has a real package'. The switch lane (carl-switch, xkolowqk) could not name its checker primitive INSTALL because of this global and used the placeholder checker-install. Acceptance: repl.f opens a package named for its subject (card section 1), its callers qualify the words they use, words only repl.f calls go private; no global INSTALL remains. Files: src/habu/repl.f and its callers. Verify: the repl rows of test/run.f, a rebuilt hb starting its REPL (echo bye | hb, rc 70). Depends: none. Ownership: unassigned. Claim: unassigned.
