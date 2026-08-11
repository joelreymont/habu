---
title: A failed defer assignment reports a bare token and rc 70
status: open
priority: 2
issue-type: task
created-at: "2026-08-11T09:18:10.134037+02:00"
---

Found by the fixpkg lane (2026-08-11) while packaging hb-build: 'is' resolves the name it parses through the engine's own lookup, which does not consult used publics, so a bare target under an open using-import fails - and the failure prints ONLY the bare token (e.g. 'HOOK') to stderr with rc 70: no error code, no file, no line, no hint that using does not cover parsing words. Probe on record in the lane report ([: MINE ;] is HOOK vs is DP:HOOK). Fix the diagnostic: the engine's is-resolution failure should name the word, the source location, and state that parsing words resolve outside using-imports; consider whether the checker should reject a bare defer target under an open import outright (same family as habu-checker-defined-answers-1504bbde - scope-explicit resolution for parsing words). Files: engine defer/is implementation (src/habu/habu2.f or src/core), src/core/checker.f. Depends: none.
