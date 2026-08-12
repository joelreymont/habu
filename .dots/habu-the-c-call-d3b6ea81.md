---
title: The C-CALL shape test counts substrings, prose included
status: open
priority: 2
issue-type: task
created-at: "2026-08-11T20:00:00.000000+02:00"
---

Found in the inliner-record landing (3fbe2243): tools/c-call-emitter-test.f pins emitter shapes by counting raw substring occurrences across habu2.f, so source PROSE is part of its input - a comment naming C-CALL-COPY-INLINE or C-CALL-REJECT-UNSAFE one more time flips the count and reds the suite (the landing had to reword prose to fit). Fix the counter to parse structure instead of text per the Test Integrity rule: count definition heads / call sites, not substrings, with fixtures that hide the expected text in comments and strings to prove the parser is not fooled. Files: tools/c-call-emitter-test.f. Depends: none.
