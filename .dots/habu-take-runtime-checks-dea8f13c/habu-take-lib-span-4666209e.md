---
title: Take library span-validity checks out of default code
status: open
priority: 2
issue-type: task
created-at: "2026-11-25T13:53:54.599615+01:00"
---

Problem: library words recheck at run time what only a program error breaks: a str's negative length or null data (src/lib/crypto/evp.f:224 RS-SPAN, :108 WITHIN-RANGE's lower bound, src/lib/zlib.f SPAN-CHECK's negative and null halves), CRYPTO RS-OPEN's active guard (evp.f:199; RS256 runs no caller code and RS-CLOSE runs in a finally, so a nested call cannot happen) and FMATH's NO-OVERLAP? (src/lib/fmath-basis.f:352). AGENTS.md Changes: no runtime checks. A length past a C int is an input limit and stays. Acceptance: a census of this class across src/lib; each check is gone, or a static check holds it. Depends: none. Claim: unassigned.
