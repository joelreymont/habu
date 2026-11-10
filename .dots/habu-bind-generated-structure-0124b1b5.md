---
title: Bind generated STRUCTURE and LAYOUT text to engine words, not the declaring scope
status: open
priority: 1
issue-type: task
created-at: "2026-10-30T11:20:57.575455+02:00"
---

Problem: the source text STRUCTURE and LAYOUT generate names engine words by spelling, and that spelling is looked up through the declaring scope's search order, so a package definition of the same name captures it. STRUCTURE's accessors spell `* record-at` unqualified (src/core/sumtype.f:1990); LAYOUT's buffer words spell `+ @`, `* +`, `throw`, LDEFER-BIND and LDEFER-GROW unqualified (src/core/layout-buffer.f:388-411, LDEFER-SOURCE). Reproducers found by phil, on the installed hb: a package that defines `: record-at ( a n -- a ) drop ;`, or its own `*`, before a STRUCTURE makes `2 S3-AT` answer element 0 (prints 'element 0 holds 55'); a second probe (shstruct.f) printed 'V holds 77'. It is type-safe so far: a hygiene hole. Only `field-project` is closed: a sealed engine refuses it as a definition name (src/habu/definers.f:255-258, src/core/structure-make.f:134-138). A qualified spelling does not fix it, because the checker gives qualified tokens no intrinsic identity (src/core/checker.f:20143-20146, TOK-IDENTITY). The general fix, generated text binding engine words by identity rather than by name through the caller's order, needs a design nobody has made.
Acceptance: a design, approved by Joel and recorded here, for how generated STRUCTURE and LAYOUT text binds the engine words it names; with it, the record-at and `*` probes above, as tests, either answer the right element or are refused.
Files: src/core/sumtype.f, src/core/layout-buffer.f, src/core/structure-make.f, src/core/checker.f.
Verify: the probes as test cases: a package defining record-at or `*` before a STRUCTURE, then `2 S3-AT`, does not answer element 0.
Depends: none.
Ownership: unassigned. Claim: unassigned.
