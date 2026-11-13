---
title: "A load's prose refusal prints no suggestion"
status: open
priority: 3
issue-type: task
created-at: "2026-11-10T14:16:42.377615+02:00"
---

Problem: of a load's prose refusals only a definition's prints its suggestion (src/core/render.f DIAG-PRINT :1954 ends the line with '; suggestion: ' and the packet's text). Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Settled by the long-term rule: prose prints the suggestion, since agents read a load's output and see nothing else; a prose reason that repeated the instruction drops it. Open: render.f's other prose writers (BADSIG-DIAG, TDECL-DIAG, REC-REFUSE, USHADOW, SBARITY, UAMB, the top-level token diagnostics, TSTALE, REPLAY-ONLY, BADQUAL and the row diagnostics). The minted-phantom line in DIAG-PROSE (:1756) was changed by reading and no program reaches it yet: a case reaches it or shows it unreachable. Acceptance: a prose refusal that has a suggestion prints it after its reason; a case pins it. Depends: none. Claim: unassigned.

Acceptance case: prose refusals written outside render.f print no suggestion: the interpreter's locals refusals (src/habu/interpreter.f:1709 more than 64 locals, :1715 a local name over 16 bytes; their checker arms LOCALBAD-PROSE kinds 1-2, src/core/render.f:1494, look unreachable under a plain load), tools/check.f's 'missing name after' (tools/check-verify-core.f:411) and the load's 'hb: reader keyword needs a name' (src/habu/interpreter.f:647). Acceptance: each of these lines says what to write, from the same text its packet carries where it has one; a census lists every prose refusal writer outside render.f with what it prints; a case each.

Acceptance case: DIAG-PROSE's scalar arm prints 'actual: n;' where the row arms print 'actual: n ; suggestion:', so a prose line's row ends two ways; eight assertions in test/type-decl-suite.f rest on the trailing space. Acceptance: one form for the end of a printed row in prose, and the tests that pin it.
