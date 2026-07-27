---
title: Decide catch argument contract
status: open
priority: 1
issue-type: task
created-at: "2026-07-27T12:13:45.291669+02:00"
---

ENGINE/DOCS CONTRADICTION (found by the vector-seal lane, 2026-07-27): docs/forth.md promises catch preserves the pre-call args under the throw code, but the engine returns the argument cells CLOBBERED on the throw path - minimal fixture: : B3 ( n -- n ) {: v:n :} v 0 > if E-TEST-PROBE throw then v ; caught with 77 on the stack returns the argument cell holding -4401 (the throw code), not 77. This cost the lane a real crash: the first VEC:SORT! released its seal through the pointer catch handed back. ANS catch/throw restores stack DEPTH with unspecified values; Habu's docs promise more. Owned result: DECIDE which contract is authoritative. If the ANS depth-only semantic stands: fix docs/forth.md, then AUDIT every catch site that reads argument values after a caught throw (lib/memory.f WITH-BYTES already defensively passes no arguments - that pattern becomes the documented rule), and add a lint or checker capability that rejects reading post-catch argument cells. If the stronger promise stands: fix the engine to restore pre-call argument values, with the B3 fixture as the regression. Either way the temporary 2drop workaround in VEC:SORT! (commented at site) is revisited and deleted or blessed. Acceptance: the B3 fixture pinned with whichever semantic wins; the audit or engine fix landed; docs and engine agree; VEC:SORT!'s workaround resolved.
