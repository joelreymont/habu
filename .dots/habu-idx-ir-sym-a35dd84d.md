---
title: "Index IR-SYM:SCAN so interning is not a linear walk"
status: open
priority: 2
issue-type: task
created-at: "2026-09-12T18:11:06.102488+03:00"
---

Problem: IR-SYM:SCAN walks every row of a module's interner, and since the session-compile stack every module starts as a clone of the dialect prototype (~230 rows), so IR-SYM:INTERN costs ~456 us per definition (rowan's bisect, 2026-09-12; the remaining largest term of the compile floor after the stack). Acceptance: a per-module index (hash or sorted) over the interner's spellings so a lookup is O(1) or O(log rows), built with the clone (NEW-FROM) and maintained by INTERN; the ir-symbol suites and negatives green; tools/compile-floor.f on a quiet box shows the per-definition cost drop with the number quoted. Files: src/compiler/ir/symbol.f, test/compiler/ir-symbol.f. Verify: the suites, the floor. Depends: none. Ownership: hazel. Claim: unassigned.
