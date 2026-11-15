---
title: Find why a type nested 100 deep hangs the checker
status: open
priority: 2
issue-type: task
created-at: "2026-11-07T02:05:26.787384+02:00"
---

Problem: `PRODUCT sw-double<a> FIELD a a FIELD b a ;PRODUCT`, then `1 TYPED-BUFFER ITEM sw-double<...100 deep...<n>...>` and `: USE ( -- ) 0 ITEM drop ;` (probe nest100.f, one item per line; the ITEM type is one token: `sw-double<` 100 times, `n`, then `>` 100 times) runs over 2 minutes on master hb without an answer. On the installed hb it prints nothing within 10 s, while the same probe 3 deep loads at once. Unexamined. Hypothesis, untested: a checker or storage walk treats the type as a tree, and each sw-double level holds its argument twice, so the walk visits 2^100 nodes where the type has 100 distinct ones. Acceptance: the cause named with the code location; a fix brief follows (a DAG walk visits each distinct node once, or a named refusal at a stated depth). Files: src/core/checker.f, src/core/type-family.f (to confirm). Verify: the probe answers within a second or is refused. Depends: none. Ownership: none until the cause is known. Claim: unassigned.
