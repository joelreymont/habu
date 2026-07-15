---
title: Fix forth.md Naming section fictional STRUCTURE example
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-15T19:03:52.201196+02:00\""
---

Found by the docsync lane 2026-07-15 (habu-reconcile-forth-md-289ac1be follow-up): docs/forth.md Naming section (~lines 16-22, 33 - a different section from the fixed type-declaration block) still teaches the fictional STRUCTURE ... ;STRUCTURE as the canonical opener/closer example, forbids the BEGIN-FOO/END-FOO shape while BEGIN-STRUCTURE/END-STRUCTURE are live shipped words, and asserts 'Removed type delimiters have no compatibility spelling' - resting on the same never-shipped unified-grammar fiction the type-declaration block had. Fix: rewrite the Naming section's opener/closer guidance around the REAL live surface (SUMTYPE/;SUMTYPE, PRODUCT/;PRODUCT, ENUM/;ENUM, VALUE-RECORD/END-VALUE-RECORD, BEGIN-STRUCTURE/END-STRUCTURE), pick a naming rule consistent with what ships, and delete the removed-delimiters claim (probe-verify every named token first: docsync's evidence method - rg definition + usage + live bin/hb load probe). Doc-only. Verify: host-lint, and every asserted token status proven by probe. Ownership: docs/forth.md Naming section.

Claim: agent=namefix workspace=.jj-ws/fable-namefix
