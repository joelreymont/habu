---
title: Retire stale source citations in proof files
status: open
priority: 2
issue-type: task
created-at: "2026-07-29T10:20:02.328538+02:00"
---

Full context: formal/Common/Interning.v cites Habu source line numbers throughout its header and inline comments, and every one has drifted — IR-SYM:ROW-MATCH? is cited at symbol.f:294-298 but sits at 316-320; IR-TYPE:ROW4-MATCH? cited at type.f:498-503 is at 524-529; IR-ATTR:ROW5-MATCH? cited at attr.f:641-647 is at 662-668, and roughly a dozen more. The citations actively mislead a reader today and will drift again with the next edit. Now that test/compiler/ir-intern-proof.f binds the model to the source by WORD NAME AND STRUCTURE — which does not rot — replace every file.f:NN-MM citation with the package-qualified word name alone. Sweep formal/Common/IdLaws.v and IdAllocatorLaws.v for the same pattern. Acceptance: no line-number citation of a Habu source file remains in formal/, and the gate still binds every cited word.
