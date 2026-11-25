---
title: "Split a local annotated str in a split-str file"
status: open
priority: 2
issue-type: task
created-at: "2026-11-19T12:36:48.278614+01:00"
---

Problem: in a file marked split-str a local annotated a:str is not split: after 'split-str' and 'require src/lib/prelude.f', ': F ( ^u8 n -- ) {: a:str :} a 2drop ;' is rejected (help: give ':}' @str.tag), though docs/forth.md Types says a marked file reads each str in the rows it checks against as ^u8 n. Acceptance: that line loads in a marked file, pinned in test/split-str-marked.f, or docs/forth.md Types says a marked file does not split a local's annotation. Depends: none. Claim: unassigned.
