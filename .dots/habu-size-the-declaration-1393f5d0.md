---
title: Size the declaration label buffer to its labels
status: open
priority: 2
issue-type: task
created-at: "2026-11-16T04:59:45.678798+01:00"
blocks:
  - habu-place-each-use-b8942db6
---

Problem: src/core/structure-decl.f composes rejection labels in SD-MSG, 128 bytes (SD-MSG-CAP), and SD-MSG+ cuts what does not fit, so a label carrying a long name is cut short: the kinds label ('`a` is a scope here, but field `<field>` uses it as a value type') loses its end for a field name over about 50 bytes, and other labels that carry a name are cut the same way. Acceptance: no label the file composes is cut: the buffer holds the longest one, fixed text plus each name at TF-NAME-MAX; a declaration-suite case with a 255-byte field name shows the whole label. Depends: habu-place-each-use-b8942db6. Claim: unassigned.
