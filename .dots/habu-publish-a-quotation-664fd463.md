---
title: "Publish a quotation term's return glue"
status: open
priority: 2
issue-type: task
created-at: "2026-08-14T06:46:48.250182+02:00"
---

Found by the bundle-seams landing (15baa001): EMIT-RETURN holds a definition's outputs against the declared OUT-GLUE, but QEMIT-RETURN cannot - a quotation returns under an arity its CONSUMER declared, and that row says how many cells, not where their values begin. The return seam has one half; publishing a quotation term's glue (boundary mask) beside its din/dout is the capability. Derivation written in place above EMIT-RETURN. Files: src/core/checker.f (quotation term glue), src/compiler/native/{dict,elaborate}.f. Depends: none; sibling of the quotation-arity family.
