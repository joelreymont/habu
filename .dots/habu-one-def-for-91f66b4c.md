---
title: One definition for the engine constants the checker reads
status: open
priority: 2
issue-type: task
created-at: "2026-10-08T10:58:28.859923+02:00"
---

Problem: src/core/checker.f keeps its own copies of engine constants it cannot name because they load after it: `2 constant CK-REC-FLAGS-SLOT` and `3 constant CK-REC-NAME-SLOT` (checker.f:2877-2878) copy src/habu/xref.f:14-15 XREF-FLAGS-SLOT and XREF-NAME-SLOT. The load order today is src/habu/boot-files.f's FILL list, which tools/hb-build-core.f walks part by part (CHECKER, then DECL, then CORE and the rest, :142-148): the CHECKER part loads src/habu/layout.f (:71) before src/core/checker.f (:73), so the checker names layout.f's offsets directly (e.g. `data-base PKG-PUB-CELL + @`, checker.f:1853; ENCLOSING:REC, :13061) and the layout copies (CK-PKG-PUB-OFF, CK-DREC, CK-DICT-CAP, CK-ENCLOSING-*, CK-PKG-OUTSIDE-OFF) are gone; xref.f is a CORE row (:104), after the checker.
Acceptance: the record-format constants the checker reads load before src/core/checker.f, so the checker names them directly; the CK-REC-* copies of xref.f's slots are deleted. Anything moved ahead of the checker loses its recorded types unless the definition is the source of its type (blocking dot). Also measure whether the checker walks the using list and package record to resolve names itself where the engine's own lookup would answer; if it duplicates lookup, record that as its own dot.
Files: src/core/checker.f, src/habu/xref.f, src/habu/boot-files.f.
Verify: full suite on the built hb.
Ruling (Joel, 2026-10-08): the kernel is the words compiled before the type checker runs. Constants moved ahead of the checker become kernel words; those checked code reads carry a trusted type declared at their definition.
Depends: habu-take-a-word-01856558. Ownership: unassigned. Claim: unassigned.
