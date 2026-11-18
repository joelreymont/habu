---
title: Place XML reader rejections in the original document
status: open
priority: 2
issue-type: task
created-at: "2026-11-18T00:27:00.705731+01:00"
---

Problem: the reader's REJECT (src/lib/xml/state.f:320) places by the lexical UTF-8 copy, while the source stage (src/lib/xml/source.f SRC-NEXT, SRC-CHECK-DECL; scalar.f) places by the original bytes, the byte order mark counted, and by 16-bit units in UTF-16. One document's columns therefore differ by which stage rejected it: after a UTF-8 byte order mark by 3 on line 1, and in UTF-16 by the UTF-8 length of every non-ASCII character before the place. SRC-MAP (source.f:443) already maps a lexical offset to the original. Acceptance: a reader rejection and a source rejection at the same place in one document give the same line and column, in UTF-8 with a byte order mark and in UTF-16; a test pins each. Depends: none. Claim: unassigned.
