---
title: Drive FN-END and pool ceilings from vectors
status: open
priority: 2
issue-type: task
created-at: "2026-07-29T10:20:02.339385+02:00"
---

Full context: the interning gate checks IR-TYPE:FN-END's compared-field list, write ordering and staged-reference guard structurally, but no intern sequence drives it, because formal/Common/Interning.v MODEL GAP 7 leaves the function row's pool window outside the model and GAP 5 collapses the two ceilings (row table and payload pool) into one. Extend the model with a key whose equality includes a bounded element list, instantiate it for the function-type case, then add shared vector sequences that (a) intern the same staged function type twice and get one ordinal, (b) intern two function types differing only in one staged element, and (c) hit the POOL ceiling while the ROW ceiling is still free so E-IR-TYPE-LIST gets a row of its own. Do the same for IR-SYM's byte pool (E-IR-SYM-BYTES) and IR-ATTR's pool room check. Acceptance: each new sequence is driven through both the real words and the generated obligations from the one shared table.
