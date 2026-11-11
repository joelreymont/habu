---
title: A store brings a part in when the part never reached the stored cell
status: active
priority: 2
issue-type: task
created-at: "2026-11-11T17:27:27.619363+02:00"
---

Problem: src/habu/linker.f STORER+/PAIR+ :2158-2172 record a store as (holder piece row, storing piece row) and WALK-STORERS :1219-1230 follows every store of every piece that stored into a piece once the walk reaches any of that piece's data. A body edit of CORE-FOLD-C (src/core/util.f) rewrites 129 of 502 engine objects in 10.5 s; about 90 come only through stored values (66 through prefix-boundary.f:22's quotation in checker.f FIRST-RECORD-XT, 22 through type-family.f:7418's TFL-VAR? cell, 4 through decl-event hooks). Evidence: .out/handoff-notes-49.md RCA, ~/.cache/tmp/carl-rca. Acceptance: a part depends on a stored value only when its walk reached the cell the value was stored into; the reduced case (A: variable V, defer H; B stores [: F ;] is H, F calls G; C runs a word reading only V; G's body changes) leaves C's object linked, as a case of test/file-objects.f; warm equals cold; the util.f body edit's rewritten count is reported before and after. Files: src/habu/linker.f, test/file-objects.f. Verify: test/file-objects.f rc 0, the six-edit census (~/.cache/tmp/carl-words/six/six.sh). Depends: none. Ownership: linker.f store records and the reach walk. Claim: agent=carl workspace=.jj-ws.noindex/carl-rca (hunt brief .out/briefs/hunt-store-reach-by-piece.md running; the fix is a second brief).

## Hunt result (2026-11-11, ~/.cache/tmp/carl-rca/h2)
- Reduced case reproduced: changing H-INNER's body rewrites c.f (reads only a.f's variable) and cb.f (reads only a2.f's variable, which holds the number 5); ca.f, which reads no data, is not rewritten. Sources h2/src, rows h2/rows.txt.
- Cause: PAIR+ (linker.f:2158) drops the store's cell and merges pairs; ROW-EDGES is one span per piece (:2383, :4309), so one store's value cannot be told from the piece's others. STORES+ (:2260) already finds the cell's build-table row and discards it.
- With a store followed only into a reached cell (prototype walk, kernel tree): prefix-boundary group 0 of 8 stay, type-family 2 of 10 (layout-buffer.f+0 and layout-valid.f+0 reach checker.f's cell through TFAM-INST-WIDTH-XT / TFAM-LAYOUT?-XT), decl-event 0 of 2. Engine tree (58 of the 66) not measured: the fix's census gives it.
- Fix brief: .out/briefs/fix-store-reach-by-cell.md (worker in carl-rca on master e6d4c460).
- Side finding, not chased: in a bare LINKER:WITH with no startup files `defer H-HOOK ( -- n )` is refused with "H-HOOK at a.f:4" and no error class.
