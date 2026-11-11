---
title: "Bind a local over a generic word's own layout"
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T11:54:55.893055+02:00"
---

Problem (found 2026-11-11 by the fix for dot habu-record-a-local-c2c817ab; programs ~/.cache/tmp/carl-locw/scratch-locw-e2.f, e4.f): a word generic over a, such as ( a -- n option<a> ), cannot bind its option<a> to a local, typed or untyped: the layout's width reads a variable, so it counts as open, although a generic argument is always one cell (a wide value passed as a is refused, case B25). The typed form was accepted and stored one cell of two; it is now refused as the untyped form is. Acceptance: a layout over the word's own declared variable has its real width, so both forms bind and read all its cells, or the reason it cannot is stated in docs/forth.md 'Rules learned by refusal'; test/wide-typed-local-probe.f WLC-VAR-ANN accepted again with a cell between the bind and the read. Files: src/core/checker.f (LAYOUT-WIDTH-OPEN?, LOC-BUNDLE-ROW). Verify: that probe. Depends: habu-record-a-local-c2c817ab. Ownership: unassigned. Claim: unassigned.
