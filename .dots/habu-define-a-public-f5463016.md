---
title: Define a public structure in a package with a part
status: open
priority: 2
issue-type: task
created-at: "2026-11-03T13:32:02.073077+02:00"
---

Problem (from tim via Maki, 2026-11-03; reproduced by carl on the installed hb, Habu 09565d43): probe e1.f 'package QZ package QZ-PT public : HELLO ( -- n ) 1 ; ;package public STRUCTURE pt FIELD x n ;STRUCTURE ;package 4 QZ-PT:MAKE QZ-PT:UNMAKE . cr' dies 'xref: protected-WID constructor not found', rc 76: an internal xref failure, not a checked refusal or a working structure. Related: Habu dot habu-declare-a-structure-243eefd8 and its design (/Users/joel/Work/maki/.jj-ws.noindex/tim/.out/r2/design-structure-in-part.md), which tim sends after Joel approves its public items. Acceptance: root cause named (why the structure's constructor lookup misses when the package already has the part QZ-PT); the probe either builds and runs or is refused by the rule docs/forth.md states for a structure's family name, with a named error, never rc 76. Files: to find (src/habu/xref.f protected-WID constructor lookup, src/core/structure-decl.f). Verify: the probe, structure and package tests. Depends: none. Ownership: unassigned. Claim: unassigned.

Consumers: no Maki package that has a part declares a public structure.
