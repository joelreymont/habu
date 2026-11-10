---
title: Make a linear type an entry so a build knows it
status: active
priority: 2
issue-type: task
created-at: "2026-11-10T07:57:54.951730+02:00"
---

Problem: a type declared with DEFLINEAR loads but cannot be named in a build: hb-build of a program requiring src/lib/power.f, byte-edit.f or pg.f ends rc 74 'unknown type' (reduced: DEFLINEAR QB:t then LINEAR: >T ( n -- QB:t ), or a colon signature naming it). Cause, measured 2026-11-10: in a build DEFLINEAR (src/core/roles.f:144) runs the product's copy of CHECKER-DEFLINEAR, whose CT-ADD-LINEAR writes the product copy's type table, while the running hb checks the build's signatures against its own table (CT-FIND); the table is not a registered store and a linear type is not a type entry, unlike a family (linker.f TYPE-ENTRIES, type-family.f TFAM-FIND). DECISIONS 'where the checker's facts live': a type is an entry whose text is its declaration. Acceptance: programs requiring the reduced files, power.f and byte-edit.f build; the E-LINEAR-SCOPE and E-LINEAR-OWNER refusals are the load's in a build; a warm build that links the declaring file equals a cold build; the engine rebuilds itself byte for byte; the suite passes. Files: src/core/checker.f, src/core/type-family.f, src/habu/linker.f. Verify: brief .jj-ws.noindex/carl-layout/.out/briefs/linear-type-is-an-entry.md. Depends: none. Ownership: carl. Claim: agent=carl worker workspace=.jj-ws.noindex/carl-deflin. Open beside it: VALUE-RECORD's table is not a registered store either (untested in a build; the lane tests it and reports); a program requiring pg.f on master stops earlier, at aio.f ('the cell ... in ROWS holds an address outside hb's data and the build's space'), which the host-loads lane removes.
