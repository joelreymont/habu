---
title: Scope value-record names per package
status: active
priority: 2
issue-type: task
created-at: "2026-10-30T21:55:41.294612+02:00"
---

Problem: VALUE-RECORD names are one program-wide list. src/core/checker.f CHECKER-TRY-RECORD refuses a name TYPE-RESERVED? answers (a record already in the VREC table, global, whatever package declared it; or a family tail TFAM-CLAIMED? finds in any package, private rows included). Reproducers (public bin/hb): a private VALUE-RECORD box in package P1 and another in P2 -> 'checker: bad or duplicate value-record type' rc 70; a private STRUCTURE box in P1 then a top-level VALUE-RECORD box -> same; a top-level VALUE-RECORD box then STRUCTURE box in P1 -> 'reserved name'. STRUCTURE box in P1 and at top level coexist. Found when test/c2-records-program.f (C2-RECORDS-TYPES public box) and test/c2-retained-contract-wide.f (top-level VALUE-RECORD box) could not share one suite row. Ruling (Joel 2026-10-30, interview): value-record names follow package rules like STRUCTURE. Acceptance: a value record declared in a package is read there and, when public, as PKG:name or through using elsewhere; two packages may each declare one name; a top-level record still claims the global name; rejected/accepted programs in the type-declaration suites; c2-source rows merged into one. Files: src/core/checker.f, src/core/type-family.f, src/habu/verify-source.f, tools/check-core.f, docs/forth.md, docs/forth-card.md, tests. Verify: focused suites, full suite, check CLI. Claim: jerry.

Joel 2026-10-30 (interview): DEFLINEAR names follow the same package rules in the same change (two private DEFLINEAR lin2 in two packages collide today). Lane: workspace .jj-ws.noindex/jerry-vrec on ff4b6876, brief /tmp/jerry-big/vrec-brief.md, reproducers /tmp/jerry-big/vrec-repro/, worker launched 2026-10-30. Lead runs the full suite at landing; then merge rows c2-records-source, c2-view-record-source and c2-xml-source into one.

Feature design started on Fable (Plan, effort high) 2026-10-30 22:07 from the worker's findings; its choices go to Joel by interview, then one worker lane for both halves.

Joel 2026-10-30 (interview): a package declares its linear type bare (`DEFLINEAR tail` inside the package; `DEFLINEAR PKG:tail` refused, 22 sites migrate); compiled units and saved images gain a public/private mark per linear type and record, all rebuilt, no version mark. Lead: tails lowercase only like family tails; a packaged linear type is stored under its folded `pkg:tail` key, as records are, so graph and owner-interface formats stay. Design and cut: /tmp/jerry-big/vrec-design-cut.md. Lane after the T0 release: it rewrites checker.f regions the stack edits.
