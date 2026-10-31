---
title: "Let signatures name a ticked primitive's type"
status: active
priority: 2
issue-type: task
created-at: "2026-10-31T13:36:44.325810+02:00"
---

Problem: seven TRUSTED: words (elaborate.f TYPE-ENTRY, XT-STORE-ENTRY, EXECUTE-ENTRY; t0.f TYPE-ENTRY, XT-STORE-ENTRY; select-x64.f THROW-ENTRY; trap.f ROUTINE) exist only to turn a primitive's tick into an entry number, because no signature or CAST: can spell the type the checker gives a ticked primitive (the opaque-xt atom; xt falls to BAD-SIG-TYPE although forth-card section 2 lists xt). Design /tmp/jerry-big/mtick-design.md (Fable), trimmed by the lead. Acceptance: TOK-TYPE reads xt as the atom, renamed xt in diagnostics; no storage holds an xt (a declared xt cell would hold an engine code address no relocation kind follows across an image restore: first worker's app-image probe, 10 restores, never equal), refused at the storage gate; xt-cell rows refuse executing a declared xt (E-EXEC-OPAQUE-XT), minting one by CAST: (E-CAST-CLASS) and raw-storing one (E-RAW-CELL-PTR) and declaring a TYPED-VARIABLE of xt, each failing on master's engine; card sections 2 and 5 and forth.md Types and Rules learned by refusal state the rule. Files: src/core/checker.f, src/core/type-family.f, test/xt-cell-test.f, test/top-row-storage-test.f, docs/forth-card.md, docs/forth.md. Verify: engine build; SUITEs xt-cell, top-row-storage, diag-position; full suite and generation chain at landing (shared checker behaviour, atom names in capture). Depends: none. Ownership: those files. Brief /tmp/jerry-big/tcl-tick1-brief.md. Claim: agent=jerry worker workspace=.jj-ws.noindex/jerry-tick
