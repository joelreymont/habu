---
title: Stop file-object hits throwing in stripped builds
status: active
priority: 2
issue-type: task
created-at: "2026-10-23T23:07:29.609463+03:00"
---

Join stack xqznxmkk: stripped/AOT builds throw E-NUNIT-PROFILE (-8597) on a file-object hit (~24 RED gate rows: stripped-*, hb-build-stripped*, hb-build-aot-*, native-file-*). Reproducer: test/stripped-entry.f with a fresh cache on the stack engine. Lane: worker in .jj-ws.noindex/carl-hit (@ = xqznxmkk + urspuwsy); brief /Users/joel/.cache/tmp/carl-file/brief-hit.md; results /Users/joel/.cache/tmp/carl-hit/. Gate evidence: /Users/joel/.cache/tmp/carl-jgate.v29Z/suite.log.

Likely cause (L2a hand-off 3, relayed to the worker): the does> re-publish rebinds the record in RETIRE-REUSED (tools/native-file-reference.f:665-672) and uses up a name occurrence. Ruling: refresh the token on a same-declaration re-stamp, never replay a second publication at install.

Root cause confirmed (hit-handoff.md): CODE-A (PTR-VARIABLE, src/arch/arm64/icode.f:114) is rebound as occurrence 1 by RETIRE-REUSED after the does> re-publication; REF>DATA (NFR:1313) throws. Ruling: the cut retires rows (NFR follows CUT-NOTICE), a stale token on publish refreshes the token and keeps the key; ON-PUBLISHED is bound only with the cut cell. Fix lane: worker in carl-hit, brief /Users/joel/.cache/tmp/carl-file/brief-hitfix.md.

Fix ynxllvlk "Keep re-stamped records' keys across does>" (carl-hit): stripped-entry, stripped-image, aot-image, stripped-path, retired, types pass; new replay case PTR-RESTAMP passes. Open: tools/hb-build-stripped-cache-test.f rc 67, a warm build of unchanged sources goes stale at src/habu/aot-decl.f segment=2 with "candidate identity unknown" after address-carrier.f is reused. Root-cause lane: brief /Users/joel/.cache/tmp/carl-file/brief-candid.md. F1/F2 review fold-ins accepted and folded into nxnoutxt/kquxmzvo.
