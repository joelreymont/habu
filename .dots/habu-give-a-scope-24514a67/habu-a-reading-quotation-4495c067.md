---
title: "A reading quotation stored through a structure's MAKE is accepted"
status: active
priority: 2
issue-type: task
created-at: "2026-11-11T10:17:29.178453+02:00"
---

Problem: found by the feature review's attack programs on the quotation line (.jj-ws.noindex/quot-wire), observed: ~/.cache/tmp/quot-review/a/a11-struct-field.f certifies and crashes (rc 139): STRUCTURE holder FIELD act [ -- ] ;STRUCTURE, HOLDER:MAKE takes a quotation that reads W's local and W returns the holder; a18-struct-store.f stores the holder in a TYPED-VARIABLE and runs it after W returned (rc 0 by luck). A generic box<a> (a22), a VALUE-RECORD (a20) and a typed buffer (a12) are refused. Cause not yet known, nor which other generated words share it: hunt brief .jj-ws.noindex/jerry-wasm/.out/briefs/quot-hunt-b.md. Acceptance: a11 and a18 are refused E-LOCAL-ESCAPE, and so is every generated word that stores a quotation input (the hunt's list). Files: src/core/checker.f or the declaration definers, test/engine-suite.f. Verify: the programs exit 70 on an engine built from the fix; a QL- case per generated word kind. Depends: none; habu-give-a-quotation-304dcb8d lands only with it. Ownership: the quotation line. Claim: agent=jerry workspace=.jj-ws.noindex/quot-wire (hunt running).
