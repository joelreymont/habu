---
title: run-in-stack accepts a reading quotation and does not follow its record
status: active
priority: 2
issue-type: task
created-at: "2026-11-11T10:17:29.189894+02:00"
---

Problem: found by the feature review's attack programs on the quotation line (.jj-ws.noindex/quot-wire), observed: ~/.cache/tmp/quot-review/a/c03-run-in-stack.f: a plain quotation given to run-in-stack prints 77, a quotation reading W's local certifies and crashes (rc 139). The kernel lane made execute, catch, finally and c2-invoke follow a record; every other primitive taking a quotation was to refuse a reading one at the call. Cause not yet known, nor whether another primitive row shares it: hunt brief .jj-ws.noindex/jerry-wasm/.out/briefs/quot-hunt-c.md. Acceptance: for every primitive row with a quotation input, a reading quotation is either refused at the call or its kernel body follows the record; c03 no longer crashes. Files: src/habu/prims.f, src/habu/habu1.f, src/core/checker.f, test/quotation-locals.f or test/engine-suite.f. Verify: c03 exits 70 or prints 77 and 10 on an engine built from the fix; a case per row. Depends: none; habu-give-a-quotation-304dcb8d lands only with it. Ownership: the quotation line. Claim: agent=jerry workspace=.jj-ws.noindex/quot-wire (hunt running).
