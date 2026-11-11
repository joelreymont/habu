---
title: A reading quotation passed to RECURSE into a word that keeps it is accepted
status: active
priority: 2
issue-type: task
created-at: "2026-11-11T10:17:29.166617+02:00"
---

Problem: found by the feature review's attack programs on the quotation line (.jj-ws.noindex/quot-wire, 15fa640e..7a4fee53), observed: ~/.cache/tmp/quot-review/a/a01-recurse-late-keep.f (rc 102), a23-recurse-mid.f and a24-keep-then-recurse.f (rc 139) certify and crash. W takes a quotation input, keeps it (KEPT ! or is ACT) and calls itself through RECURSE with a quotation reading W's local; a non-recursive keeper is refused at the call (a02, a03, b06). Cause not yet known: hunt brief .jj-ws.noindex/jerry-wasm/.out/briefs/quot-hunt-a.md. Acceptance: the three programs are refused E-LOCAL-ESCAPE; a recursive word that passes a reading quotation down and never keeps it still certifies and runs (test/quotation-locals.f recursion case). Files: src/core/checker.f, test/engine-suite.f (QL- cases). Verify: the three programs exit 70 on an engine built from the fix; test/quotation-locals.f and test/engine-suite.f rc 0. Depends: none; habu-give-a-quotation-304dcb8d lands only with it. Ownership: the quotation line. Claim: agent=jerry workspace=.jj-ws.noindex/quot-wire (hunt running).
