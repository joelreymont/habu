---
title: Add WSTORE builder and buffer disposal exits
status: closed
priority: 2
issue-type: task
created-at: "2026-07-26T10:25:00.000000+02:00"
closed-at: "2026-08-02T15:42:53.236033+02:00"
close-reason: "Ancestor 5b0ebb070a5b deleted the unused GPT2LOAD/GPT2TX/WSTORE/MODELPROV host path and suites; retaining this task would resurrect deleted architecture."
---

Completes the owner-owns-its-exit principle that habu-add-wstore-public-db6c70fe (TABLE-DISPOSE, landed in the S6b1 lane) applied to one of WSTORE's three linear owners. tbuilder and buffer still have no public exit: a caller that mints a builder and then meets E-SLOT/E-SET/E-EXTENT strands the block, which the module's own suite documents (weight-store-test.f asserts exactly six stranded builders and a final residue of 14). Proven relevant by the S6b1 destruction review (finding 8): S6b2 COMMIT-ALLOCATED mints a WSTORE:buffer, and its resource-exhaustion path - which the frozen contract requires to dispose the prep completely - is not expressible until BUFFER-DISPOSE exists or the buffer is created after every fallible step. Behavior: public BUILDER-DISPOSE ( WSTORE:tbuilder -- result<n,n> ) and BUFFER-DISPOSE ( WSTORE:buffer -- result<n,n> ) over the existing private free paths, same result discipline as DISPOSE/TABLE-DISPOSE. Tests: mint-then-dispose legs with WSTORE:LIVE deltas; linearity double-use checker negatives; the stranded-builder residue in the existing suite drops accordingly and the changed assertions state why. Owner: package WSTORE. Dependencies: none; blocks the S6b2 freeze. Acceptance: weight-store suite green with updated residue counts; both diff lints; maki/test.f green. Note 2026-07-26: file content was lost to a working-copy detachment and rewritten from the orchestrator record; the created-at above is approximate. Claim: agent=s6b2 workspace=.jj-ws/habu-s6b2-alloc (first commit of the S6b2 lane)
