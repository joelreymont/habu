---
title: Cut what tier 1 and the observers leave unread
status: closed
priority: 2
issue-type: task
created-at: "2026-11-13T11:00:44.450469+02:00"
closed-at: "2026-11-13T19:01:19.176700+01:00"
---

Problem: tier 1's deletion leaves code with no reader or a constant answer: test/compiler/aot-mode.f's `1 set-tier`; src/compiler/native/checker-owner.f SOURCE-LOADED, FIELD and BY-NAME?, which always answer -1, 0 and true; definition.f's write-only M-IN, M-OUT, M-DOES-IN and M-DOES-OUT; NFETCH:CAPTURE (definition.f), whose facts nothing reads, with fetch-owner.f, checker-fetch-abi and LOWER-CERT; the host registry publication still fills (host.f NHOST:TAKE, host-publish.f PUBLISH-TAKEN, the IMPL-ENTRY, IMPL-LEN and IMPL-LIVE rows and their INVALIDATE scan), which nothing reads; tools/tier-bench.f, tier-dump.f and tier-census.f, which accept tier 1 and then die at set-tier; test/engine-stack-lifecycle-lib.f PRIMITIVE-RETURN-X64, which expects tier 1's code; and INTEL.md and docs/xcomp.md, which describe HIR and x64ir. Acceptance: each is deleted, or folded into its callers where it answers a constant, after rg -i --no-ignore over src, tools, test and the three consumers finds no other reader; the engine builds and a warm build equals a cold one. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-int3 (the merged line owzltqtq::uoxptryl).
