---
title: Lower wide values at tier 0 under 0 set-check
status: active
priority: 2
issue-type: task
created-at: "2026-10-23T20:03:26.919365+03:00"
---

Under '0 set-check' tier 0 compiles a wide value in a ':' or TRUSTED: body one cell wide ('1 2 7' where tier 1 prints '7 2 1'): the hookless arm of EM-COMPILE-PUBLISH-HOOKED (src/habu/habu2.f) never scans, so no certificate or pass 2 exists. Found reviewing slnrrlyu (2026-10-23). Reproduce: c2a-trusted-body.f in ~/.cache/tmp/jerry-t0fix-cases/ preceded by '0 set-check', at both tiers. Acceptance: tier 0 prints tier 1's output with checking off; a test/tier.f row. Claim: jerry, worker in .jj-ws.noindex/jerry-nocheck.

Root cause of the Gforth recovery rc 70 (2026-10-24): tier 0 reaches the unjudged scan through the owner record (C-CALL-CHECK-UNJUDGED, DECL-CHECK-UNJUDGED-OFF) but fetches its certificate by name (habu2.f FREEZE and the keep site: C-FIND-GLOBAL of lower-cert:bytes/keep/kept; seed forth.fs ~7249-7266). In the bootstrap reload the names resolve to no checker or another one. Fix: install the three certificate operations as owner-record fields, as lower-cert-base.f installs FOR-HASH at CHECKER-FETCH-ABI:CERTIFICATE-OFF, and reach them through DECL-OWNER in habu2 and the seed. Clearing state in seed-ndict! was rejected: prefix-rewind.f callers keep the checker live and rely on set-check resuming.
