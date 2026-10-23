---
title: Lower wide values at tier 0 under 0 set-check
status: open
priority: 2
issue-type: task
created-at: "2026-10-23T20:03:26.919365+03:00"
---

Under '0 set-check' tier 0 compiles a wide value in a ':' or TRUSTED: body one cell wide ('1 2 7' where tier 1 prints '7 2 1'): the hookless arm of EM-COMPILE-PUBLISH-HOOKED (src/habu/habu2.f) never scans, so no certificate or pass 2 exists. Found reviewing slnrrlyu (2026-10-23). Reproduce: c2a-trusted-body.f in ~/.cache/tmp/jerry-t0fix-cases/ preceded by '0 set-check', at both tiers. Acceptance: tier 0 prints tier 1's output with checking off; a test/tier.f row. Claim: unassigned.
