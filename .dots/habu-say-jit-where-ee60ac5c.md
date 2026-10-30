---
title: Say JIT where a comment means the JIT, not tier 0
status: open
priority: 2
issue-type: task
created-at: "2026-10-30T23:56:20.257037+02:00"
---

Problem: on arm64 tier 0 is T0 (src/habu/t0.f); on x86-64 tier 0 is still the JIT. 367 comment mentions of 'tier 0' in src, tools and test predate that split; some mean the JIT (habu2.f near 8706 'tier 0 keeps literals inline until string.f installs', 8720 'Tier 0 (the JIT) is the cold default', prims.f ~953 'binds one at tier 0 only', definition.f CERTIFYING? 'tier 0 also publishes'; the round-2 comment fold-ins take these four). Acceptance: every comment says the JIT, T0 or tier 0 for what the code beside it does on each arch; no 'tier 0' left meaning the JIT. Files: per directory census first (rg -n -i 'tier[ -]0'), then lanes of a few named files each. Verify: strip.py code identical per file. Depends: the T0 release landing. Ownership: comments only. Claim: unassigned.
