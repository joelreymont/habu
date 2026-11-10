---
title: BUF says whether a buffer holds storage
status: active
priority: 3
issue-type: task
created-at: "2026-11-10T10:14:07.803132+02:00"
blocks:
  - habu-log-tracks-whether-695f3301
---

Problem: src/lib/log.f:47 keeps LIVE, set while the row holds storage (used at log.f:62-64, 89-92, 176-178). src/lib/byte-buffer.f:3-4 already says the capacity cell is zero exactly when no storage is held, but BUF has no public word that answers it and BUF:CAP@ throws on a dead buffer, so LOG keeps a second flag, set in one place and tested in another. Source: the code review of package LOG Joel asked for on 2026-11-10 ("check that LOG:CODE+ is implemented correctly, and the whole LOG package"), report ~/Work/reviews/habu-10x-2026-11-10/log-review.md, cases ~/Work/reviews/habu-10x-2026-11-10/log-review/, run as `cd <installed Habu tree> && bin/hb --load <case>`. Lines are as of 92c8b01b. Acceptance: BUF gains one public predicate, `BUF:LIVE? ( ptr a -- bool )`, true while the buffer holds storage (the name follows BUF's private CHECK-LIVE); LOG uses it and LIVE is deleted. Files: src/lib/byte-buffer.f, src/lib/log.f, their tests. Verify: src/lib/log-test.f and BUF's test. Depends: habu-log-tracks-whether-695f3301 (both change the row words in src/lib/log.f); Joel, 2026-11-10: "dot and have agents fix". Ownership: src/lib/byte-buffer.f, src/lib/log.f. Claim: agent=bob workspace=.jj-ws.noindex/habu-buf-says-whether-a696ddf9
