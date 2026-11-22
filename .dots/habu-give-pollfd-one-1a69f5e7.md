---
title: Give pollfd one FD type
status: active
priority: 2
issue-type: task
created-at: "2026-11-22T14:18:22.403730+01:00"
---

Problem: five modules pack a struct pollfd by hand: src/lib/fd.f:133-135,165-168 (POLL-STORAGE, LE offsets 0, 4 and 6), src/lib/process.f:121-122,322-329 (PFD-AT!, PFD-REVENTS), src/lib/process-fork.f:158-192 (a create'd 8 bytes, PROC-REAP-PFD!) and test/gate-pool.f:1231-1235 (GT-POOL-PFD!). Acceptance: FD declares one pollfd type and the words that build one and read its revents; every module above holds pollfd values in typed storage through them and no shift, mask or offset spells a pollfd's layout outside FD; the fd, stdlib-process-fixtures and gate-signal suites pass. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-pfd.
