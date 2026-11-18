---
title: Give signal.f and the fork child public words
status: closed
priority: 2
issue-type: task
created-at: "2026-11-18T17:40:40.700389+01:00"
closed-at: "2026-11-18T17:44:13.642331+01:00"
---

Problem: src/lib/signal.f:126-139 reads the two published signal cells as data-base plus an offset, and src/lib/process-fork.f:142 clears the catch chain as data-base plus HND-CELL, so both need data-base, which the parent dot takes from programs. Acceptance: neither file names data-base; SIGNAL-ABI answers the two cells by typed words and ERROR runs a forked child's root. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-sig.
