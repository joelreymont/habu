---
title: Check the C2 memory entries
status: closed
priority: 2
issue-type: task
created-at: "2026-10-31T13:08:03.756165+02:00"
closed-at: "2026-10-31T14:16:13.282865+02:00"
close-reason: "Landed 47c5d481: WITH-INIT, WITH-RECORDS and LENGTH-CELL are : definitions; the c2-memory.f header no longer calls the entries raw. Full suite 510/510 and x86-64 cross-build rc 0 on the batch stack."
---

Problem: c2-memory.f WITH-INIT/WITH-RECORDS and owner-runtime.f LENGTH-CELL are TRUSTED: but check as : (census tc1, probe tc1-c2-mem-probe.diff). Acceptance: all three are :, the c2-memory.f header no longer claims the entries are raw, engine build passes, c2 rows pass. Files: src/lib/c2-memory.f, src/lib/c2-memory/owner-runtime.f. Verify: engine build; rows c2-memory, c2-memory-runtime, c2-memory-source, c2-memory-aot, c2-init-accessors, c2-init-accessor-image. Depends: none. Ownership: those files. Claim: agent=jerry worker workspace=.jj-ws.noindex/jerry-tc4
