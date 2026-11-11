---
title: Trace and delete the words and slots the ENGINE-ORIGIN review found without readers
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T09:12:26.557829+02:00"
---

Problem: the review of the ENGINE-ORIGIN deletion (2026-11-11) found older items with no reader or caller, by rg only, not yet traced through a build: src/habu/aot-capture.f ACAP-KEEP?'s comment justifies keeping CHECKER-STORAGE-CELLS by a lookup that does not exist (aot-data-cell.f is gone; heap.f and proc-maps.f cite it too), and whether the keep itself is dead needs a build; the ORIGIN-SYM-OFF slot and CHECKER-ORIGIN-SYM (src/core/checker.f) have no reader; CHECKER-REG:ORIGIN-KEY has no caller; tools/hook-sites.f HOOK-SITES:COUNT, PATH$, NAME$, CLASS$, OWNER$, CHECK?, TOP? have no callers; src/core/packages.f reads EXPORT-ACCEPT-OFF, which nothing sets. Acceptance: each item is traced (rg -i --no-ignore over src tools test, then an engine build without it) and deleted with its comments, or kept with the caller named in this dot; no slot is renumbered. Files: src/habu/aot-capture.f, src/core/checker.f, src/core/checker-owner-abi.f, src/core/packages.f, tools/hook-sites.f, src/habu/heap.f, src/habu/proc-maps.f. Verify: the engine built twice byte-equal, the chain check, the full suite at landing. Consumers: none meets it. Depends: the ENGINE-ORIGIN deletion landed (habu-engine-origin-records-84dbb78e, same files). Ownership: carl. Claim: unassigned.
