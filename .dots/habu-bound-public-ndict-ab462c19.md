---
title: Bound public ndict! before dictionary arithmetic and rebuild
status: open
priority: 1
issue-type: bug
created-at: "2026-09-14T11:56:31Z"
---

Audit M3 remains present at source 220930e479bdff0f21819944ea6aefbfe380e798 and optimized native engine SHA-256 2a49a29c9804f00292652d45f0a32aa27e6f224034585501e890bf9d2acbc14c. Public BNDSET (src/habu/habu1.f:1257) computes DBASE+n*DREC before checking a numeric bound, accepts a raise, stores it in NDICT, then rebuilds. HIDX:LREBUILD (habu1.f:3856) refuses only n >= HIDX:LOAD-MAX ($18000), while DICT-CAP is $10000. Therefore $10000 < n < $18000 drives C-HIDX-INS through non-record memory; negative n also lacks a numeric guard and bypasses the unsigned seal-floor comparison. The closed seed-ndict! lower-bound task covers a separate primitive.

Safe current probes: ndict@ dup ndict! ndict@ = returns true; a tier-1 compiled $18000 ndict! exits 74 with "hb: dictionary index exhausted" before any record scan. Dangerous 70000/negative dictionary mutations were not executed during revalidation; the missing interval guard is established from the production sink and rebuild source.

Fix the public setter before pointer arithmetic, mutation, or rebuilding: admit only the count domain 0..DICT-CAP, then retain existing seal-floor and protected-span checks. Preserve legitimate rewind/restore and identity operations; a full count differs from the index of the next append. Add real child refusals for negative and over-capacity counts, verify current live-count/full-boundary semantics and unchanged lookup after valid restore, and use the optimized native build. Scope: BNDSET and focused dictionary fixtures. Integer-overflow lane owns implementation; parent Astra reviews before integration.
