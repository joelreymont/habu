---
title: Bound HB-ENTRY? by the seeded words
status: open
priority: 2
issue-type: task
created-at: "2026-11-24T17:35:45.539491+01:00"
---

Problem: src/habu/linker.f HB-ENTRY? (:1910) loops below FIRST-REC, a looser bound than LNCOUNT: a lookup reaches only the seeded primitives dict[0..LNCOUNT) (habu2.f:1984-1996). Acceptance: HB-ENTRY? loops below LNCOUNT; builds unchanged. Depends: none. Claim: unassigned.
