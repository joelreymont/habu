---
title: Index imported-name lookup
status: open
priority: 2
issue-type: task
created-at: "2026-08-05T10:36:19.592864+02:00"
---

CG-26. src/habu/habu2.f:4944-5014 scans all dictionary rows and then every used WID for an unresolved bare token — O(NDICT * using-depth). Measured: 1,000 imported calls cost 11.797 ms at 11,736 records and 13.264 ms at 13,978, while the indexed-global control moved only 4.024 -> 4.058 ms. Fix: reuse the existing one-wordlist hash probe for each used public WID, preserving ambiguity across WIDs; resolve a using package through the same indexed namespace path. Do not create a second index.
