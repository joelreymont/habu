---
title: Collect wordlist ids in package WL
status: closed
priority: 2
issue-type: task
created-at: "2026-11-05T12:18:42.422219+02:00"
closed-at: "2026-11-05T12:53:48.624712+02:00"
---

Joel 2026-11-05: 'collect worldlist-related things in the WL package, not the WID pacakge'. A rename: package DICT-WL (src/habu/layout.f:133: NAMESPACE, RETIRED, RETIRED-NAMESPACE) and package WID (layout.f:146: MAX) become package WL, so DICT-WL:NAMESPACE is WL:NAMESPACE and WID:MAX is WL:MAX; 76 DICT-WL uses in 22 files, 2 WID:MAX uses. The record field DICT-WL-OFF is not the package and keeps its name. No behaviour change. Acceptance: rg finds no DICT-WL: or WID: qualifier; engine builds; probe and the suite pass. Files: layout.f and every user (rg). Verify: engine build, x86-64 build, full suite at landing. Depends: none. Ownership: those lines. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-wl
