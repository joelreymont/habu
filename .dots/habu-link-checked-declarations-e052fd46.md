---
title: Link checked declarations at publication
status: active
priority: 2
issue-type: task
created-at: "2026-10-16T16:17:44.814940+03:00"
---

Native bd295aba literal-cache E2E shows CAST LIT-PTR>N has no RECORD-CONTRACT, making BODY-LOOP producer unknown. Matching compiler-bundle seed E2E also proves public EXPORT aliases have no live links before capture: CHECKER-EXPORT passes DL-NEW after DL-TAKE-TAIL clears it, so publication receives effect0. Preserve each successful declaration exact symbol/effect through its actual publication; cover CAST/LINEAR and exported aliases using existing literal and complete-contract bundle E2Es. No name guessing, cache admission of unknown facts, or weaker alias assertions. Keep literal byte-identity failures distinct until their cause is proved. Independent Astra review and rebuilt native/convergence required.
