---
title: Build weight table atomically
status: closed
priority: 2
issue-type: task
created-at: "2026-07-29T20:31:06.640317+02:00"
closed-at: "2026-07-30T10:21:05.724667+02:00"
close-reason: "Removed from the GPT-2 critical path: raw pair-pointer construction cannot prove source capacity, and the existing builder remains the bounded owner."
blocks:
  - habu-enforce-ptr-elem-7e7cf2e4
  - habu-delete-resident-and-05c594cb
---

Problem: maki/infer/weight-store.f exposes WSTORE:tbuilder through TABLE-NEW, SLOT!, and SEAL even though GPT2LOAD already holds every validated slot pair; callers can omit a slot. Result: after resident deletion, package WSTORE exports TABLE-FROM-PAIRS ( ptr n -- WSTORE:table ). It preflights pair count, multiplication, every offset plus length, table geometry, and allocation before publication; writes exactly n slots; and returns the sealed table. Delete tbuilder, MINT-TBUILDER, TABLE-NEW, SLOT!, SEAL, TB>BLOCK, TB>TABLE, and every bypass. Migrate both GPT2LOAD mapped and copied builders atomically. Preserve table layout, slot order, failure codes, and ownership. Owner: WSTORE table construction and its two GPT2LOAD callers only. Production red: the public three-step API permits an incomplete builder. Acceptance: incomplete pairs reject before publication; valid mapped and copied fixtures produce byte-identical tables; overflow and allocation failures publish nothing; deleted words do not resolve; WSTORE, GPT2LOAD, package, typed-local, and exact-diff gates pass. Forbidden: new type, compatibility word, second registry, raw source span, or resident API. Claim: agent=claude-wstore workspace=.jj-ws/wstore-fix.
