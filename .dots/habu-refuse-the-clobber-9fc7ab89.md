---
title: Refuse the clobber record early
status: open
priority: 2
issue-type: task
created-at: "2026-08-03T21:14:07.549218+02:00"
---

CRITICAL, destruction review of NCLOB. NPUB:REPUBLISH documents 'everything that can refuse refuses before the first byte is written' (publish.f:340-343) but the order at :344-356 is WRITE, RETARGET, then NCLOB:RECORD — so E-NCLOB-WIDEN fires AFTER the routine is live and the dictionary retargeted. Probe pb.f: migrate narrow at A, forget, land wide replacement on exactly A; rc=-8567 yet B entry is retargeted, runs (answers 55), and the row still says gpr=3 while B writes x0..x10; pc.f then migrates a caller against the stale narrow row: 119 where 110 is correct — a REFUSED publication produced running wrong code. Fix: add RECORD-CK beside NCLOB:ROOM-CK (clobber.f:112-117 already argues exactly this for capacity) and call it from REPUBLISH before WRITE, so the widen refusal costs nothing. NOTE: the NINL COMMIT ordering half (E-NINL-STATE/E-NINL-DUP after REPUBLISH in NMIGRATE:WORK) is already claimed under habu-ask-every-commit-faa7b83d in workspace .jj-ws/habu-decline-the-row-315c7f64 — this dot is the NCLOB/publish.f side only; reconcile at merge.
