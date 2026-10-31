---
title: "Make the hook stack's warm build equal cold"
status: active
priority: 2
issue-type: task
created-at: "2026-10-31T13:07:09.995783+02:00"
---

uyzslxrr + NMAP fix gives warm == cold (160 reused); with voxvpzxp (hook key) 162 files are reused (native-body.f, engine-origin-checked.f) and warm != cold: one captured DATA cell at offset 988488 (0xf1548) holds 2^48-1 cold and 0 warm. Evidence /tmp/carl-wcd2 (cold-hb, warm-hb, lane-hb, win.py). Brief carl-dots/.out/carl-file/brief-wcd2.md; one fix commit on osrvporo (the NMAP fix duplicated onto voxvpzxp). Blocks the hook commit's landing. Claim: agent=carl worker workspace=.jj-ws.noindex/carl-wcd2.

Root cause (carl-wcd2 worker): the cell is DATA offset 854552 (win.py's stream offset was 64 bytes stale), the checker variable XM-ACC (src/core/checker.f ~4372). XMASK-OF / CALL-EDGE-MASK leave XMASK-ALL (2^48-1) on throw edges and nothing resets it; in a cold build the last files checked (native-body.f, engine-origin-checked.f) leave it set, in a warm build they are reused and it stays 0. Fix svnpqmuk 3f61294e "Rest the throw-mask cells at file boundaries" on osrvporo: XM-I, XM-ACC, XS-I, XE-PD, XE-PR join QDEPTH's CHECKER-REG:SCRATCH span (zeroed at file boundary and capture). Proof /tmp/carl-wcd2/run/p0.txt: eng rc 0, cold 164 compiled, warm 162 reused 2 compiled 45.9 s, cmp hb and hb.map equal. Opus review running. Folds into the lane 11 squash with moxyulxx.

Opus review: claims 1-4 CONFIRMED (each cell written before read in every use; none crosses a file boundary); one low finding applied by the lead: CHECKER-SCAN-SNAP-RESET no longer stores 0 into XE-PD/XE-PR (the span's reset already does). svnpqmuk is now be86f766.
