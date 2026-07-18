---
title: LPROT narrow protection windows
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-18T10:44:49.277728+02:00\""
---

Problem: LPROT (src/habu/habu1.f EMIT-PROT) mprotects the ENTIRE 4 MB
REGION between RW and RX around every protected write - dict-record flag
pokes (BWIDEMARK/BINTMARK/immediate), patch32, emission windows, and
checker-call windows (~50 BL sites across habu1.f/habu2.f). Measured on
macOS arm64 (2026-07-18): per-process boot cost is linear in the flip
window - 4 MB flips = 184 ms nop boot, 5 MB = 203 ms, 8 MB = 225 ms
(10-run averages, interleaved). This makes region growth (dict capacity
DICT-CAP 32768 + REGION 8 MB, needed to register the three landed
diff-runner suites in maki/test.f - maki peaks ndict 16347/16384 and the
code area is at 92% of the 4 MB split, measured via LATEST/XREF) cost
+41 ms per process (+22%), which trips the runtime time ratchet
(test/gate-engine-lib.f MAX-MS 10000: measured 10400 ms, reproducible on
a quiet host).

Fix (land BEFORE the region growth so both commits are independently
green): narrow the flip windows.
1. Keep FULL-REGION flips only at the bulk writers (snapshot load/
   rebase, AOT/image reconstruction - the habu2.f 3750/3760-class sites
   that genuinely write the whole region; they fire O(1) per boot).
2. Add a narrow-window flip routine for the compile-path sites: flip
   (a) the dict tail page(s) - the newest record(s) at
   (NDICT-1)*DREC page-floored up to DICT-SIZE is sufficient and simple -
   plus (b) the code window [CP - $8000, CP + $8000] page-clamped to
   [DICT-SIZE, REGION): intra-definition back-patches always target >=
   definition start, and C-COLON-CODE-ROOM bounds a definition by the
   $4000 end reserve, so CP +/- $8000 covers every legal write in the
   bracket. Two mprotect calls per flip (~40 KB total) instead of one
   4-8 MB call.
3. BPATCH32 (habu1.f ~1827) may flip just its own target page (it
   already GUARD-SPANs the exact 4-byte write).
4. The bootstrap mirror (bootstrap/cg/forth.fs, LPROT at ~1320) EMITS
   the engine, so it must emit byte-identical routines - mirror every
   change exactly (DDC gforth==native discipline, layout-slice
   precedent).
Preserve W^X invariants exactly: everything outside the flipped windows
stays RX; test/protection-span.f must stay green (raw stores into dict/
TXN bands SIGBUS; cp!-redirected emission stays bounded by
GUARD-CODE-WORD). Do NOT bump the gate ratchet - the fix must make boot
FASTER than the 184 ms baseline (dict-poke flips shrink 4 MB -> ~pages).

Acceptance: fixpoint x2 byte-identical; old-binary boot; test/run.f
green INCLUDING the runtime time ratchet with the measured slice back
under ~7 s; protection-span + wide-store-seal + lower-txn suites green;
maki/test.f + gate-stdlib green; nop-boot 10x measurement recorded in
the dot (before/after). Then the follow-on region-growth commit
(REGION $800000, DICT-CAP 32768, CFSTK-OFF $180000, DICT-SIZE $181000,
HIDX-SLOTS $10000 + the three habu1.f HIDX-SLOTS MOVZ -> LIT64 sites,
HIDX-BYTES $40000, mirror constants, protection-span DICT-SIZE/REGION
tokens instead of baked literals, maki/test.f registration of
diff-suite-id/diff-runner-tensor/diff-runner-inject) rides on top -
prepared material parked with the orchestrator.

Verify: bin/hb --load test/run.f; 10x nop-boot timing old vs new;
test/protection-span.f.

Claim: agent=lprot workspace=.jj-ws/fable-lprot (narrow flip windows; the region-growth commit rides after)
