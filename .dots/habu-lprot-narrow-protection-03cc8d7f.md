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

---

## 2026-07-18 — Commit 1: narrow LPROT flips to written windows (agent=lprot)

Measured root-cause refinement (not assumed): the dominant boot flip cost is the
seal-time internal-mark pass (src/core/internal-mark.f IMK-WALK), which calls
`int-mark`/`min-in-mark` PER RECORD over [IMK-NDICT0, ndict) — thousands of
full-REGION (4 MB) mprotect RW/RX pairs every boot. Each poke touches ONE record's
flags cell [16] with CP and NDICT FIXED across its own open/close bracket, so a
narrow flip of only that record's page(s) is W^X-SYMMETRIC (open and close flip the
identical window; no orphaned RW page). This is exactly the RCA's "dict-poke flips
shrink 4 MB -> ~pages" dominant win.

New routine LPROTREC (habu1.f EMIT-PROT; mirrored in forth.fs): input x9 = target
dict-record address, x2 = prot; mprotects [floor_page(x9), +$8000) — two 16 KB pages,
straddle-safe for any 48-byte DREC record — then RET. x9 rides the kernel-preserved
x2-x15 band across the syscall for the caller's poke + closing flip.

Site classification:
- NARROWED to LPROTREC (symmetric flag pokes; compute x9=&record[i] BEFORE the flip):
  habu1.f BINTMARK (int-mark), BMININMARK (min-in-mark), BWIDEMARK (wide-mark);
  habu2.f EM-REC-WIDE-PUBLISH (DNAME-MIN-IN poke of record NDICT-1).
- KEPT FULL-REGION (proven NOT soundly narrowable by a stateless CP-window):
  * Emission/colon RW windows (EM-INTERPRET-COLON habu2.f:4245, C-TRUSTED, EM-P2-START)
    and the `;`-flush RX close (EM-COMPILE-FLUSH-PEND): the region is held RW from
    colon start to the `;` flush (habu2.f:6095 comment); CP advances by the body
    (EM-COMPILE-FLUSH-PEND computes bodylen = CP_close − def_start, habu2.f:5509).
    A stateless flip of floor_page(CP±$8000) opens at CP=def_start and closes at
    CP=def_end≠def_start, shifting the close window UP and orphaning up to one 16 KB
    page of previously-compiled RX code as RW after every straddling definition.
    IMPOSSIBILITY PROOF: W^X soundness needs close-window ⊇ open-window whenever CP
    grows; that forces the window low bound NON-INCREASING in CP while the high bound
    tracks CP — only a constant low bound (= DICT-SIZE, i.e. no narrowing at maturity)
    satisfies both, so NO stateless narrow window is W^X-monotonic for a def-scoped
    bracket. Refutes the parked spec's emission-window recipe.
  * Checker-call / immediate windows (EM-ADT-CON-FAM/VAR habu2.f:6105/6139,
    EM-ADT-MATCH-* 6219/6244/6269, EM-COMPILE-CALL immediate leg 5736/5747): flip the
    region RX to EXECUTE a checker/immediate JIT word at an ARBITRARY low code address,
    NOT near CP; CP±$8000 does not cover the call target, so the code-window recipe is
    incoherent for them. Stay full.
  * Defining-word brackets EMIT-CREATE / C-CONSTANT / C-DEFER: increment NDICT AND
    advance CP inside their own bracket — same orphan hazard. Stay full.
  * Bulk writers EM-SEED-AOT (3750/3760), snapshot rebase / EM-SNAPSHOT-RX-FLUSH:
    whole-region writes, O(1) per boot. Stay full.
  * Die/recovery RX restores (EM-COMPILE-UNDEF 5918/5930, EM-COMPILE-DIE 5985/5989/6073):
    mid-definition abort, unknown open window — full-region RX restore is the only
    sound recovery (per task). Stay full.
  * BPATCH32 / EMIT-DOESPATCH / C-BP-RESTORE-ONESHOT (own-page-capable patch sites):
    left full this commit to keep the diff minimal + focused on the dominant symmetric
    win; rare debug/does paths. Optional follow-up.

Deviation from parked spec (recorded per "no invariant weakening / STOP if a probe
refutes the window spec"): the parked spec asked to also narrow the emission/colon and
checker-call windows to floor_page(CP±$8000). That recipe is W^X-UNSOUND (orphan proof
above) and incoherent for checker-call windows, so those windows are KEPT FULL. The
soundly-narrowable dict-poke sites carry the dominant cost and already beat the 184 ms
baseline with headroom, so emission-window narrowing is NOT required for acceptance. A
sound emission-window narrowing would need a stateful saved-window flip (store the exact
window at open, re-seal it at close) or a def_start-anchored frontier window
[def_start, REGION); both are architecture changes beyond this commit — dot as follow-up
only if a future ratchet demands it.

Measurements (macOS arm64, page = 16 KB, quiet host):
- 10x nop-boot (bin/hb --load <empty>): BEFORE mean 188.6 / min 183.5 ms;
  AFTER mean 163.7 / min 161.2 ms  (−25 ms, −13%; under the 184 ms baseline). ✓
- fixpoint x2 byte-identical: E1 == E2 =
  sha256 26f5fc83e33939f230c8bad23cc10b9e4cefb2e2d956ece5bde597d64d53b0d7
  (old bin/hb was 36bf9828160e4d62ae09b08dc79dd2b931dc6ed6c4e63af80fed51f16ce16320).
- test/run.f perf slice: attempt e=23762 ms vs wall-budget 40600 ms (band=pass).

Gate table (commit-1 tree):
| gate | result |
|------|--------|
| fixpoint x2 byte-identical | PASS (26f5fc83…) |
| old-binary boot (old hb built new) | PASS |
| test/run.f (correctness + runtime ratchet + perf verdict) | RUN_EXIT=0; perf-verdict performance=pass correctness=t |
| test/protection-span.f | ok |
| test/wide-store-seal.f | ok |
| test/lower-txn-large.f | ok |
| test/lower-txn-protection.f | ok |
| test/internal-word-gate.f (int/min-in mark regression) | ok |
| test/gate-stdlib.f | PASS (aggregates the 7 lint suites) |
| maki/test.f | ok (exit 0) |
| host-lint | 0 finding(s) |
| filemap-lint | 0 finding(s) |
| error-code-lint | 0 finding(s) |
| trusted-inventory strict | baseline, exit 0 |
| typed-local-diff-lint (jj diff --git) | exit 0 |
| no-binary bootstrap check (forth.fs mirror, Gforth→native seed) | PASS (exit 0) |

Files: src/habu/habu1.f (LPROTREC routine + variable; BINTMARK/BMININMARK/BWIDEMARK),
src/habu/habu2.f (EMIT-LABEL-CORE label alloc; EM-REC-WIDE-PUBLISH),
bootstrap/cg/forth.fs (mirror: variable + label alloc + EMIT-PROT LPROTREC + BWIDEMARK).
