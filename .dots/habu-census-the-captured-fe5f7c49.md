---
title: "Census the captured DATA window: live bytes vs baked emptiness"
status: open
priority: 2
issue-type: task
created-at: "2026-08-18T14:04:04.550453+02:00"
---

The product bin/hb is 3.65MB; the largest single component is the 1,531,045-byte captured chain DATA window, never audited for composition. Question: how much is LIVE data (tables, interned strings, initialized structures) vs cap-sized allotments persisted at their full size while mostly empty (arenas allotted at cap, buffers with low watermarks)? Method: walk the window with the record readers + the residue tools, classify by owning allotment (snap-heap-owner + the create-owner map), report bytes live / bytes zero / bytes cap-slack per owner. If slack dominates, the fix candidates are: capture live extents with per-buffer watermarks (the arena pattern), or zero-run compression in the artifact + sparse seed. This decides whether the post-cut product is ~2MB or approaches the engine+compiler-code floor (~460KB + pools). Feeds the user's <200k bar: engine hb-host IS 165KB; the product's path under 1MB runs through this census + the post-cut recapture (1.2MB code -> ~295KB, measured 4.1x).

USER RULING 2026-08-18 (direct, supersedes the census-decides
framing): HB CREATES ITS BUFFERS AT STARTUP; THE BINARY IS
TIGHT CODE ONLY. The design is settled: the artifact and the
seed ship CODE + genuinely initialized constant data and
NOTHING else - every arena, scratch buffer, and cap-sized
allotment is CREATED AT BOOT, not persisted. The census's job
is now the implementation map, not a decision: per owner,
classify initialized-constant (ships) vs created-at-boot
(deleted from the payload), and the fix follows immediately -
capture initialized extents only, the seed allots the rest.
Expected outcome: the DATA section collapses from 1.53MB to
the constant tables' true size; the product approaches
engine + dense code + constants + signatures.

CENSUS VERDICT + FIX RULINGS 2026-08-18 (the table: 758 owners,
1,531,045B, LIVE = 32B in four cells of -1, 99.998% zero -
cross-checked by an independent parse; the fix = reserve + 4
runs, restoring the semantics COPY-DATA's own header describes):
(1) THE FIFTH SCALAR IS APPROVED with the honest amendment to
the counts-not-stored rule: with a sparse payload the span is
NO LONGER DERIVABLE from a section length, so the scalar is the
ONLY authority - the rule's spirit (one authority per number)
is preserved; the letter is amended in the format's own comment.
VERSION bumps; a 42%-of-the-product migration is what the
version field is for.
(2) MERGE composes run tables by offsetting the second window's
runs by the first's span - the established shift-class
discipline; sum-family mutation per the merge suite's method.
(3) The boot decoder: minimal run-walk (offset,len,bytes per
run) with the DP residue step preceding as today and
TRAP-XTCELLS after; mutation-backed through the existing
suites.
(4) EXPLICIT ZERO of the whole span before applying runs -
relying on anon-mmap zero is an environmental assumption that
merge and snapshot-restore paths can violate and future region
reuse WILL violate; ~50us buys by-construction. Ruled.

SHAPE RULINGS 2026-08-18 (the recon handoff): (1) THE NARROWING
IS ADOPTED, superseding my fifth-scalar ruling's letter: S-WDATA
becomes the RUN TABLE (8B rows, offset u32 + length u32), the
run COUNT stays derived as length/8 (counts-not-stored working
correctly), and ONLY THE SPAN is the new scalar - the amendment
shrinks to exactly the one number that stopped being derivable.
(2) THE RUN BYTES GET THEIR OWN SECTION (S-WRUNS, SEC-N 17->18)
- the lane's recommendation adopted for its own reason: every
row stays fixed-width and the whole-number-of-rows refusal
stays STATABLE; riding bytes inside variable rows makes that
refusal impossible to express. VERSION bumps once for both.
(3) The AGREE trap is on the record: DATA-CAP's drop moves
AOT-SECTION:BYTES ~2MB, so aot-decl.f + icode.f AOT-SECTION-CAP
+ CODE-CAP-BYTES move IN ONE COMMIT or the metabuild dies at
load by design. (4) ACAP-MASK-XTCELL's invariant transfers:
declared cells are EXCLUDED FROM EVERY RUN (no buffer to zero);
the straddle refusal still fires. The ten walked anchors are
the implementation map - do not re-find them.

