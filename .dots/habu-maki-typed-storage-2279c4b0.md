---
title: "maki: typed storage for region/node scratch cells (untyped-@ laundering)"
status: open
priority: 2
issue-type: task
created-at: "\"2026-07-12T20:36:51.163793+02:00\""
---

Region-migration review (2026-07-12) LOW: scratch cells LEW-RID/LMM-RID/LRED-RID/MDL-PROBE-RID (and node-id analogs like LMM-OUTNODE) are plain variables, so '( n -- CAD-KIND:region ) CELL ! CELL @' CERTIFIES - a fetch from an untyped cell is a fresh unifiable var and the typed-output declaration is an unchecked re-assertion. Real stores all feed validated values and guarded consumers revalidate, so no live unsoundness, but R3 'storage preserves the nominal family' is not literally met for these cells and the lower-launch.f:489 comment overstates. FIX PROBE FIRST: can a 1-count LAYOUT-BUFFER (or S1 typed ptr slot) hold an arity-0 TK-CELL CAD-KIND family? If yes: mechanical migration of the scratch cells to typed slots, then the '@' seam is checker-enforced and the round-trip comments become true; negatives per cell. If no: file the checker capability (typed cells for arity-0 nominal kinds) with the ST2/ST3 reproducer from the review, and keep this dot as the consumer. Files: maki/lower-{ew,mm,red,move,launch}.f, model-ir node cells. Evidence: review probes ST2/ST3 (CHECK-QUIET-CANDIDATE! -1 where reject expected).

## PROBE RESULT 2026-07-12 (worker, no edits)

Phase 1 FAILED: LAYOUT-BUFFER rejects TK-CELL families (E-LAYOUT-BUFFER 7121;
TFAM-LAYOUT? = product|sum|enum only). The typed !/@ seam already enforces
nominal families (M1-M5 probes green); the gap is the introduction form PLUS
the ungoverned variable->ptr-family unification for TK-CELL (P1/P2: one raw
variable certifies as ptr region AND ptr cols). BLOCKED BY the checker
capability dot filed from this probe. Migration plan unchanged once it lands:
scratch cells -> 1-count LAYOUT-BUFFER slots, accessors unchanged, ST2
reproducer flipped to a reject pin.
