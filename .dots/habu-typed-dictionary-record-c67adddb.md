---
title: Typed dictionary record schema
status: open
priority: 2
issue-type: task
created-at: "2026-07-01T23:07:20.872752+02:00"
---

Checked DREC record type over dbase@ (records already exist - only the n->ptr base cast is missing, one provenance mint): retires ~15 TRUSTED: (hide/xref N>REC family, BP-SLOT-*, BPW-*) and unlocks rewriting the aot-closure.f (~165 ln), build.f/maker.f (~91 ln) 0-set-check REGIONS to checked code (leave <=2 named instruction-decode boundaries). Effort M (~5d). Depends: typed-defining-words mints. Big win: kills most remaining open set-check regions outside bootstrap.

## Design spec (2026-07-07, probe-proven from head d83ebce1) - SPEC-AND-STOP

The mechanism needs src/core/checker.f and src/habu/* rewrites: engine lane.
test/drec-shape-test.f (landed with this spec, wired as gate suite
dictionary-record-shapes) pins every claim below against the live checker.

### Probe results

CHECK TODAY, zero new trust (the exact shapes the rewrite uses):
- `( ptr a n -- ptr a ) +`                      = TRUSTED: XREF-REC+
- `( ptr a n -- n ) cells + @`                  = XREF-CELL@ path
- `( ptr a n -- ptr u8 ) ptr-field @`           = XREF-PTR@ without XREF-N>U8
- `( ptr a -- ptr u8 ) $18 STRUCT-BYTE+`        = inline name without XREF-A>U8
  (STRUCT-BYTE+ is an existing audited axiom, structures-effects.f)
- `( -- n ) data-base FRIEND-LATCH-CELL + @`    = SEAL-LATCH@/SEAL-NDICT@
  (data-base is already PE-PTR-A, checker.f:3901)

REJECT TODAY, fail-closed rc 70 (the only two real gaps):
- `( -- ptr a ) dbase@`   - PRIM row PE-N out (checker.f:3897)
- `( n ptr a -- ) patch32` - PRIM row n n in (checker.f:3885)

### Mechanism (engine lane, in landing order)

1. checker.f: add overload `PRIM: patch32 PE-N PE-IN PE-PTR-A PE-IN PRIM;`
   next to the existing row (overload precedent: the +/- rows at ~3773+).
   Retires XREF-PATCH32 wholesale: checked callers compute the slot pointer
   (ptr-field / cells +) and call patch32 directly. Same commit MUST flip
   test/drec-shape-test.f DRS-GAP-PATCH32 from reject to accept (the fixture
   comment says so) - that is the negative-regression swap.
2. The ONE provenance mint (the dot headline): keep dbase@ numeric; add a
   single empty-body cast `TRUSTED: DREC-N>PTR ( n -- ptr a ) ;` in xref.f
   (or a small src/habu/drec.f schema file if build/maker also consume it).
   Exactly two call sites: XREF-REC (dbase@ idx DREC * +) and XREF-NULL (0).
   NOT recommended now: flipping dbase@'s PRIM row to PE-PTR-A - it breaks
   every numeric consumer (aot-closure scans, snap-lib, treeshake) at once;
   revisit after those rewrites.
3. xref.f rewrite on the pinned shapes: XREF-REC+ loses TRUSTED (plain
   checked +); XREF-CELL@ unchanged but its rec ptrs now minted once;
   XREF-A>U8 deleted (STRUCT-BYTE+); XREF-N>U8 deleted (NAME slot read via
   ptr-field @); SEAL-LATCH@/SEAL-NDICT@ become plain checked words;
   XREF-RETIRE recomputes its slot ptr checked and uses the patch32
   overload. XREF-N>REC replaced by DREC-N>PTR.
4. debug.f BP-SLOT-ADDR/INSTR/HITS/CTRL: rewrite on the checked BPW-SLOT
   precedent (debug-watch.f:29 is ALREADY checked): give BP-TAB a TRUST row
   like BPW-TAB's, slot k of record i = `BP-TAB i 4 * k + ptr-field`
   (@ for the ptr u8 addr slot, cells + @ for numeric slots). Candidates in
   the same shape family: BPW-PRINT-ADDR, BPW-DATA-CELL.
5. hide.f BFR-N>REC KEEPS its boundary: the refresh prelude compiles under
   the OLD engine's checker (version-skew seam), so it may not assume the
   current PES surface. Row note only; retire when the seed floor guarantees
   the rows above.
6. aot-closure.f / build.f / maker.f 0 set-check regions: after 1-3, the
   record walks rewrite on the same shapes; keep <= 2 named
   instruction-decode boundaries (TGT/CALL? word scans) per the headline.

### Trust delta

Retired: XREF-N>REC, XREF-A>U8, XREF-N>U8, XREF-REC+, XREF-PATCH32,
SEAL-LATCH@, SEAL-NDICT@, BP-SLOT-ADDR/INSTR/HITS/CTRL (+BPW-PRINT-ADDR,
BPW-DATA-CELL candidates) = 11-13 TRUSTED rows.
Added: DREC-N>PTR (1 mint) + patch32 overload PES row (+ BP-TAB TRUST row).

### Dependencies

Steps 1-4 have NO hard dependency - probe-proven on the current head.
habu-typed-defining-words-aa224eb5 later upgrades DREC-N>PTR from TRUSTED:
to a checked mint declaration and supplies the null-ptr role that replaces
`0 DREC-N>PTR` in XREF-NULL. habu-staged-fixpoint-src-0b5fc6e6 benefits
downstream (step 6 shrinks the unchecked builder prefix).

### Oracles

test/drec-shape-test.f (shape + gap pins), tools/xref-test.f +
gate-dictionary (behavior), test/seal.f (latch guard), full gate.
