---
title: snap-rebase straddling-range gap past endpoint guards
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T22:46:16.026032+02:00"
---

From 2b-v slice (e) (2026-07-04): BSNAPREBASE guards only the ENDPOINTS (base x8, end x16) via PROT-GUARD, so a rebase region that STARTS below a protected band and ENDS above it (straddles) walks through the band unguarded — endpoint checks miss it. The legit snapshot builder (snap-lib.f SNC-CANON) never straddles (high scratch mmap), so this is a hardening gap, not a live bug. Fix: range-overlap check — reject when [base,end) intersects [FRIEND-ARENA, FRIEND-ARENA+latch) or the second protected-WID band, not just when an endpoint lands inside; add a negative fixture with a straddling range (metabuild probe pattern from slice c). Files: src/habu/habu2.f BSNAPREBASE (~:2787) + test/seal.f. SEQUENCE: after 2b-v chain merges.

## Spec-and-stop (2026-07-07, head 43cca9d7) - habu2.f is item-8's lane

FILE: src/habu/habu2.f BSNAPREBASE (:2998); guard body PROT-GUARD in
src/habu/habu1.f (:139). habu2.f is item-8-owned - this is a spec, not an edit.

ROOT: BSNAPREBASE pops base->x8, end->x16 (snap-rebase
( base end count dbase dlen newbase -- ), :2999) and runs
`8 PROT-GUARD  16 PROT-GUARD` (:3000). PROT-GUARD is a POINT test: it traps
only when a single address's region-relative offset falls in
[FRIEND-ARENA, +FRIEND-ARENA-LEN) or [PROT-REG-OFF, +PROT-REG-LEN)
(habu1.f:143-150, C-CC unsigned-below-len). A range that STARTS below a band
and ENDS above it has neither endpoint inside, so both point checks pass and
the relocation (LSNAPRBD/LSNAPRBC, :3001-3002) walks the band unguarded.

FIX: a two-band RANGE-overlap guard replacing the two point guards. Add to
habu1.f beside PROT-GUARD (same DREG=x12 / EREG=x13 scratch discipline, base
and end registers preserved):

  \\ Reject when [base,end) intersects either sealed band. Overlap with band
  \\ [S,S+L): (end-DATA) > S AND (base-DATA) < S+L. Subsumes the point checks
  \\ (an endpoint inside a band is a range that overlaps it). Inert while the
  \\ latch is 0. snap-rebase's base<end invariant holds, so the empty-range
  \\ corner (base==end) never arises.
  : PROT-GUARD-RANGE ( baseReg endReg -- ) {: br:n er:n :}
     LBL LBL LBL {: ok:label trap:label b2:label :}
     EREG DATA FRIEND-LATCH-CELL LDR,          \ x13 = latch
     EREG ok CBZ,                              \ open -> no guard
     \\ --- band 1: [FRIEND-ARENA, +FRIEND-ARENA-LEN) ---
     EREG er DATA SUB,                         \ x13 = end - DATA
     EREG FRIEND-ARENA CMPI,                   \ end-DATA vs S
     C-LE b2 BCOND,                            \ end-DATA <= S -> no overlap, try band 2
     DREG br DATA SUB,                         \ x12 = base - DATA
     DREG FRIEND-ARENA FRIEND-ARENA-LEN + CMPI,\ base-DATA vs S+L ($B0, fits imm12)
     C-LT trap BCOND,                          \ base-DATA < S+L (and end>S) -> overlap
     \\ --- band 2: [PROT-REG-OFF, +PROT-REG-LEN) ---
     b2 LBL,
     EREG er DATA SUB,
     EREG PROT-REG-OFF MOVZ2,                  \ S=$3CB8 > imm12: materialize, then CMP reg
     ...compare end-DATA > PROT-REG-OFF; if C-LE -> ok
     DREG br DATA SUB,
     DREG PROT-REG-OFF PROT-REG-LEN + MOVZ2,   \ S+L=$3D00: materialize
     ...compare base-DATA < S+L; if C-LT -> trap else ok
     ok B,
     trap LBL,  0 E-SEAL-VIOLATION MOVZ,  NR-EXIT-GROUP SYS,
     ok LBL, ;

(band-2 uses register-materialized compares like PROT-GUARD's PROT-REG-OFF
path at habu1.f:147; C-LE/C-LT = 13/11 from mnem.f. The `...compare` lines are
CMP-reg + BCOND pairs - two scratch registers suffice since base/end regs are
untouched.) Then BSNAPREBASE:3000 becomes `8 16 PROT-GUARD-RANGE`, and the
:2996-2997 "residual dotted case" comment is deleted.

NEGATIVE FIXTURE (test/seal.f, add WITH the fix - unsafe to run pre-fix because
the guard is what stops the relocation from executing on a straddling range).
The guard fires BEFORE LSNAPRBD/LSNAPRBC, so a straddling call traps 83 with no
relocation. Forge source (child-engine program, seal live), straddling band 1
[$20,$B0): base=data-base+$0 (below), end=data-base+$100 (above):

  : SLV-SNAPRB-STRADDLE-FORGE$ ( -- ptr u8 n )
     SB-RESET
     s" data-base 0 +  data-base $100 +  0  data-base  0  data-base $200000 +  snap-rebase"
       SB-APPEND SLV-LF
     SB$ ;

Expect child EXIT 83 (E-SEAL-VIOLATION), like the other PROT band forges. Move
snap-rebase from the "hand-review only" prove-absence block (seal.f:481-483)
into the forged set, since the range guard now makes it safely exercisable.
Register note: x8/x16 are unique to this sink; the straddle forge is the only
exercise of PROT-GUARD-RANGE.

VERIFY on landing: fixpoint install --force (habu2.f baked; blocking certify);
test/seal.f + test/seal-absence.f green; the straddle forge EXIT 83; a legit
high-mmap snap-rebase (SNC-CANON path) still succeeds (range in neither band).

STOP: habu2.f + habu1.f are engine files (item-8 lane). Route this spec.
