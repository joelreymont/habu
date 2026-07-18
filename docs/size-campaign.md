# Engine size campaign: back under 128 K

Target: `bin/hb` ≤ 131,072 bytes on both targets (the README's read-whole
claim), from 165,367 (macOS) / 147,648 (linux) at 92213edd. Stretch: ~100 K.

## Why the binary grew — and why repo debloat will not shrink it

`bin/hb` is the emitted engine (habu1/habu2 stencils, keyword data, dict
seed, guard rows) plus, on macOS, the code signature. Library/maki/doc size
is irrelevant to it. The ratchet history (test/gate-build-size.f) attributes
the 2026-07 growth: TFAM pass-2 width lowering, native MATCH lowering, and
the certified-word underdepth gate — and notes each macOS bump was
page-granular: "~1 KB of emitted code crossing the 16 KB page floor"
accounts for a 16.5 K step. The measured deltas imply real code growth of a
few KB riding three page-floor crossings.

That granularity is leverage in both directions: compacting a few KB of
emitted code can recover a whole 16 K page.

## Rule 1: attribute before compacting

No compaction lands without a committed size-attribution baseline. Step 1 is
a build report emitting bytes per contributor — per-emitter-phase machine
code (habu1 prims, habu2 keyword JIT, MATCH lowering, P2WIDE dispatch),
kwdata tables, LARITY guard rows, dict seed, page padding, signature — in
the gate-build-size.f manifest style (committed rows, gate-validated). The
report tells us which of the candidates below actually pays; guesses do not
land.

## Compaction candidates (validate against attribution)

1. **Stencil tail sharing.** Emitted stencils repeat push/pop/frame tails
   inline; hoisting shared tails into engine-resident helpers trades one
   branch for N copies. Candidate sites: the MATCH lowering family
   (EM-ADT-MATCH-*), P2WIDE transport, LP2* helpers — the exact families the
   bumps name.
2. **Guard-row packing.** LARITY rows and the min-in compact-record bytes:
   byte-width fields and interned name refs instead of cell-width rows.
3. **kwdata interning.** Keyword/diagnostic strings: single interned pool,
   suffix sharing where the emitter allows.
4. **Out-of-line diagnostics.** Inline error legs (e.g. C-DIE-BAD-TAG) moved
   to one engine-resident die routine with a tag argument.
5. **Page-floor engineering.** After 1–4, if a target sits just above a
   floor, shave to cross it; the report must show distance-to-floor per
   target so this is a measured decision, not hope.

## Constraints

- Fixpoint: every step rebuilds byte-for-byte to its own fixpoint; gate
  green on both targets; ratchet rows lowered in the same commit (the
  STALE-BASELINE mechanism enforces this).
- No performance regression on the gate's timing bands: stencil-tail sharing
  is a size/speed trade — the PERF-VERDICT bands are the referee.
- DDC: bootstrap/cg mirrors any stencil change per its own dialect; the
  scheduled DDC check (habu-scheduled-ddc-gate-935db9d5) is the safety net.

## Sequencing

Attribution report → ratchet the report into the gate → candidates in
measured-payoff order, one per commit, ratchet lowered each time → README
claim restored when ≤131,072 on both targets.
