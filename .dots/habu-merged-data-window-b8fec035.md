---
title: Merged DATA window is appended unaligned
status: open
priority: 2
issue-type: task
created-at: "2026-08-16T02:00:18.215234+02:00"
---

Proven by bake-chain-10 (2026-08-16): aot-file.f MERGE-DSITES appends the artifact's DATA window at H-DATA with no alignment, shifting every merged chain DATA address by a non-multiple of 8. Measured: A64RAV:DKEEP-HOOK's cell at 0x4400061d60e (aligned=NO) in the merged engine vs 0x...628 aligned in the source-loaded chain. Self-consistent today (written and read at the same skewed address) so not the current crash - but LDAR/STLR and atomics FAULT on misaligned addresses (SEAL-WIDS, already uses LDAR/STLR on the protected band; any chain word using atomics on its own DATA will die), and post-seed DP inherits the skew for every later allocation. Fix: align the merged window base up to 8, pad the content, and extend the merge suite's sum checks to prove the pad (merged DATA size = artifact size + pad, all rebased addresses 8-aligned when their source was). Regression: an alignment assert over the merged engine's declared cells.

MEASURED AND ON THE MILESTONE'S CRITICAL PATH 2026-08-16
(bake-chain-11, dot c970bf04's lane; ruling 2 put this dot in the same
lane, as a SEPARATE commit).
IT IS NOW A CRASH, not a latent risk. With c970bf04's three does>-links
repaired at runtime with patch32, the merged engine gets past the
SIGSEGV and dies SIGBUS (sig 10) at FIND-B+172 (wid 360) on
`ldaxr x9,[x9]` with x9 = 0x440004f065f - odd. That is this dot's
predicted failure, reached by the real workload:
`s" : FOO ( n -- n ) 1 + ;" 1 1 8 NMIGRATE:DEFINE 7 FOO .`
THE SKEW IS EXACTLY -1, AND ONLY THE ALIGNED CELLS BREAK. Census of
every `create`/`variable` body (len 24, movz-x9 address chain) decoded
in two processes - the merged engine, and a source-loaded chain
(`bin/hb --load tools/aot-chain-capture.f`) - matched by name+wid:
1133 comparable cells, 1120 keep their 8-residue, 13 change it, and
all 13 shift by exactly -1 (mod 8 histogram: {7: 13}). The 13 are the
cells that were 8-ALIGNED in the source-loaded chain - STG-MODE, STG-V,
STG-N (wid 350), BND-MODE (wid 414 and 416), and the host-window
BPW-TAB/BPW-IDX/BPW-LAST/SBUF/SLEN/SPOS/STEPPING - i.e. `variable`s,
which are the only cells an atomic may legally touch. The other 1120
are `create ... allot` byte buffers that were already unaligned in both
and so cannot show the skew: a census that does not match residues
PAIRWISE reads 740/369 in the pre-window band and concludes nothing.
WHY THE HOST-WINDOW WORDS MOVE TOO: their DATA comes from DP after the
seed, so the artifact window's unaligned LENGTH shifts them as well as
its unaligned BASE shifting the chain's. One cause, two populations.
WHERE IT IS: aot-file.f `H-DATA @` is the host's AOT-DATA-SIZE latched
by LATCH-HOST (~668) and used unrounded in three places -
BASES-AFTER-HOST `H-DATA @ S-WDATA BASE!` (~692), MERGE-DSITES
`... AOT-DATA-D0 @ + H-DATA @ + SNAP-RELOC:SET-CHAIN` (~822) and
MERGE-COUNTS `H-DATA @ S-WDATA ROW-LEN@ + AOT-DATA-SIZE !` (~859).
Rounding that base up to 8 in all three is the fix.
ONE THING TO SETTLE BEFORE EDITING, and it is why this was not done in
the same sitting: the pad bytes between H-DATA and the rounded base
must reach the seeded image as ZERO. The DATA payload buffer's reset
discipline (the SMEAR in tools/aot-chain-capture.f) has to be read
first - if it writes a poison byte rather than zero, the pad must be
zeroed explicitly, because those bytes become live DATA in the booted
engine.
THE PROOF TO ADD: test/aot-file-merge.f already walks the merged DATA
sites in ?DSITES (~355) and checks each chain value lands in the merged
window. Extend it with the residue rule this census measured - a merged
DATA address whose artifact-side address was 8-aligned is 8-aligned -
plus the sum check the dot asks for (merged DATA size = rounded base +
artifact size). The mutation that reds it by name is removing the
rounding.
