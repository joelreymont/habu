---
title: Remove bounds checks from an optimized build
status: active
priority: 1
issue-type: task
created-at: "2026-09-22T11:30:53.087679+03:00"
---

Problem: Joel (2026-09-22): 'bounds checking should be removed during optimization, unless we are in debug mode' - unproved either way. Probe: a stripped program with a SPAN:U8! loop over a span, objdump -d, the compare-and-branch pairs per access counted; then the same under whatever release or no-check build mode exists - if none exists, that absence is the finding. Acceptance: the per-access check count recorded with the engine sha for both modes, and either a release mode that drops the checks or the reason one cannot exist yet; Tender's bin/tenderd re-measured by hb-build's size line. Verification: the images and disassembly under ~/.cache/tender/habu-gaps/bounds-checks-release/, then Tender's python3 scripts/habu.py build --server. Ownership: alder. Claim: agent=alder workspace=.jj-ws/alder-size-probes.


Initial measurement on the same engine: a native `SPAN:U8!` with index 999
against a one-byte span still throws `E-SPAN-RANGE` at runtime. The probe's
JIT listing retains the span frame and loop compare/branch; no release or
no-check build mode was found. The optimization question therefore remains
open, but removing this check without a proven range would change semantics.

Measured on integrated source 06c5c9e9, engine SHA256
`3da80b230e3040b53ffea3bf403a45534c8bd490178daf50af6c8babc95cea11`:
the stripped loop `span SPAN:LEN 0 ?do 65 span i SPAN:U8! loop` retains
the call through U8! to AT on every iteration. AT is 156 bytes and executes
two range CMPs (index < 0, index >= reach), two CSET/NEG pairs, ORR and one
conditional branch. The loop's own induction comparison has not eliminated
either range comparison. The inline failure arm carries throw plus a fallback
die path. The primitive c! separately calls PROT-SPAN; that store-protection
check is distinct from the span range rule.

The native loop body is 156 bytes, U8! is 36, and c! is 44. Two immediately
cancelling SUB/ADD pairs on x19 occupy sixteen of U8!'s thirty-six bytes:
image offsets 0x1740/0x1744 (32 bytes) and 0x174c/0x1750 (16 bytes). This is
evidence for existing planner dot d187f629, owned by Hazel; no new issue needed.

The valid image runs with status 0 and prints AA, checking the first and last
bytes of the sixteen-byte span. Index 999 is refused: the stripped image exits
67, and a caught call on the same native host returns -6100 (E-SPAN-RANGE).
Both builds finish with status 0. The valid image has 4,376 code bytes and is
65,728 bytes including ELF padding. The build usage and option parser expose
no release/debug/no-check mode, so there is no second mode to compare.

Sources, images, emitted member offsets, exact GNU objdump slices, size output
and runtime results are preserved outside /tmp at
`~/.cache/tender/habu-gaps/bounds-checks-release/engine-3da80b23/`.
The element type alone does not prove a dynamic index is in range; the
remaining optimization is to prove and eliminate redundant checks, including
the loop case, without changing the invalid-index behavior. No compiler or
library source was changed. This dot remains open for that optimization.
Current owner: alder (measurements), Hazel (compiler implementation); workspace
`.jj-ws/alder-bounds-proof`.
