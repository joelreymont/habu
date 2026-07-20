---
title: Emit direct BL for every known native call
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-03T17:59:14.114649+02:00\""
---

Measured root defect, expanded from the earlier AOT-only compaction proposal on
2026-07-19. C-CALL-EMIT-ABSOLUTE emits every known non-inlined dictionary call as
movz/movk/movk x16; blr x16: 16 bytes even though the entire runtime dictionary and
code area share one fixed 8 MiB REGION, far inside BL's signed 128 MiB range. A
measured caller is 36 bytes; direct BL makes it 24 bytes, exactly 12 bytes smaller per
call. The captured AOT REPL contains 156 such sites, so this removes exactly 1872 bytes
from its current blob before counting ordinary runtime-compiled code. The fixed
INL-MAX policy currently compares callees against this inflated 16-byte call cost, so
this is also the prerequisite for an honest inlining cost model.

A 2026-07-19 whole-live-dictionary census on a fresh fixpoint strengthens the scope:
the 4,296-record startup dictionary has 4,277 live code records totaling 856,716
bytes, and contains 9,523 exact `blr x16` absolute-call tails versus only 163 direct
`BL` instructions. Replacing those statically known calls with one `BL` removes up to
114,276 bytes from the runtime-generated code, 13.3% of the current live JIT image,
in addition to the 1,872 captured-AOT bytes above. The acceptance census must classify
each site through relocation metadata rather than trusting opcode coincidence, and
must report dynamic `execute`, deferred, and FFI calls separately.

Do the root fix at emission time, not the old post-capture reflow. Make C-CALL emit one
BL imm26 for a statically known dictionary target. Teach aot-capture.f to recognize,
resolve, record, canonicalize, and boot-patch that 4-byte BL site directly; because the
captured blob is already compact, no instruction reflow or remapping of the 119 other
PC-relative instructions is needed. Convert the two explicit helper call emitters
(LP2VEXEC and LPROTSPAN) through the same registered-helper relocation contract. Keep
dynamic execute/FFI calls indirect. Prove the range from REGION and fail closed if a
future layout violates it; do not silently choose the old absolute chain. Mirror the
native bootstrap dialect.

Acceptance: disassembly pins the measured 36-to-24-byte caller and one direct BL at
each known call site; the captured AOT report still resolves all 156 calls and its blob
shrinks by exactly 1872 bytes with no unresolved target; boot relocation rewrites only
imm26 and preserves the absolute callee; forward/backward, sibling, registered-helper,
recursive, quotation, defer, DOES>, package, snapshot, stripped-AOT closure, and
out-of-range negative cases pass; non-call instructions are byte-identical;
the startup-dictionary census proves every converted call was statically known and
reports the exact runtime byte reduction, with dynamic calls unchanged; native
fixpoint x2, both target builds, full AOT suites, host/filemap/dot lints, and full gates
pass; CODELEN and whole-file ratchets are lowered honestly. Compact u16 blob offsets
remain fail-closed at 64 KiB. Files: src/habu/habu2.f, src/habu/aot-capture.f,
src/habu/aot-closure.f tests, bootstrap/cg/forth.fs, AOT tests, and size gates.

Claim RELEASED 2026-07-20 (agent=aotbl, spark): no implementation landed - the lane STOPPED on blocking evidence, see below.

2026-07-20 BLOCKED ON A DESIGN DECISION (aotbl lane, spark; evidence independently re-verified by the orchestrator).

STAGE (c) IS PHYSICALLY IMPOSSIBLE AS SPECIFIED. Live JIT code is emitted into
the REGION at RBASE-VA = $300000000 (src/habu/layout.f:8); the engine __text
holding the LP2VEXEC/LPROTSPAN helper bodies loads at VMBASE = $400000
(src/os/linux/elf.f:25). The gap is 12.00 GiB = 12,880,707,584 B, versus BL
imm26's +/-128 MiB reach: ~96x out of range. Those two emitters use the
absolute movz/movk/movk x16; blr x16 chain (habu2.f:4874-4875, :4901-4902)
PRECISELY BECAUSE the callee is 12 GiB away. "One BL to the helper" cannot be
encoded for a live call; the registered-helper BL contract only closes in the
STRIPPED/CLOSURE image, where the helper body is copied adjacent to its callers
(aot-closure.f:166-173 FINDPTR allowlist, aot-lib.f:263-267 PATCH-BL reflow).
Implementing the dot literally would emit an out-of-range displacement on every
live typed accessor.

THE PREMISE COUNTS ARE STALE. A fresh capture measured 219 call sites (recs=115,
blob=21376 B, names=956), 0 unresolved, and 0 helper sites - not 156. So the
"blob shrinks by exactly 1872 bytes" acceptance is unmeetable; 219 x 12 B =
2628 B. Every one of the 219 resolves to an ordinary dictionary word.

STAGES (a)+(b) REMAIN SOUND: call site and callee both live inside the 8 MiB
REGION, so disp = target - CP is always in BL range and a REGION-derived
fail-closed guard is trivial; the stage2 fixpoint is assembler-emitted and
C-CALL-independent, so the builder has the BL emitter before any capture runs.
They cannot be landed in isolation, though, because the ACAP wire format
(aot-capture.f:57-63,217-233) and boot patch (habu2.f:3653-3677) must decide
BL-only vs dual-format, and that decision DEPENDS on the helper resolution -
committing BL-only now pre-commits a design the correct helper handling would
have to undo (forbidden churn).

ROOT CAUSE (orchestrator, 2026-07-20): the 12 GiB gap is a LAYOUT CHOICE, not a
law. RBASE-VA is a constant (layout.f:9) and the region is mapped MAP_FIXED at
that constant (habu2.f:3315, fail-closed exit 78). Map the code region within
BL's +/-128 MiB of the engine __text and EVERY call becomes BL - dictionary
calls AND the helper calls stage (c) wanted. The dual-format ACAP question
disappears; the result is ONE call format, simpler than today's, and this dot
becomes implementable exactly as written. Keeping the helpers absolute is
designing around the defect instead of removing it - rejected.

Two real costs, both of which are latent defects worth paying off rather than
reasons to keep the gap:

1. macOS is PIE (macos/macho.f:12, `PIE? -1`), so its __TEXT slides at load and
   a fixed near-text RBASE-VA cannot work there. The region must be mapped
   RELATIVE to the runtime-discovered text base: mmap with a hint, verify the
   returned address is in BL range of __text, and die named if not (never a
   silent fall back to the absolute chain). Linux is non-PIE at VMBASE $400000
   so it could use a constant, but the mechanism must be uniform across targets.
2. CELL-TEXTPTR? (aot-lib.f:99-100) discriminates code/dict pointers from data
   by testing membership in the RBASE-VA window, and AOT-DATA-TEXTPTR? scans raw
   data cells with it. That is a MAGNITUDE HEURISTIC that is only safe because
   12.9 GiB is implausible as user data; move the window down near the text and
   false positives become likely. It must be replaced by explicit relocation
   metadata first - the same principle this dot already demands for the census
   ("classify each site through relocation metadata rather than trusting opcode
   coincidence"). Fixing it is independently correct: identifying pointers by
   how big they are is wrong regardless of where the region sits.
3. RBASE-VA is baked into snapshots, so the move bumps SNAP-FORMAT-VERSION
   (layout.f:11, currently 3).

SEQUENCE (this dot is now the third step, unblocked and implementable AS
ORIGINALLY SPECIFIED once the first two land):
  A. habu-identify-code-pointers-b973e6cc - replace the CELL-TEXTPTR? magnitude
     heuristic with explicit relocation metadata.
  B. habu-map-the-code-5268af94 - hint-and-verify region mapping within
     +/-128 MiB of __text on both targets, fail closed, SNAP version bump.
  C. THIS DOT - direct BL for every statically known native call, one format,
     dictionary and helper alike. Re-baseline the acceptance count to the
     measured site count at implementation time (219 / 2628 B as of 2026-07-20),
     not the stale 156 / 1872.

2026-07-20 PREREQ A LANDED (e5ecd233, dot habu-identify-code-pointers-b973e6cc closed): aot-lib.f CELL-TEXTPTR? now classifies from live dictionary extents, not magnitude. RESIDUAL FOLDED INTO THIS DOT: aot-capture.f ACAP-SCAN-DATA (:248) / ACAP-SCAN-CODE (:269) still classify x9-chain literal VALUES by range against the DATA/code spans - converting them needs an emit-time relocation kind tag threaded through the ~9 C-LIT/C-X9-LIT sites in habu2.f, which is exactly this dot ("record relocation kind and site explicitly when emitting an address"; also the habu-separate-scalar-and-dffe142e acceptance). Sequence stands: B (habu-map-the-code-5268af94, now unblocked) then this dot, which must ALSO retire the capture-side value-range scan.
