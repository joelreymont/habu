---
title: Emit direct BL for every known native call
status: open
priority: 1
issue-type: task
created-at: "2026-07-03T17:59:14.114649+02:00"
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
