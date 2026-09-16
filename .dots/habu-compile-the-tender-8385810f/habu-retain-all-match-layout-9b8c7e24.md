---
title: Retain every recorded match-layout fact in native compilation
status: closed
priority: 1
issue-type: task
created-at: "\"2026-09-13T15:48:42Z\""
closed-at: "2026-09-16T14:34:47.954786+03:00"
close-reason: "done: Match-layout facts now go to the growing CWIN owner store with a distinct kind, so the 24-fact table and its silent drop are gone. [src/core/checker.f:8702-8705 MWIN-COMMIT calls CW-MATCH-CELLS CWIN-ADD; PLAN.md records the actual 76-arm enum compiling and executing]"
blocks:
  - habu-compile-the-tender-8385810f
---

The native checker stores only 24 layout facts in MWIN-TAB. MWIN-COMMIT
silently drops subsequent rows; EFFECT-MATCH-CELLS then reports absent facts
and native MATCH lowering refuses with E-NELAB-MATCH (-8650). This is normal
recording, independent of the separate NFAM source-owner mismatch. On private
host 158fe0ffbdafcd189710567f37bd6dc2197d366871d2841922a43237d7021e91,
an all-tier1 core rewind followed by a public derived enum with 23 variants
passes; 24 variants fails because its family token plus arms require 25 facts.
The compiler's A64IR opcode enum has 76 variants and fails at its generated TAG.
Ignored reproductions: /tmp/cedar-family-bridge-count-{23,24}.f and
/tmp/cedar-family-bridge-opcode.f in cedar-owner-typed.

Own checker.f's recorded layout-fact storage/commit/read/reset/retry ownership
and focused native-match regressions. Reuse the existing growing CWIN owner
store with a distinct layout-fact kind if its lifecycle and ordinal keys fit;
otherwise use equivalent checked growth. Do not replace 24 with a guessed cap,
drop facts, infer a missing width, or skip native validation. Allocation/range
failure must refuse before partial publication. Preserve absent-site refusal,
parametric construction padding, retry rollback and source-owner transfer.

Acceptance: the 23/24 boundary, at least the actual 76-arm compiler shape,
and a wide construction beyond the former limit compile and execute correctly
through real tier1 load. Missing/non-exhaustive/wrong-width controls still
refuse, and a rejected recording cannot contaminate the next accepted one.
Run the existing native-match suite with its obsolete overflow expectations
replaced by meaningful runtime checks; then complete the all-tier1 compiler
bridge and product-hosted rebuild. The normal full gate alone was insufficient:
native-match explicitly expected the former ceiling to refuse, while the
compiler implementation was still JIT-built.

The same owner had two mapping leaks: CWIN-ENSURE replaced its pointer with
ARENA-BYTES-GROW's fresh mmap without releasing the prior capacity, and
CHECKER-CAPTURE-SCRATCH-PREPARE cleared the retained pointer/capacity without
unmapping it. No other owner retained either allocation. Layout facts now use
CWIN kind -4; CWIN growth releases the superseded mapping after copying, and
REC-RELEASE resets recording/latches and releases the retained mapping at the
existing capture seam. Ordinary definition reset keeps capacity for reuse.
Retry restores one CWIN count for call and layout facts together. Finalization
selects only CW-CALL-RAW; call/glue/payload/quotation readers compare exact kinds.

The native-match regression now executes its former 26-row refusal and the
widened constructor after 24 dispatch facts. Its additional load fixture reads
the actual opcode enum from a64ir.f and compiles it in a fresh package, then
executes TAG and derived equality. Ownership checks use MAPPED:LIVE? against
the mapping immediately preceding growth and capture cleanup, verify retained
rows, separate query kinds, cleared pending facts, reuse and repeated release.

Focused validation in cedar-match-layout: the standard native-build rebuilt
private bin/hb (SHA-256 5b969df80ce504da56583533bab0591c62bf9d507c3d34558297ac2d4937d40a).
Native-match and native-quot pass with test/compiler/aot-mode.f; the signature
pool suite passes. This is functional evidence from a normally built native
host. The paired all-tier1 compiler bridge and integrated full gate remain
with the design/integration lanes; this binary is not that bridge artifact.
