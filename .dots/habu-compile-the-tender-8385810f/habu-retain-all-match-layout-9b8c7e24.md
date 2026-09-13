---
title: Retain every recorded match-layout fact in native compilation
status: active
priority: 1
issue-type: task
created-at: "2026-09-13T15:48:42Z"
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
