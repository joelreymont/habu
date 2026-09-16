---
title: Prepare shared compiler boundaries for two new targets
status: closed
priority: 2
issue-type: task
created-at: "\"2026-09-10T18:03:13.398146+03:00\""
closed-at: "2026-09-16T14:34:51.312907+03:00"
close-reason: "superseded by habu-campaign-c6-targets-86bb56bb: This is the targets campaign itself; the shared boundary review landed and only the qualified engine handoff and the new backends remain"
---

2026-09-14: reviewed shared HIR target binding and explicit ARM64 backend
admission are implemented. HIR owns the existing 64-bit cell policy separately
from pointer width. Rebuilt SHA91ea98b7 passes backend-boundary, native-hir,
native-select, arm32-asm and tic6x-asm; the encoder suites are now registered.
See docs/compiler-ir-design.md section 5.4 for current extension points and
host-call limits. Compatible qualified engine handoff remains pending. Joel
deferred review of the downloaded x86-64/TI DSP proposals until the current
correctness work is qualified; do not start those backend implementations here.

Owner: Cedar. Explicit user requirement from 2026-09-10: Habu architecture and directory tree must allow two additional codegen backends. Finish the review and necessary factoring of shared frontend/IR/optimization, per-target layout/ABI/code emission, and host/OS loading; existing IR and optimization passes remain shared. Preserve ARM64 behavior and remove only demonstrated coupling that blocks the target split. Review/integrate the existing generic ARM/Thumb2 and TI C6x encoder work with Kestrel and provide the compatible image handoff. Acceptance: target extension points and module ownership are clear, existing target tests pass, and independent encoder fixtures load through the generic library path. This request does not authorize implementing two complete new optimizing backends or putting application firmware policy in Habu.
