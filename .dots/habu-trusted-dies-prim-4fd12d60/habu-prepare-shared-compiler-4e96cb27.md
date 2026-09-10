---
title: Prepare shared compiler boundaries for two new targets
status: open
priority: 2
issue-type: task
created-at: "2026-09-10T18:03:13.398146+03:00"
---

Owner: Cedar. Explicit user requirement from 2026-09-10: Habu architecture and directory tree must allow two additional codegen backends. Finish the review and necessary factoring of shared frontend/IR/optimization, per-target layout/ABI/code emission, and host/OS loading; existing IR and optimization passes remain shared. Preserve ARM64 behavior and remove only demonstrated coupling that blocks the target split. Review/integrate the existing generic ARM/Thumb2 and TI C6x encoder work with Kestrel and provide the compatible image handoff. Acceptance: target extension points and module ownership are clear, existing target tests pass, and independent encoder fixtures load through the generic library path. This request does not authorize implementing two complete new optimizing backends or putting application firmware policy in Habu.
