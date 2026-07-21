---
title: Per-site relocation record for AOT capture
status: open
priority: 3
issue-type: task
created-at: "2026-07-21T16:56:23.143178+02:00"
---

Follow-up from the literal-split landing (6856f799): the emission-side separation is complete, but aot-capture.f's value-range scans (ACAP-SCAN-DATA/CODE) remain as the backstop because fully removing them needs an explicit per-site relocation record consumed at capture - a runtime recording table written at emit time (site offset + kind), reset per capture window in stdin.f, mirrored in forth.fs, serialized carefully in the fixpoint-critical AOT area. The register-kind-marker alternative was evaluated and REJECTED as value-fragile (an incidental 4-chunk scalar into x9 would collide with DATA recognition). With the record in place the scans retire and the linker chain-reject stays the only heuristic.
