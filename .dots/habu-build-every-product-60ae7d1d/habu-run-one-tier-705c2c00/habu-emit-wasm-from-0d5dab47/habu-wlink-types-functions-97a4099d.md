---
title: WLINK types functions by lanes and origin that only its tests use
status: open
priority: 4
issue-type: task
created-at: "2026-11-10T20:22:09.826620+02:00"
---

Problem: src/arch/wasm/link.f FUNCTION+ takes in and out lanes, a frame variant and an origin (kernel, captured, adapter), which tier 1's capture link (src/habu/link-wasm.f, removed) used. LINKER's writer (src/arch/wasm/link-module.f) adds only aligned-frame functions; the other shapes are reached by test/wasm/link.f, test/wasm/device.f and test/wasm/w03.f alone. Acceptance: WLINK keeps one function shape, or the lanes stay with a stated user. Files: src/arch/wasm/link.f, link-module.f, test/wasm/link.f, device.f, w03.f. Verify: engines converge; test/wasm/link.f; test/wasm/device.f. Depends: habu-wasm-3-remove-5698c955. Ownership: none. Claim: unassigned.
