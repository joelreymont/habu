---
title: Fix wasm dynamic test quotation refusal
status: open
priority: 2
issue-type: task
created-at: "2026-10-22T08:16:30.514098+03:00"
---

Problem: `bin/hb --load test/wasm/dynamic.f` (also run first by test/wasm/device.f; not an ordinary gate row, needs wasm-tools and bun) exits 67 with only `hb: uncaught throw code -8651` (E-NELAB-QUOT) and no diagnostic. Measured 2026-10-22 on master db9290cb with that tree's engine (built from it in .jj-ws.noindex/jerry-merge) and on master c453e484's engine: both rc 67. test/wasm/link-capture.f passes on the same engines. Unknown: which definition the native elaborator refuses (the throw escapes without the ncomp: cannot compile line), and whether this is habu-compile-an-empty-2d88de28 (unconsumed quotation at tier 1) or a new case. Acceptance: test/wasm/dynamic.f and test/wasm/device.f exit 0 on a master engine; if the refusal is legitimate, the elaborator names the definition and token. Files: test/wasm/dynamic.f, src/arch/wasm/dynamic.f, src/compiler/native/elaborate.f. Verify: bin/hb --load test/wasm/device.f. Claim: unassigned.
