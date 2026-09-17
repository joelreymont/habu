---
title: "Publish each quotation body's span to the linker"
status: open
priority: 3
issue-type: task
created-at: "2026-09-17T18:24:32.552490+03:00"
---

Problem: src/habu/aot-closure.f BODY-END-SCAN (habu-let-a-stripped-0a064bf5, 2026-09-17) derives where an anonymous quotation body ends from the next recorded code literal inside the owning record, else the record's end, because the emitter (src/compiler/native/emit.f FUNCTION-OFFSET@) lays out function 0 then the quotations in order but publishes no per-function span; an over-estimate copies dead bytes and an emitter that interleaved functions would under-copy. Acceptance: the emitter publishes a per-function span row for every quotation body (beside the payload span table or the address map), the linker's ADD-BODY-CLO reads it instead of scanning, BODY-END-SCAN is removed, and a fixture with two quotations in one initializer pins the exact byte ranges. Files: src/compiler/native/emit.f, src/habu/aot-closure.f, src/habu/layout.f (if a table is added), test/compiler/aot-xt-cells.f. Verify: test/compiler/aot-xt-cells.f; test/gate-aot-positive.f; byte fixpoint; test/run.f. Depends: habu-let-a-stripped-0a064bf5. Ownership: AOT linker and emitter. Claim: unassigned.
