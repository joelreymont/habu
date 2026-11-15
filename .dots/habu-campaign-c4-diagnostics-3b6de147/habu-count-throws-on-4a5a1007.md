---
title: Count throws on Wasm as on arm64
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T12:53:24.529874+01:00"
closed-at: "2026-11-16T04:07:36.576612+01:00"
---

Problem: src/arch/wasm/link-routines.f THROW-BODY, FINALLY-BODY and CATCH-BODY never touch THROWS-CELL, so REJECTION:CARRIED? is never true on Wasm and a passed record is re-saved at the outermost throw. Acceptance: a Wasm module places a nested require's rejection at the inner file, as arm64 does. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
