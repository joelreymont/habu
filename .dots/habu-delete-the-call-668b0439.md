---
title: Delete the call-site implementation ids and the rows they index
status: open
priority: 2
issue-type: task
created-at: "2026-11-12T15:27:55.201392+02:00"
---

Problem: NEMIT:CALL-IMPL@ and CALL-LOC@ have no reader but src/compiler/session/emission.f ROWS,, which copies them into kept emissions where NART:CALL-IMPL@ and CALL-LOC@ have no caller. Each emitter fills the id per call site with NHOST:ID-OF, a scan over every published row (src/arch/arm64/passes.f, src/compiler/native/emit-x64.f, src/arch/wasm/encode.f). Without the columns ID-OF has no caller but test/wasm/encode.f, nothing reads IMPL-ENTRY, IMPL-LEN or IMPL-LIVE, and PREPARE, TAKE, PUBLISH-TAKEN, ABANDON-TAKEN, the SEED words, INVALIDATE and its HOST-INVALIDATE cell feed nothing. Acceptance: each is deleted with what then feeds nothing, or kept with its reader named in its comment; the engine builds twice, the suite passes and the consumers build cold = warm. The reason classes stay with habu-remove-the-observers-5a486a1d. Depends: none. Claim: unassigned.
