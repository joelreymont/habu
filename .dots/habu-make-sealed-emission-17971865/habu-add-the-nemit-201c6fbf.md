---
title: Add the NEMIT surface with an A64EMIT adapter
status: open
priority: 2
issue-type: task
created-at: "2026-09-29T12:51:36.501288+03:00"
---

Problem: `src/compiler/native/publish.f:9,15,44-52,60-105` names `A64EMIT`, assumes 4-byte instructions (`INSN-BYTES 4`), discovers calls by decoding BL words (`RELOC-CALLS`) and shortens legacy spans (`size INSN-BYTES -` in `RECORDED-LEN`); the x86 engine publishes its own tier-1 definitions at runtime, so publication must be byte-based and target-neutral.
Acceptance: `src/compiler/native/emission.f` (package `NEMIT`) exposes `SIZE`, `BYTES`, `EXACT-SPAN`, `FUNCTION-OFFSET@`, `CALL-SITES`/`CALL-SITE@`, `ADDR-SITES`/`ADDR-SITE@` (bytes), `PLACED?`/`PLACEMENT`; `src/arch/arm64/passes.f` fills it (BL decoding moves into the adapter, once); `publish.f` reads only `NEMIT`; the `;does` companion records that `bb05d695` retains keep their exact entry through `FUNCTION-OFFSET@`; chain gen2==gen3.
Files: new `src/compiler/native/emission.f`, `src/compiler/native/publish.f`, `src/arch/arm64/passes.f`, `src/compiler/native/emit.f` (site recording), the `test/compiler/native-*` publish suites.
Verify: spark: the focused publish suites; rebuild; chain gen2==gen3; gate.
Depends: none (parallel with lane C). Serialise with P3 and X1 on `publish.f` and `src/arch/arm64/passes.f`.
Route: Alder (shared: src/compiler/native/emission.f, src/compiler/native/publish.f, src/arch/arm64/passes.f, src/compiler/native/emit.f, test/compiler/native-*).
Ownership: krait (Intel lane).
Claim: unassigned.
