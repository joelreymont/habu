---
title: Add the NEMIT surface with an A64EMIT adapter
status: closed
priority: 2
issue-type: task
created-at: "2026-09-29T12:51:36.501288+03:00"
closed-at: "2026-09-29T16:00:06.929381+03:00"
close-reason: landed on master c595e06e (Alder); interdiff against the reviewed bookmark empty
---

Problem: `src/compiler/native/publish.f:9,15,44-52,60-105` names `A64EMIT`, assumes 4-byte instructions (`INSN-BYTES 4`), discovers calls by decoding BL words (`RELOC-CALLS`) and shortens legacy spans (`size INSN-BYTES -` in `RECORDED-LEN`); the x86 engine publishes its own tier-1 definitions at runtime, so publication must be byte-based and target-neutral.
Acceptance: `src/compiler/native/emission.f` (package `NEMIT`) exposes `SIZE`, `BYTES`, `EXACT-SPAN`, `FUNCTION-OFFSET@`, `CALL-SITES`/`CALL-SITE@`, `ADDR-SITES`/`ADDR-SITE@` (bytes), `PLACED?`/`PLACEMENT`; `src/arch/arm64/passes.f` fills it (BL decoding moves into the adapter, once); `publish.f` reads only `NEMIT`; the `;does` companion records that `bb05d695` retains keep their exact entry through `FUNCTION-OFFSET@`; chain gen2==gen3.
Files: new `src/compiler/native/emission.f`, `src/compiler/native/publish.f`, `src/arch/arm64/passes.f`, `src/compiler/native/emit.f` (site recording), the `test/compiler/native-*` publish suites.
Verify: spark: the focused publish suites; rebuild; chain gen2==gen3; gate.
Depends: none (parallel with lane C). Serialise with P3 and X1 on `publish.f` and `src/arch/arm64/passes.f`.
Route: Alder (shared: src/compiler/native/emission.f, src/compiler/native/publish.f, src/arch/arm64/passes.f, src/compiler/native/emit.f, test/compiler/native-*).
Ownership: krait (Intel lane).
Claim: agent=krait workspace=.jj-ws/habu-add-the-nemit-201c6fbf.
Preflight corrections (these override the Acceptance and Verify above where they differ):
- `NEMIT`, bytes throughout: `SIZE ( -- n )`, `BYTES ( -- ptr u8 )`, `RET-BYTES ( -- n )` (the trailing return's length, 0 for an exact span; the ARM64 adapter answers 4 or 0 from `TRAILING-RETURN?`, `src/compiler/native/emit.f:2010`), `FUNCTION-OFFSET@ ( n -- n )`, `PLACED? ( -- bool )`, `PLACEMENT ( -- n )`, `CALL-SITES ( -- n )`, `CALL-SITE@ ( n -- n )` (byte offset), `CALL-KIND@ ( n -- n )` (`CALL` or `TAIL`), `CALL-TARGET@ ( n -- n )` (absolute), `ADDR-SITES ( -- n )`, `ADDR-SITE@ ( n -- n )`, `ADDR-SITE-KIND@ ( n -- n )`. `EXACT-SPAN` is replaced by `RET-BYTES`.
- Lifetime: `A64PASS:EMIT` fills `NEMIT` after `A64EMIT:EMIT` (BL and B decoded there once, through `NBR`); the rows are valid until the `RETIRE` row and cleared on refusal.
- `publish.f` keeps its behaviour from the rows: the `EXTERNAL?` filtering of call sites (`publish.f:93-100`, matching the tier-0 recording at `src/habu/habu2.f:650-653`) and the external-tail refusal `E-NPUB-RELOC` (`publish.f:83-91`). The `SIZE-CK`/`OFFSET-CK`/`MAP-CK` alignment checks move into the ARM64 adapter.
- Verify: the gate rows `compiler-native-code-span`, `compiler-native-trap`, `compiler-native-dictionary-publish`, `compiler-native-create-does` (and its `-aot` row), `native-tail-placement`, `test/does-clause-record.f` and `stripped-does`; rebuild; the chain (gen4 == gen5); AND preservation: the base tree `5985bcb4` built by the P1 engine is byte-identical to the base engine (`11c00585…`), which proves the recorded maps, lengths and companion entries unchanged; `rg 'A64EMIT:' src/compiler/native/publish.f` is empty; gate.
