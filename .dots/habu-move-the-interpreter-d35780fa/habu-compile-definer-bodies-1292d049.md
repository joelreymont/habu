---
title: Compile definer bodies through the compiler
status: open
priority: 2
issue-type: task
created-at: "2026-09-29T12:51:36.672233+03:00"
---

Problem: `create`/`variable`/`constant`/`defer` bodies are assembly chains (`habu2.f:3155-3232`); `aot-closure.f:683-690` and `address-carrier.f:9-30` decode their MOVZ/MOVK chains.
Acceptance: `src/habu/definers.f` compiles a synthetic body per `create`/`variable`/`constant`/`defer` through NCOMP; `DKIND` stamps as today; `aot-closure.f` `REC-CELL-SITE?`/`DATA-CELL-OWNER` and `address-carrier.f` read recorded sites through `SITES`; the `stripped-*` and `stripped-does` rows green; chain fixpoint.
Files: `src/habu/definers.f`, `src/habu/aot-closure.f`, `src/habu/address-carrier.f`, `src/habu/aot-capture.f` where it reads those sites.
Verify: spark: the `stripped-*` and `stripped-does` rows; rebuild; chain gen2==gen3; gate.
Depends: habu-close-definitions-with-8ace78d8 (I5e), habu-add-the-nemit-201c6fbf (P1), habu-represent-x86-live-729a7ac6 (P2). Serialise with X2a and X2b on `aot-capture.f` and `aot-closure.f`.
Route: Alder (shared: src/habu/definers.f, src/habu/aot-closure.f, src/habu/address-carrier.f, src/habu/aot-capture.f).
Ownership: krait (Intel lane).
Claim: unassigned.
- From I4 design: this leaf also owns the interpret-mode defer words `is`, `defer-unset` and `undefine` (dispatched before find, `habu2.f:8339-8362`).

K-lane correction (design 2026-09-30): Acceptance add the x86 `does-patch` contract and body. `LDOESPATCH` (`habu2.f:3105-3135`) rewrites the created word's final 4-byte RET slot; an x86 `ret` is one byte, so the slot is a compiler emission shape: the created body ends with a 5-byte `jmp rel32` (displacement 0) immediately before its `ret`, at a NEMIT function offset (C6 records the exact spans); x86 `does-patch` writes the displacement at slot+1 and clears `DKIND`. The body goes in the K8 section of `src/habu/kernel-x64.f` (Files add).

Lead note (2026-09-30, from the I8/I5a design): stamps DKIND through `def-open`'s kind argument (I4c).

Lead note (2026-10-01, X4c habu-link-the-shadow-74e41be7 landed): `src/habu/link-x64.f` refuses a code record the capture's shadow carries no routine for (rc 74), and the x86 kernel's `create` and `does-patch` are refusal rows, so an x86 window holding a `variable`, a created word, a `defer` or a `does>` child links only once this leaf's NCOMP bodies give those records x86 routines and the x86 `does-patch` body lands. habu-resolve-x86-entry-cb671d4d (X4d) waits on this leaf for that reason; `test/prim-parity.f`, X5's entry, uses definers.
Lead note (2026-10-09): its acceptance cannot be met as written: a created word's code pushes a data address, which NCOMP may not compile as a number (src/compiler/native/dict.f:105-109), and the AOT decoding and x86 does-patch slot serve the native capture docs/architecture.md retires ("Every codegen emits unplaced code", "Build-time data is product data"). `create` and `variable` in the Habu loop moved to habu-read-create-and-98e016f0; `is` is compile-only and `undefine`/`defer-unset` are words, not interpret keywords.
