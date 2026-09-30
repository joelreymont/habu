---
title: Model the code-provenance band on x86
status: closed
priority: 2
issue-type: task
created-at: "2026-09-30T09:22:47.271870+03:00"
closed-at: "2026-09-30T14:47:34.483613+03:00"
close-reason: "done: X64PROV (src/habu/code-origin-x64.f) models the TIER-PROV band on x86; patch32 invalidates, code-publish records unknown, code-origin queries, BOOT-OPEN, marks the kernel text native. [test/x86-64-kernel-engine.f hb-x64-kernel-origin and -origin-patch exit 0, -origin-full 101; the kernel-entry check exited 21 on the base.]"
---

Problem: `code-origin` has x86 consumers that must answer truthfully: `snap-lib.f:453` (retained code without native evidence gives rc 100), `tools/native-build.f:9` (the self-build driver, G3) and `test/tier.f:366-371`. Dropping provenance on a tier-1-only engine would report `patch32`-written bytes as native, the exact hole `habu1.f:2417-2419` closes. Decision (K-lane design, 2026-09-30): model the band on x86.
Acceptance: `src/habu/code-origin-x64.f`, package `X64PROV`: twins of `code-origin.f` `EMIT-SET`/`EMIT-QUERY` (registered `engine-code-origin-set`/`engine-code-origin-query`) and `INVALIDATE,`/`UNKNOWN-RANGE,`/`NATIVE-RANGE,`/`QUERY,`/`OPEN,`/`CLOSE,` over the same DATA layout (`layout.f:1775-1785`). Add the calls into K8b's `patch32` and `code-publish` and K9a's `code-origin` row. Cases through the booted harness: a published span answers native, a `patch32` inside it answers unknown, an untouched address answers unknown, each with a negative twin.
Note for lane I: every `TIER-PROV:OPEN,`/`CLOSE,` call site is the assembly interpreter (`habu2.f:2783,7704,9259-9381`), so I5e adds provenance open/close rows to `prims.f` with bodies on both targets; this leaf provides the x86 bodies.
Files: `src/habu/code-origin-x64.f`, `src/habu/kernel-x64.f`, `test/x86-64-kernel-engine.f`, `docs/x86-64.md`.
Verify: host K3's product engine: `--load test/x86-64-kernel-engine.f`; ThinkPad: the images natively.
Depends: habu-emit-x86-code-973a0074 (K8b), habu-emit-x86-engine-86b5f8e7 (K9a).
Route: direct (x86-only files).
Ownership: krait (Intel lane).
Claim: krait.

Preflight corrections (2026-09-30; override the lines above where they differ):
- Citations: TIER-PROV is `layout.f:1832-1840`; the hole is `habu1.f:2316-2318`; the assembly interpreter's provenance sites are `habu2.f:2795, 7755, 9335-9457`.
- `code-publish` certifies nothing: as `BCODEPUBLISH` (`habu1.f:2472`, "only the compiler return may certify this emission"), x86 `PUBLISH,` (`kernel-x64.f:1889`) calls `X64PROV:UNKNOWN-RANGE,` over [dst, CP). Native evidence comes only from `X64PROV:OPEN,` then `1 X64PROV:CLOSE,` after a successful compile (ARM64 `habu2.f:2795`, `7755`). Cases: `OPEN,`, `code-publish`, `1 CLOSE,` answers 1; a bare `code-publish` answers -1.
- Engine text: ARM64 marks the engine text native at boot (`habu2.f:7513-7516`); x86 `START,` makes no provenance call. Add `X64PROV:TEXT-NATIVE, ( label label -- )` marking the kernel text native; call it at the entry after `KERNEL,` in `X64HARNESS:BOOT-OPEN,` (`test/x86-64-boot-harness.f:49-58`); X4d (`habu-resolve-x86-entry-cb671d4d`) calls it at the product entry. Case: a kernel row's entry answers 1 (pre-change failing check: it answers -1 today, `kernel-x64.f:2615-2617`, where ARM64 answers 1; `test/tier.f:367`). Files add `test/x86-64-boot-harness.f`.
- Split, merge and shift paths (`EDGES,`, `MOVE,`, `PUT,`, `code-origin.f:53-130`) get cases: after a `patch32` inside a native span the bytes before and after the patched word still answer 1; an unknown span published after it still answers -1 (row shift); two adjacent native closes leave `TIER-PROV:N-CELL` at 1.
- Register contract: the ARM64 `OPEN,`/`CLOSE,` preserve every register; the x86 bodies may clobber rax rcx rdx rsi rdi r8-r11 (I5e's row bodies rely on this statement).
- Out of scope: `CAPTURE-ORIGIN!`, `CAPTURE-RANGE,`, `ABANDON,`, `RESTORE-REGION,` (x86 refuses `snap-rebase`, `kernel-x64.f:2601`); the captured-region rows at boot belong to the linker leaves X4b-X4d.
- Verify: host the stack fixpoint engine (not K3's product): `--load test/x86-64-kernel-engine.f`; ThinkPad: the images natively.
