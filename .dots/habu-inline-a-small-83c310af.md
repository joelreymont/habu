---
title: Inline a small callee in the native chain
status: active
priority: 2
issue-type: task
created-at: "\"2026-08-03T12:23:59.463795+02:00\""
---

Measured in tools/codegen-compare-corpus4.f (the fourth codegen table). The engine's emitter COPIES a callee's body into its caller instead of calling it: src/habu/habu2.f C-CALL, threshold $28 constant INL-MAX = 40 bytes of body (clen <= 56 for a callee with the standard sub sp,#16 prologue, whose span is clen-16; clen <= 40 for one without, whose return slot must still hold a Ret), and C-CALL-SCAN-SAFE then refuses the copy if the span holds a Bl, B, B.cond, Cbz/Cbnz, Tbz/Tbnz, Blr, Br, Ret or Adr/Adrp. The native chain has no inliner: src/compiler/native/select.f emits a Bl for every call at every size. Counted on emitted machine code by tools/codegen-compare-test.f: CODEGEN-CORPUS4:CALL-FAN (five call sites) contains ZERO Bl in the engine's code and FIVE in the chain's; CALL-LOOP-3 zero against three; TINY-CALLEE zero against four. Cost effect in the fourth table (idle 12-core Apple Silicon, entry taken off): TINY-CALLEE old 56.0 ns / new 70.7 ns, CALL-LOOP-3 old 47.7 ns / new 65.5 ns. Both are loops whose body is nothing but calls to a 40-byte callee. What to build: a copy of a callee's selected machine body into the caller at selection time, under the same safety rule the engine applies (no branch, no Ret, nothing PC-relative in the span) plus the chain's own extra condition that the callee's routine contract be a leaf with no frame. Depends on nothing; the fourth table is the yardstick and its rows are pinned in tools/codegen-compare-test.f. Owners: A64SEL, NMIGRATE.

Claim: agent=inlinelane workspace=.jj-ws/habu-inline-a-small-83c310af
