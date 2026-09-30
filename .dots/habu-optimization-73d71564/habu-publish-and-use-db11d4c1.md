---
title: Publish and use callee clobber summaries
status: open
priority: 1
issue-type: task
created-at: "2026-09-30T10:58:43.391728+02:00"
blocks:
  - habu-select-shifted-idx-c57b5f1b
  - habu-drop-the-link-f2071a86
---

Campaign: ARM64 code-size fixes, design revision 3 (§3.6). Line references are at master 8c9b75af; re-verify before editing.

Problem: MB-FORBID-CALLS (src/compiler/native/regalloc.f:1645-1660) forbids every call-destroyed register for a class crossing any call, and the machine declares the whole pool destroyed (src/arch/arm64/machine.f:109-118; abi.f:66-70, :87-99), so every value live across a call is stored and reloaded through the frame: 7,780 frame transfers in the corpus's 46,477 instructions. The schema can state a smaller destroyed set (native-effect.f); nothing produces or consumes one.

Acceptance: after acceptance publish.f computes the routine's destroyed GPR and FPR sets from the accepted module (results, fixed registers, x30 when it calls, the union of its direct callees' summaries) into a session table keyed by entry, cleared with the session and on rewind. RESOLVE-CALLABLE (hir-word.f, rows read at :1053-1080) supplies the callee's summary, or the whole pool for engine primitives, execute, deferred words, self-calls and anything not published by tier 1 in this session; the wordcall operation carries a64.clobber/a64.fclobber; MB-FORBID-CALLS and the verifier's crossing rule read the attribute. No summary is narrowed after publication. Tier-1 fixtures: a caller keeps a value in a register across a two-register leaf (no str/ldr via sp in its span) and answers correctly; the same around execute, a deferred word and an engine primitive still spills; a callee that calls a spilling word propagates the union; after undefine and redefinition the compiled caller still calls the old body and a new caller uses the new summary; a value live across catch is spilled. Artifact: suite output; call-crossing-spill census row before and after; tools/engine-size.f totals; gen 2 == gen 3; the Etch share of spills against baked callees (decides habu-persist-clobber-summaries-890d67ea).

Files: src/compiler/native/regalloc.f, regalloc-verify.f, select.f, a64ir.f, publish.f, hir-word.f, compiler.f; src/arch/arm64/passes.f only if a new module is wired (coordinate with swift); test/compiler/native-regalloc.f, native-exec.f. Run as worker-max.

Verify: tools/native-build.f product; bin/hb --load test/compiler/native-regalloc.f; bin/hb --load test/compiler/native-exec.f; census; tools/engine-size.f; tools/two-generation-build.f; bin/hb --load test/run.f. No engine text.

Depends: habu-select-shifted-idx-c57b5f1b (shared select.f/a64ir.f); habu-drop-the-link-f2071a86 (frame trait rule).

Ownership: the files above.

Claim: unassigned.
