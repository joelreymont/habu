---
title: Drop the link save for terminal-only words
status: open
priority: 1
issue-type: task
created-at: "2026-09-30T10:58:28.244238+02:00"
blocks:
  - habu-divide-through-div-1118f223
---

Campaign: ARM64 code-size fixes, design revision 3 (§3.2). Line references are at master 8c9b75af; re-verify before editing.

Problem: EMIT-TERMINAL (src/compiler/native/select.f:2637-2641) counts a terminal site in N-CALLS; CALLED-CK (select.f:520-525) then requires T-CALL, and the contract's traits (select.f:3532, from NABI:CALL-FRAMED, abi.f:106-116) make the routine push and pop x30 around a body whose only BL never returns (BTHROW restores x30 from the catch frame, habu1.f:2676-2700; die exits). ARRAY:A-LEN in docs/compiler-measurements.md is the shape: 8 of its 76 bytes.

Acceptance: a terminal site counts as a trap for the link obligation; a body whose only non-returning sites are terminals or traps gets the leaf contract; regalloc-verify.f applies the same rule. test/compiler/native-trap.f gains a guard word ( n -- n ) throwing on a negative argument: its span has no `str x30`/`ldr x30`, it answers for a valid argument, and a child catches its refusal with the right code; a word with one returning call keeps both instructions. FM-GUARD in native-fused-moves.f keeps passing. Artifact: suite output, the terminal-only-frame census row (−8 bytes per word), tools/engine-size.f before and after, gen 2 == gen 3.

Files: src/compiler/native/select.f, the contract chooser (the caller of NABI:CALL-FRAMED in compiler.f or backend.f), regalloc-verify.f, test/compiler/native-trap.f.

Verify: tools/native-build.f product; bin/hb --load test/compiler/native-trap.f; bin/hb --load test/compiler/native-fused-moves.f; census; tools/engine-size.f; tools/two-generation-build.f; bin/hb --load test/run.f. No engine text.

Depends: habu-divide-through-div-1118f223 (shared select.f).

Ownership: the files above.

Claim: unassigned.
