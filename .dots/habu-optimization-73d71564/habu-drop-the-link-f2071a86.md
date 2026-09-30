---
title: Drop the link save for terminal-only words
status: closed
priority: 1
issue-type: task
created-at: "2026-09-30T10:58:28.244238+02:00"
closed-at: "2026-09-30T14:27:29.118916+02:00"
close-reason: "Landed as 'Drop the link save for terminal-only words', stacked on the division slice. A word whose only calls are the engine's terminal throw/die takes a leaf contract with no link save. The contract keys on A64SEL:MAKES-CALLS?, taken on the post-fold module SELECT walks, so it agrees with CALLED-CK by construction. VLINK-OWED-CK in the verifier refuses a returning routine that has a DBACK call site but no link save; a hand-built native-regalloc case proves it fires. Census terminal-only-frame 266 sites / 2,128 B -> 0. Gen-2 aot/code-blob with the division slice, against master+S12: 1,391,616 -> 1,390,344 (-1,272 B; about -928 B from this slice). Engine image 2,477,047 B, unchanged. Gate at the combined tip: full suite 501/501 (one curl-http flake, also seen on master+S12, green on rerun); gens 2-5 byte-identical. Untested: the verifier's NO-RET early exit."
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

Claim: agent=heron workspace=.jj-ws/arm-link
