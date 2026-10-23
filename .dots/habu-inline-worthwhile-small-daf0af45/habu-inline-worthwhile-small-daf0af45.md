---
title: Inline worthwhile small native calls
status: open
priority: 2
issue-type: task
created-at: "2026-10-14T16:55:46.872105+03:00"
---

Otto reports404 reachable routines of64 bytes or less and454 checked-store calls. Determine real cost and safe existing compiler opportunities; inline only where actual effects, checked-store bounds, exceptions, code-size and call semantics remain correct. Estimated3–5KB savings are unverified; do not remove checks based on Zig comparison. Acceptance: observable success/refusal E2Es written or selected first, measured same-source behavior and code-size improvement; native gate, convergence and Astra review.

The first bounded slice is seeded min through the existing HIR expansion path, alongside max. Its signed comparison/selection semantics are authoritative in the primitive reference and avoid subtraction overflow. Preserve exact recorded binding, source ordinals, scalar-width refusals, shadowing and redefinition; share the maximum selection helper where useful. This is not arbitrary small-word inlining and does not close the broader task. Use existing primitive parity, inline-primitive attribution, native-word binding and string comparison E2Es; add only the distinct min binding case before implementation. Actual changed-emitter execution and unchanged-source code-size/hot-path measurement are required, not a baked baseline run or estimated savings.

The native product built from ed94c47b passes native-word-binding and undefine-binding at both tiers. On identical tier-1 fixture/census inputs, MIN-BEFORE and MIN-AFTER each change from 16 bytes, four instructions and one call to 36 bytes, nine instructions and no call. The package-shadowed MIN-INSIDE remains 16 bytes, four instructions and one call. This demonstrates exact seeded expansion and a 20-byte increase per measured wrapper; it establishes no runtime speedup or overall code-size saving. High dictionary-ordinal runtime coverage remains for the integrated gate.

A three-million-call loop with the same checksum retires about 2.41 fewer instructions per min call after subtracting each engine's zero-iteration startup. Both wall times round to 0.05 seconds, so throughput improvement is unresolved. The eight-word probe grows from 220 to 232 bytes overall. General small-word and checked-store inlining remain unfinished; these measurements do not establish Dots-wide size savings.

A paired checked-store form was tried with the existing span helper and call-save boundary, without relaxing its guard. The exact-parent source-only compiler loaded and executed tier-1 stores; a protected crossing still exited with SEAL-VIOLATION. On the same two-million-byte-store probe, the original source retired 15.52 instructions per store and the paired form 37.07, after subtracting each zero-iteration run. The compiled loop grew from 92 to 112 bytes, and the ARM emitter scratch ceiling would rise from three to eight instructions per operation. The candidate was reverted because both measured runtime and code size worsened. Keep the distinct tier-1 protection-span regression case; pursue a different lowering only with a measured hot-path gain.

A bounded scalar-leaf recipe trial captured the exact three-comparison JSON escape predicate through the source-loaded native compiler. Expanding it grew a JW-STR-N-shaped caller from 164 to 196 code bytes; a JW-RUN-END-shaped caller stayed 96 bytes. The existing JSON callers also measured 164/96 bytes. The implementation and its experiment were reverted because they did not improve caller size. No final stripped artifact or retired-instruction comparison was measured, so this establishes neither total-program size nor runtime performance. General small-word inlining remains unfinished.
