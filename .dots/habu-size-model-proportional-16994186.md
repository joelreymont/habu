---
title: Size model-proportional tables from the model, not constants
status: open
priority: 1
issue-type: task
created-at: "2026-07-20T16:01:34.666729+02:00"
---


Joel-directed 2026-07-20, design settled after two rounds of his review (first correction: not runtime - load time; second: is declaration-first really best? No). FINAL HIERARCHY:
(1) library constants (today) - worst: recompile the framework to scale;
(2) user pre-declaration - second-best: leaks internal units (the user must know blocks-to-IR-nodes expansion, a formula that changes whenever adjoint derivation changes) and adds load-order brittleness;
(3) DERIVE FROM THE MODEL AT ITS LOAD - the design this dot now specifies.

PRIMARY DESIGN - derive-from-model, all at load time:
1. The capture pass already walks every op when a MODEL: body loads - it COUNTS nodes; the backward builder knows the adjoint expansion; the executor arena need is derivable from shapes. No user declaration.
2. At model BUILD (still load time) the tables are allotted at exactly the counted sizes - plain linear allot in the DATA region, no heap. Capture's own buffers are bounded by the loading file's size, knowable up front.
3. Failure boundary: one place - an absurd model dies NAMED at build against a generous sanity ceiling; nothing partially constructed (transactional, the MIR-LN-CK bar).
4. Multi-model images (the test suite builds 169 suites' models sequentially): mark-and-release arena discipline around each model build (the classic HERE-rewind idiom, scoped to a dedicated model arena so unrelated allotments never interleave), or measured grow-to-largest reuse - decide with evidence, leak bounded either way.
5. THE MEASURED COST, stated up front: column accessor words today compile the table base as an immediate address (LAYOUT-BUFFER defines at library load). Deriving at build means base-bound-at-build - accessors read one cell of indirection. This MUST be measured (executor + checker throughput on the landed gptblock-attn suite, before/after); if measurable, the JIT specializes the base once bound. Do not assume it away; do not accept an unmeasured regression.
6. The declaration word survives ONLY as an optional sanity-ceiling override, never required.
7. Work items: span/base-variable variant of LAYOUT-BUFFER definition with the pointer-rebind audit (everything caching column bases re-derives per build); count-then-allot wiring in capture/build; the arena mark/release; red-first too-big-model die; a regression proving two models of different sizes get exactly-sized tables in one image; snapshot/replay audit (nothing persists raw column addresses).

INTERIM unchanged: the coordinated constant raise (habu-coordinated-capacity-raise-0b4e8a84, in flight) lands first as the labeled interim; this dot is the recorded correct long-term fix. Sequence after that landing.
