---
title: Finish the source-owned native runtime build
status: closed
priority: 1
issue-type: task
created-at: "\\\"2026-09-10T18:03:13.327440+03:00\\\""
closed-at: "2026-09-16T14:34:48.772389+03:00"
close-reason: "superseded by habu-campaign-c1-finish-1f129a00: The source-owned native runtime build still cannot complete an optimizing self-build from current source"
---

Owner: app_image; checker.f and concretely failing prehook type declarations in cedar-warm-bootstrap; coordinate tools/native-build.f and native-runtime.f with Cedar. Finish honest prefix contracts and transfer checked declarations into the new target checker with their actual type, raw, control and defer semantics. Do not substitute PRIM assertions for ordinary helper declarations. Cold native build, prefix-declaration regression and standalone capture pass; checked warm self-build remains unfinished. Acceptance: tools/native-build.f rebuilds bin/hb from the current native binary, the rebuilt image checks accepted and rejected programs and rebuilds again, and a fresh checkout has one documented working recovery path. Remove superseded bootstrap orchestration only as that path replaces it.

Current handoff (2026-09-11): owner Cedar integration cedar-crossing-realpath; app_image is no longer an active agent. Cold generation works around20s but optimizing current-source selfbuild remains unfinished. Earlier BI/-8519 is repaired by RQ metadataa83fac94; generic KEEP ABI candidate931a543c and separate spill frame-order -8522 must finish before next optimizing selfbuild.

b0b90daa fixes actual full prefix static certification: remove nonexistent pointer-storage-effects.f include, avoid CHECKER-CALLS:CELLS shadowing by explicit CELL scaling and CELL-VIEW. Exact emitted prefix now certifies rc0 in0.904s, independently0.848s. This is only certification, not completed selfbuild. Probe /tmp/cedar-prefix-probe.f calls BF-CERTIFY-RC outside any temporary package; invoking inside BUILD-FIXPOINT gives irrelevant CORE-LAYOUT-RC package-context failure.

Use current tiered bin/hb with1 set-tier before all native-build input. Actual old /tmp/cedar-crossing-realpath/hb-stdin is196KB cold legacy seed, has neither set-tier nor NCOMP:COMPILE and falls back to JIT. It cannot prove 100% optimized output. Routing tracked habu-select-optimizing-compilation-cf2b21d4. Separate cedar-warm-bootstrap20-file WIP (checker private owner/57PRIM retirement/VERIFY scope etc.) is NOT integrated; blockers include sealed CHECKER-REG reopening, removedHOOK/INSTALL consumers and SOURCE-BUF-AT-IN-SCOPE origin semantics. Do not import wholesale.

Fresh no-binary Gforth recovery workspace cedar-shaped-pair21444b34 is unfinished: assembler now works but generated Linux startup exits79, suspected4KiB/64KiB segment alignment. Resume only at responsible ELF layer with docs/debugging.md; not final recovery proof. Acceptance remains real native selfbuild twice, accepted/rejected checked programs and one working documented recovery path.

Confirmed first-generation compiler tier: on root SHA1b965ddf, baked IR-ID:COUNT-N has recorded length16 and first instruction0xd10043ff (JIT frame); freshly JIT-compiled identity matches16/0xd10043ff, tier1 identity is length0/0xd65f03c0 (RET). Native-runtime loads compiler.f before NCOMP:COMPILE exists in the cold seed. A fully optimizing selfbuild changes the performance baseline of the compiler implementation itself. Root owns that build; Rowan stopped duplicate tier-lane selfbuild work. No predicted speedup claimed before measurement.
