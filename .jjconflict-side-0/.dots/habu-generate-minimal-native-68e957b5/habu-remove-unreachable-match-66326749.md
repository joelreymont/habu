---
title: Remove unreachable MATCH tag path
status: open
priority: 2
issue-type: task
created-at: "2026-07-19T22:06:07.763963+02:00"
---

Measured native compiler bloat: src/habu/habu2.f:6347-6352 emits an alternate movz-x16 plus register compare whenever a MATCH tag exceeds the AArch64 cmp-immediate limit 4095. The declaration reader is capped at 4096 total source bytes by TDECL-CAP in src/core/sumtype.f:35, so no single payload-bearing ENUM declaration can contain 4097 distinct named variants; even the shortest legal variant spelling and separators require more than one byte each. The large-tag branch is unreachable for every constructible family but remains in the compiler and complicates the stencil. Prove the actual maximum variant count from the declaration grammar and cap with a boundary fixture, express the reachable tag bound as one named compiler invariant, delete the fallback emitter and its label/control flow, and emit only cmp #tag. Do not retain a compatibility branch for unconstructible metadata. Prove declarations at the exact byte/variant boundary, rejection above the declaration cap, every reachable MATCH tag, byte-identical emitted code for all constructible fixtures, disassembly with no MOVZ-X16 fallback, exact compiler CODELEN reduction, fresh fixpoint parity, and full type-family/native gates. Rebase onto landed factoring commit ed6207a00ef7 and preserve its shared stencils; the closed factoring dot was removed after landing and is history, not a live dependency.
