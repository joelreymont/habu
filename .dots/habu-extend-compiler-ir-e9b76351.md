---
title: Extend compiler IR error-code region
status: active
priority: 2
issue-type: task
created-at: "2026-07-28T21:50:41.487638+02:00"
---

Full context: the attribute table consumed -6691..-6699, so the shared compiler IR block -6600..-6699 (E-IR-FIRST/E-IR-LAST in lib/errors.f:292-293) is now exactly FULL, while the dialect-schema, operation/value pool, control-structure, freeze, verify, codec, and pass-result stages all still need codes. -6700..-6799 is already taken by the frozen compiler identity schema (COMPILER-ID-PROOF). BLOCKING for the next compiler IR lane: decide the next region (a second named block, or a relocation) and update E-IR-LAST plus the block comments before any further compiler IR work needs a code. Acceptance: tools/error-code-lint.f 0 findings with the new region reserved and documented in lib/errors.f, and a dispatchable lane can mint a code without colliding.

Claim: agent=err-region workspace=.jj-ws/habu-extend-compiler-ir-e9b76351
