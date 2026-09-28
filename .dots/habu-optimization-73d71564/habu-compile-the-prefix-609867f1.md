---
title: Compile the prefix to dense code
status: closed
priority: 2
issue-type: task
created-at: "\"2026-09-15T18:52:59.716456+03:00\""
closed-at: "2026-09-28T11:00:35.183743+02:00"
close-reason: "Retired obsolete premise: the production native build already recompiles the complete target prefix at tier 1 and rejects non-native capture provenance. The tier-0 prefix census described the separate source-booted recovery chain. Documentation corrected from pinned source evidence; no new tier-switch implementation or historical before/after size or per-word timing claim. Further density work remains in measured code-generation tasks."
---

## Current production route

The native production build already compiles the full target prefix at tier 1.
`tools/native-build-core.f` retains the callable host compiler, resets dictionary
visibility, opens the capture window and reloads the complete target runtime.
`src/habu/native-runtime.f` installs the replacement compiler after it is loaded;
`src/habu/aot-owned.f` rejects a production capture unless the entire original
window has native provenance. No compiler change is justified by the original
tier-0 premise. The `2dup` example below is a directly emitted engine primitive.

The 3,566 tier-0 boot words in `docs/compiler-measurements.md` belong to the
separate source-booted recovery chain. The documentation now distinguishes
that sample from production. Retire the obsolete tier-switch implementation
request. A matching tier-0 production baseline and the original per-word
before/after comparison have not been established; no saving is claimed from
this correction. Further native code-density work belongs to the existing
measured code-generation tasks, not a switch the production build already makes.
Read-only source evidence is recorded in
`~/.cache/tmp/habu-prefix-density-design-20260928-01.md` at Habu `6c64049ce625`.

## Original task

Problem: the baked prefix is tier-0 code: 2dup is 13 instructions, STR= (a six-line loop) 41, about 140 B per word over 15,683 words, because the optimizing tier costs about 60 ms per word (213 s for the core prefix) and is not run over the prefix. Acceptance: measured decision between (a) a tier-1 fast enough to compile the whole prefix at build (profile the chain with prof-on on a window build, fix the hot phases) and (b) a register-cached tier 0 (TOS in a register, pair folding) for the prefix; then the prefix compiled by the chosen tier, code bytes per word and the tier-1 build time reported before/after; test/run.f green. Files: src/habu/habu2.f (tier-0 emitters), src/compiler/native/*.f, tools/native-build.f. Verify: census of code bytes per word on the shipped engine; time of native-build.f. Depends: guard removal dot habu-replace-per-transfer-8523fb98. Ownership: compiler. Claim: unassigned.
