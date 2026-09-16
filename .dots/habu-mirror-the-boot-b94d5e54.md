---
title: Mirror the boot-registry open token in the seed
status: active
priority: 2
issue-type: task
created-at: "\"2026-09-16T14:18:00.814306+03:00\""
---

Problem: src/core/include.f ENGINE-KNOWN? (97db883a) keeps a canonical scan over the boot rows only because bootstrap/cg/forth.fs emits the provide rows and EMIT-REQUIRE-FREEZE-TOKEN but no REQUIRE-BOOT-OPEN token, so a gforth-seeded engine records canonical (build-directory) boot rows while a native-built engine records portable ones. Two spellings for one fact, and the seeded engine still bakes its build directory. Acceptance: forth.fs emits the `REQUIRE-BOOT-OPEN` token at the position src/habu/habu2.f EMIT-REQUIRE-BOOT-OPEN-TOKEN uses (after the base files, before the first provide row, cold boots only, paired with the freeze token); the canonical boot-row scan in ENGINE-KNOWN? and the comment justifying it are deleted so every engine has one boot-row spelling; `strings -a` of a seeded engine shows no build directory in the require rows; ENGINE-PROVIDES? answers for src/core/enums.f and lib/errors.f are unchanged on both engine kinds. Files: bootstrap/cg/forth.fs, src/core/include.f. Verify: stage0 chain tools/bootstrap.sh reaches "bootstrap check OK"; tools/native-build.f from the seeded engine succeeds; test/require-cap-test.f, test/prefix-mark-test.f, test/source-root-test.f, lib/source-test.f green on both. Depends: none (portable rows landed at 97db883a). Ownership: bootstrap/cg/forth.fs, src/core/include.f ENGINE-KNOWN?. Claim: agent=hazel-audit-seed workspace=.jj-ws/hazel-audit-seed.
