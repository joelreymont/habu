---
title: Carry the source verifier in hb
status: open
priority: 2
issue-type: task
created-at: "2026-11-05T12:06:26.063331+02:00"
---

Step 5b (/tmp/jerry-big/xbuild-order.md; DECISIONS tools in hb: verifier 'Right after step 5 (Recommended)'). Acceptance: src/habu/verify-source.f and its closure (fs, source, source-lex, primitive-source, vector, source-syntax) become src/habu/native-runtime.f rows with ML-ENTRY+ reasons in tools/manifest-lint-core.f, with every other FS owner (fs-mutate.f, fs-identity.f, fs-anchored.f); tests that reopen FS test public words or run on the whitebox engine; check-verify-core.f takes its source child; tools/check-verify-image.f and check-verify-save.f go. tools/check-verify-child-lib.f runs code at load and stays a script. Files: native-runtime.f, manifest-lint-core.f, tools/check-verify*.f. Verify: manifest lint, check-verify tests, engine build. Depends: step 5. Ownership: those files. Claim: unassigned.
