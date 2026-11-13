---
title: Carry the source verifier in hb
status: closed
priority: 2
issue-type: task
created-at: "2026-11-05T12:06:26.063331+02:00"
closed-at: "2026-11-09T15:49:24.421161+02:00"
close-reason: "landed in step 5 commit 2: hb carries src/habu/verify-source.f and its closure as a RUNTIME-FILES row of tools/hb-build-core.f (the linker-built hb; native-runtime.f goes with the old builders); check-verify-core.f takes its source child only; tools/check-verify-image.f, check-verify-save.f and the check-verify image family go; test/whole-source-read-test.f, which reopened FS to grow a file, tests SOURCE:READ-WHOLE-SAMPLED's short and long samples. Shown on gen 1 (/tmp/jerry-s5/g1, rebuilt equal; g2 == g3): heap, structure-opaque, diag-json-escape, diag-position, check-signal, whole-source-read and the 11 verifier rows pass; ffi-cabi F23 is commit 4's direct-child case."
---

Step 5b (/tmp/jerry-big/xbuild-order.md; Joel 2026-11-05: hb carries the verifier "Right after step 5 (Recommended)"). Acceptance: src/habu/verify-source.f and its closure (fs, source, source-lex, primitive-source, vector, source-syntax) become src/habu/native-runtime.f rows with ML-ENTRY+ reasons in tools/manifest-lint-core.f, with every other FS owner (fs-mutate.f, fs-identity.f, fs-anchored.f); tests that reopen FS test public words or run on the whitebox engine; check-verify-core.f takes its source child; tools/check-verify-image.f and check-verify-save.f go. tools/check-verify-child-lib.f runs code at load and stays a script. Files: native-runtime.f, manifest-lint-core.f, tools/check-verify*.f. Verify: manifest lint, check-verify tests, engine build. Depends: step 5. Ownership: those files. Claim: unassigned.
