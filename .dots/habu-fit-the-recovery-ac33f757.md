---
title: "Fit the recovery chain's source prefix or strip comments from it"
status: active
priority: 2
issue-type: task
created-at: "\"2026-09-16T19:16:43.950876+03:00\""
---

Problem: the no-binary stage0 chain (HABU_ALLOW_BOOTSTRAP=1 HABU_BOOTSTRAP_CHECK_ONLY=1 tools/bootstrap.sh) dies rc 74 `hb: source prefix buffer full` at the hb-stdin-mk step on 43f1ced4 and later heads (jit-path lane, 2026-09-16), while it reached 'bootstrap check OK' on the b04524c4-era sources at 15:06 the same day: the concatenated engine-source prefix crossed IBUFSZ (4 MiB) as today's landings added comment blocks (the audit alone measured 47 percent of checker.f bytes as comments). The recovery chain is a release criterion, so this is a red line. Acceptance: either the prefix emitter strips comments and blank lines from the source it concatenates (the checker and compiler never read them, and docs/tracker-rebuild's comment-wall dot is separate), reported as prefix bytes before and after, or IBUFSZ becomes a sized named constant with the measured prefix and headroom and a lint that refuses a prefix above 80 percent of it; the stage0 chain reaches bootstrap check OK on the head; tools/bootstrap.sh, bootstrap/cg/forth.fs and src/habu/habu2.f (the prefix source emitter) agree on the budget; the two-generation fixpoint unchanged. Files: src/habu/habu2.f (prefix emission, IBUFSZ), bootstrap/cg/forth.fs, tools/bootstrap.sh, tools/build-fixpoint.f, test/. Verify: tools/bootstrap.sh; tools/native-build.f fixpoint; test/run.f. Depends: none. Ownership: prefix emitter. Claim: agent=hazel-prefix-fit workspace=.jj-ws/hazel-prefix-fit. Priority: high (recovery chain red).
