---
title: lints that run at require time and other lint hygiene
status: open
priority: 3
issue-type: task
created-at: "2026-08-22T22:47:07.196723+02:00"
---

Problem: tools/lint/shadow-lint.f:200, clobber-lint.f:717, ptx-emitter-lint.f:207, aot-section-reach-lint.f:199, stdin-closure-lint.f:122-123, repl-lint-core.f:302 run the scan at require, so clobber-lint-test.f:4 runs the whole production scan before any fixture and cannot test the lint on a tree where it would red; tools/bootstrap-mirror-lint.f:256-265 drift guard is a substring state machine over bootstrap.sh and :352-372 reads only the SRC_COMMON array (a 'cat lib/x.f' line added outside it escapes; TEST-FILE? skips any path containing 'test'); tools/process-primitive-lint-core.f:71-82 allowlists one exact line of test/engine-suite.f by full text; tools/build-fixpoint.f:648-649,732-764 preflight is CONTAINS? over icode.f; tools/namespace-lint-core.f is itself unpackaged; tools/bootstrap-codegen-test.f:40-42 justifies its arena by a file it no longer reads and :180-184 cites deleted assertions; AGENTS.md's 'HB_TMP=<root>' prefix for dot-dep-lint is read by nothing (0 matches). Acceptance: library/entry split for the six; the drift guard lexes the script and checks every cat line; a pragma replaces the exact-line allowlist; the preflight lexes or is deleted; namespace-lint packaged; the stale comments and the HB_TMP instruction fixed. Files: as listed, AGENTS.md. Verify: each lint's test. Depends: none. Ownership: lints. Claim: unassigned.
