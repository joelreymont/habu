---
title: Migrate suite manifests to using TEST bare pairs
status: open
priority: 2
issue-type: task
created-at: "2026-07-20T13:09:44.948428+02:00"
---

After the USING capability lands: convert the pure-DSL suite manifests to 'using TEST' with bare SUITE ... ;SUITE / GROUP ... ;GROUP / SUITE-STDIN (maki/test.f, maki/test-core.f and sibling slice loaders, maki/test-db.f, maki/test-eval.f, maki/test-eval-emit.f, test/gate-stdlib-cases.f, and the lib/test/suite-test.f fixtures where they read as manifests), roughly 500 mechanical lines. Update tools/suite-coverage-lint-core.f wherever it string-matches the TEST:SUITE spelling so both the qualified and the using-scope bare spelling are recognized (the lint walks manifests textually), and its tests. Mixed-concern files keep qualified spellings. Then apply the same conversion to the ONNX encoder DSL call sites (ONNX:;ENC-SUB family, 136 sites) - coordinate with any active device-side model-import lanes before touching ONNX files; if one is active, split ONNX into its own slice and land the TEST half. Gates: full suite-coverage lint, gate-stdlib all phases, maki/test.f, and the usual lints.
