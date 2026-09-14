---
title: Carry the checker-owner guard in the cold compiler closure
status: active
priority: 2
issue-type: task
created-at: "\"2026-09-14T03:34:37.746232+03:00\""
---

The real 7,766-record cold compiler capture refused FIELD at blob offset 56,720: its callee CHECKER-OWNER-GUARD:VALIDATE was required by aot-arm before WINDOW-OPEN, in the excluded tooling prelude. The cold native startup now loads and provides the actual guard after its ABI include facts and before lower-cert-seal. The full native runtime and existing prefix source assembly use the same guard-before-mark order. Guard checks, privacy and the restricted bootstrap seed are unchanged.

Acceptance uses M (99caf411, SHA256 9522a8e5685129b17b107bd547dc0797a1f11e0bb3f89f8282b2b8206770b58c) to run the source-built optimizing NATIVE-EMIT:WRITE adapter against this workspace. `/tmp/cedar-owner-closure-cold` is a genuine empty native image, SHA256 e536e261f5dc7459e62faeb5f0cf563f987086c32809222315fd94cda170023b. Its `--load test/prefix-mark-test.f` and `--load test/checker-owner-descriptor.f` pass; the latter retains the expected ABI-only E-CAP-TRUSTED refusal. The registered prefix assertion fails on unchanged M at F13 because its guard is above the core mark. The focused existing BF source-assembly checks also pass, including the actual VALIDATE body before PREFIX-MARK. Logs: /tmp/cedar-M-owner-closure-writer.{out,err}, /tmp/cedar-cold-owner-{prefix,descriptor}.{out,err}, /tmp/cedar-M-owner-prefix-{red,assembly}.{out,err}.

`/tmp/cedar-owner-closure-cold --load tools/aot-chain-capture.f -- /tmp/cedar-owner-closure-chain.aot` gets past the FIELD/VALIDATE closure refusal, then correctly refuses a separate address target (rc74): row 297, DATA+7955160, target 24515408 outside capture code 24945144..25961228. Logs: /tmp/cedar-cold-owner-chain.{out,err}. That address-reference issue remains open under 4ccf56d9; no full chain pass or artifact is claimed here. Frozen for independent source review.
