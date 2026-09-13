---
title: Compare complete inherited environment values in tests
status: closed
priority: 2
issue-type: task
created-at: "\"\\\"2026-09-13T16:24:10.275135+03:00\\\"\""
closed-at: "2026-09-13T16:33:24.014073+03:00"
close-reason: Integrated1d62ea29+317b7dae after separate Astra review and review follow-up requiring whole-line boundaries. Existing1024-byte SB assumption removed using scoped exact-size memory. Independent actualPATH1047 real-load suite passes26 cases including first/middle matches, prefixed-name/embedded-value/changed-value refusals. No environment value or config changed; full compiler gate remains separately open.
---

Problem: lib/process-env-test.f PET-EXPECT-INHERITED constructs a complete NAME=value line through the 1024-byte SB buffer. The current PATH is 1047 bytes; both original integration host28e11361 and fresh product0c602d1c throw E-STR-CAPACITY after env-child/empty-env-child. Production inheritance has not failed this comparison. Acceptance: compare complete inherited names and values with an explicit line boundary, without truncation or a small concatenation buffer; normal and longer-than1024-byte values pass and mismatched values fail. Files: lib/process-env-test.f and existing checked string/memory helpers only if necessary. Verify: real load of process-env-test.f under the current environment, plus focused long-value assertion. No environment values in logs, no system/config changes. Depends: none. Ownership: Sol fixture lane, separate workspace. Claim: assigned; root owns independent review and integration.
