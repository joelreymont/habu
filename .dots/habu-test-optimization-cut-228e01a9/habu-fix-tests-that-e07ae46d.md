---
title: Fix tests that cannot fail or leak state
status: open
priority: 1
issue-type: task
created-at: "2026-09-29T18:01:21.059716+02:00"
---

Problem: pre-trust-defer.f:354 '76 73 T<>' is a tautology; gate-debug-lib.f BAND constant relation is a tautology; record-launder-probe L1-L4 assert that a known hole certifies (also pinned in typed-storage-structural V8/V9); gate-diagnostics pins byte-exact goldens beside schema checks and the rc-75 local-in-quotation pin is duplicated in three suites; tools/aot-call-report-test.f:128,141,148 write fixed /tmp paths shared across gates; tools/image-bytes-test.f cannot load alone (needs asm-src-test.f first). Evidence: ~/.cache/tmp/kestrel-gate/test-review/L6-test-g-z.md, ~/.cache/tmp/kestrel-gate/test-review/L8-tools.md finding 4, ~/.cache/tmp/kestrel-gate/test-review/L7-lib.md. Acceptance: each check asserts behavior that can fail (mutation shown) or is removed with its covering witness named; paths go under HB_TMP; image-bytes-test loads alone. Files/Ownership: those files only. Base: 614ae0ba (row-split stack head, not yet on master). Verify: every touched row passes standalone (bin/hb --load <row file>); a mutation of one moved or rewritten assertion fails; report per-row seconds before and after. Depends: none. Claim: unassigned.
