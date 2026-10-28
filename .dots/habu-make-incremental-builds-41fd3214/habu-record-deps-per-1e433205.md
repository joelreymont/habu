---
title: Record dependencies per file frame
status: closed
priority: 1
issue-type: task
created-at: "2026-10-22T17:12:02.897588+03:00"
closed-at: "2026-10-28T17:13:32.810058+02:00"
close-reason: landed d41898bd
---

Problem: dependency rows are recorded per definition (tools/native-unit-dependency.f BEGIN-DEFINITION), 1.68M rows per engine build, so a file has no record of what it consumed. Acceptance: NDEF-CACHE:FILE-UNIT-WITH (tools/native-definition-cache.f:745) opens a file frame; each consumed fact of an earlier file is stored once per (kind,key); the file's own records are not rows; FILE-EXPORT answers false on an ORDERED row; HABU_NATIVE_CACHE_DIAG=1 prints one native-file: frame line per file; per-definition reuse unchanged until the container dot lands. Verify: new test/native-file-frame-e2e.f, native-file-replay/auto/types e2e. Design: docs/architecture.md "Builds"; working notes ~/.cache/tmp/carl-file/design.md. Depends: vpmkpskp landed. Claim: agent=carl workspace=.jj-ws.noindex/carl-file2.
