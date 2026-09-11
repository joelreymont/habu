---
title: Package repair schema tests
status: closed
priority: 1
issue-type: task
created-at: "2026-07-22T17:33:12.686200+02:00"
closed-at: "2026-07-23T02:05:41.084430+02:00"
close-reason: Landed and remotely verified at ad47ef725011; independent destruction review and exact owning/master gates green.
---

Files: tools/repair-schema-doc-test.f only. Put the harness in package REPAIR-SCHEMA-TEST, make state and helpers private with short tails, and invoke a private RUN inside the package while still calling global checker and all-errors APIs. Preserve documentation/source scans, diagnostic schema fields, and exact repair classes. Acceptance: no RSD-* definition or storage remains global; every existing case executes; no alias. Verify: bin/hb --load tools/repair-schema-doc-test.f, typed-local-diff-lint, package-diff mutation, host-lint, filemap-lint.

Claim: agent=repair_schema_test workspace=.jj-ws/habu-pkg-repair-schema-d20f1641.
