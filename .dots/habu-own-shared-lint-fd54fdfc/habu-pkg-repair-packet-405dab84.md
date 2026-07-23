---
title: Package repair packet tests
status: closed
priority: 1
issue-type: task
created-at: "2026-07-22T17:33:12.570370+02:00"
closed-at: "2026-07-23T02:05:41.079075+02:00"
close-reason: Landed and remotely verified at ad47ef725011; independent destruction review and exact owning/master gates green.
---

Files: tools/repair-packet-test.f only. Put the harness in package REPAIR-PACKET-TEST, make state and helpers private with short tails, and invoke a private RUN inside the package while still calling global checker and all-errors APIs. Preserve packet fixtures, golden names, JSON fields, exit codes, and artifact cleanup. Acceptance: no RPT-* definition or storage remains global; all existing cases execute; no alias. Verify: bin/hb --load tools/repair-packet-test.f, typed-local-diff-lint, package-diff mutation, host-lint, filemap-lint.

Claim: agent=repair_packet_test workspace=.jj-ws/habu-pkg-repair-packet-405dab84.
