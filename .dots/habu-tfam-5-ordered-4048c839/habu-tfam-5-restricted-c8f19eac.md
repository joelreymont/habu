---
title: "TFAM 5: restricted discovery pass + fresh require registry"
status: closed
priority: 2
issue-type: task
created-at: "\"2026-07-04T08:53:45.124164+02:00\""
closed-at: "2026-07-04T09:58:18.895610+02:00"
---

Add REQUIRE-SNAPSHOT/REQUIRE-RESTORE in src/core/include.f saving/restoring REQUIRE-N + REQUIRE-PATHS slot bytes + REQUIRE-LENS (currently INCLUDE-SNAPSHOT-PREPARE :205 resets include state but NOT the require registry; warm snapshots must still PRESERVE it). Build a restricted discovery pass that: snapshots the registry, enables event recording, replays only source-composition + support-declaration forms of an entry file, emits the ordered event artifact, restores the registry. Tool-preloaded require paths must not hide a later user s" path" required/provided. Reject fail-closed when the ordered artifact cannot be produced, or when loader words are redefined/undefined/hidden before discovery completes (loader-word reservation in reserved-name-lint already lands the redefinition guard for user source). Depends on event-log store dot.
