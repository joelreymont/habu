---
title: Propagate content cache failures
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-07-19T20:48:14.544871+02:00\\\"\""
closed-at: "2026-07-20T23:16:36.745670+02:00"
close-reason: "Merged 412e2988: CK-CACHE-PERSIST rethrows the original code through cleanup (REMOVE-FILE-only swallow kept under its comment), CK-CACHE-DISABLED removed entirely (save is total: fully persists or throws), dirty state survives failure for retry, injected-fault seam proves all seven properties incl. mask-avoidance; run.f green on the gate-cache machinery"
---

lib/content-key.f:424-430 CK-CACHE-PERSIST catches every CK-CACHE-WRITE failure, removes the temp, prints "continuing without cache", sets CK-CACHE-DISABLED, and returns success. CK-CACHE-SAVE then clears CK-CACHE-DIRTY, so callers including CK-FINAL cannot distinguish a persisted content key from a failed write/rename. This is the repository-forbidden log-and-continue/error-masking pattern; it also makes build reproducibility and cache-health failures observable only in stderr text. Preserve the original throw code through cleanup and rethrow it from CK-CACHE-PERSIST/CK-CACHE-SAVE/CK-FINAL; cleanup may swallow only REMOVE-FILE failure under its existing explicit error-path comment. Remove the disabled-success state unless a typed result union is deliberately added and every caller handles the degraded result without calling it success. Add injected filesystem failures for unique-temp creation, write, close if exposed, and atomic rename; prove original error identity reaches the top-level command, dirty state is not falsely cleared, no partial final cache replaces the old file, temp cleanup is attempted once, cleanup failure does not mask the primary error, and a later retry succeeds. Update any gate that currently assumes cache I/O failure is nonfatal. Files: lib/content-key.f, lib/content-key-test.f, affected build callers/tests. Depends: none. Ownership: cache persistence error semantics only; no indexing, compaction, cache-key validity, or stale-bin behavior.

Claim: agent=cachefail workspace=.jj-ws/habu-propagate-content-cache-d7557d40
