---
title: Process-unique temp names for atomic writers
status: open
priority: 3
issue-type: task
created-at: "2026-07-20T22:51:44.361860+02:00"
---

lib/content-key.f's atomic cache writer (and lib/fs-mutate.f MAKE-TEMP-DIR, same strategy) derive unique temp names from mono-ns alone, so two processes writing the same target can collide and one rename fails ENOENT - previously masked by the cache's log-and-continue, now correctly fatal after habu-propagate-content-cache-d7557d40 landed. Harden repo-wide: include the pid (getpid is a checked primitive now) in the temp-name derivation for every mono-ns temp site, add a two-process collision test (fork two writers at the same target via PROC-FORK), and sweep for other mono-ns-only temp derivations. Flagged by the cachefail lane 2026-07-20; never manifested under heavy concurrency but is a real race window.

LONG-TERM CORRECTION 2026-07-21: a PID plus timestamp lowers collision probability but is not the ownership invariant. Consume the unique sibling temporary-file capability from habu-fs-make-atomic-61537711: exclusive creation, no-follow, validated same-directory ownership, exact inode rename, scoped cleanup, and retry on the exact already-exists class. Keep a readable nonce in the name only as an implementation detail. Tests must include adversarial precreation and symlinks, concurrent writers, process/PID reuse, and exact primary-versus-cleanup errors. Do not merge a naming-only change.
