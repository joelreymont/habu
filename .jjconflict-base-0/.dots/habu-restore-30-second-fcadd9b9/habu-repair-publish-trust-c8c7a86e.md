---
title: Repair publish trust effects
status: closed
priority: 1
issue-type: task
created-at: "2026-07-17T14:54:50.289053+02:00"
closed-at: "2026-08-02T15:17:47.156430+02:00"
close-reason: "Superseded by the a8c716c5 hard cut: TRUSTED.md and trust-lint were deleted, so the obsolete publish manifest rows no longer exist."
---

Full gate on sol-change-file-v2 fails trust-lint: src/habu/habu2.f EM-COMPILE-PUBLISH-TRUSTED code effect is label -- but TRUSTED.md records --; EM-COMPILE-PUBLISH-HOOKED code effect is label label -- but manifest records --. Update the exact manifest rows to the implemented checked effects, run trust-lint and full gate. Dependency: already-landed publish-tail sharing commits.

REOPENED 2026-08-04 (dot-purge): this dot carried `status: active` with no live owner - no `agent=`/workspace claim, or a claim explicitly released. An active dot with no owner is invisible to `dot ready` and holds its id hostage, so the status is now `open` and the dot is free to claim.
