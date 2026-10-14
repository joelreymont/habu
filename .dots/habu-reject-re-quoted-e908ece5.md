---
title: Reject re-quoted dot frontmatter
status: open
priority: 3
issue-type: task
created-at: "2026-07-26T09:44:10.432939+02:00"
---

The installed external dot CLI re-quotes raw frontmatter strings when it
rewrites a file. An already quoted created-at timestamp gains literal quote
characters on dot on/off/claim edits, and repeated rewrites compound them.
This remains reproducible during Habu backlog closure.

Fix the serializer at the Dots writer boundary, preserving parsed scalar
values across rewrites. Otto owns that upstream fix; carl supplied the exact
CLI reproduction. The Dots regression should exercise the real CLI with
quoted timestamps and legitimate escaped quotes inside a title.

After the fixed public tool is available, normalize the affected Habu metadata
once, preserving the timestamp/string values and unrelated content. Verify
ordinary claim/closure rewrites no longer change those values. Do not add a
Habu gate or permanent repair program to compensate for another package's
serializer. Keep this dot until the upstream tool and one-time cleanup are
complete; Habu code changes are not required.
