---
title: Drop the redundant Unicode output lock
status: open
priority: 2
issue-type: task
created-at: "2026-10-16T11:17:41.961034+03:00"
---

class-generate hashes generated class-data; class-verify checks it then independently regenerates and compares full output. Remove output lock generation/check/file and related docs. Preserve pinned external UCD input digests, independent semantic scalar validation and regeneration equality. Run existing Unicode generator/verifier path.
