---
title: Report load errors with source locations
status: open
priority: 2
issue-type: task
created-at: "2026-08-24T21:25:01.892214+02:00"
blocks:
  - habu-delete-the-old-679cfd35
---

Problem: bin/hb --load reports a rejected word and token but omits file, line, and column, while tools/check.f already renders those coordinates. Acceptance: the normal file and nested-require load paths report the existing source location before exiting nonzero; reuse the existing diagnostic origin and renderer, with no second parser, diagnostic IR, mode, or wrapper command. Files: the post-cut compiler/load diagnostic seam and focused CLI fixtures. Verify: two bad files through bin/hb --load assert path, line, column, token, and exit 70. Depends: habu-delete-the-old-679cfd35. Ownership: normal-load diagnostic rendering only.
