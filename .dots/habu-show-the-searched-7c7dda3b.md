---
title: Show the searched path a missing source was looked up at
status: open
priority: 2
issue-type: task
created-at: "2026-11-16T05:29:17.654917+01:00"
---

Problem: src/core/include.f MISSING-SOURCE's 'searched' note prints each root joined with the request and normalized lexically (UNDER, :1266), while the search (CANDIDATE -> JOIN -> CANONICAL, realpath) resolves the joined path on disk: for 'require lnk/../x.f' with lnk a symlink to another directory, the note names a path the search never looked at. Acceptance: the note names the path the search looked up (the joined spelling, or the physical prefix plus the absent suffix as CANONICAL builds it), from the load and check.f alike, and a test with a symlink before '..' pins it. Depends: c550f89a (it relabels the denied record's note). Claim: unassigned.
