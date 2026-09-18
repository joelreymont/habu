---
title: Preserve the source when stream-copy paths alias
status: open
priority: 1
issue-type: task
created-at: "2026-09-18T19:52:45.285581+03:00"
---

Problem: lib/fs-mutate.f:175-193 opens the destination with O_TRUNC before comparing its identity with the open source. COPY-FILE-STREAM from a file containing must-survive to a symlink to that file returned success and left the source at zero bytes on bin/hb. Identical paths and hard links share the same destructive path. Acceptance: reject or safely handle identical source/destination inodes before any truncation, using descriptor identity where needed to avoid a path-check race; regression tests cover identical paths, symlink and hard-link aliases, preserve source bytes on refusal, and retain ordinary streaming copies. Files: lib/fs-mutate.f and its focused tests, filesystem identity primitive/boundary only if needed. Verify: focused filesystem suites through bin/hb. Ownership: filesystem library. Claim: unassigned.
