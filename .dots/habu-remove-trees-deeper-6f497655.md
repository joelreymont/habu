---
title: Remove trees deeper than PATH-CAP
status: active
priority: 2
issue-type: task
created-at: "2026-10-27T10:25:17.163793+02:00"
---

Problem: REMOVE-TREE (src/lib/fs-mutate.f:218-264) names entries by joined path; FS-DESCEND-PATH (src/lib/fs.f:774) throws E-FS-CAPACITY past FS-PATH-CAP (1023), so a tree with longer paths is left behind (a probe of 7 nested 200-byte dirs leaked; test/deep-cwd-e2e.f removes its tail with a /bin/rm child for the same reason). Acceptance: removal below the root is descriptor-relative (fstatat/openat/unlinkat), symlinks unlinked never followed, failures throw, descriptors closed on every path; test/deep-cwd-e2e.f removes its deep tail with REMOVE-TREE and proves a symlinked outside sentinel survives; the rm child is gone. Files: src/lib/fs-mutate.f, test/deep-cwd-e2e.f. Verify: deep-cwd-e2e (fails before, passes after), gate-pool-test, native-file-storage-e2e, check-verify suite with src/ tools/ intact. Brief ~/.cache/tmp/carl-file/brief-rmtree.md. Depends: none. Ownership: carl. Claim: agent=carl worker workspace=.jj-ws.noindex/carl-rmtree.
