---
title: Purge the stale dot tail
status: active
priority: 2
issue-type: task
created-at: "2026-08-04T20:09:01.729502+02:00"
---

Problem: user-directed aggressive grooming round 2 - the FILEMAP citation purge, orphaned actives, stale claim lines, and retired speculation still clog .dots after the conservative pass e72ec948. Acceptance: FILEMAP references gone from live dots, orphaned actives reopened, stale claims stripped, four-clause speculation closed, dot lint 0 findings. Files: .dots only. Verify: HB_TMP=/private/tmp/claude-501/purge-lint bin/hb --load tools/dot-dep-lint.f. Depends: none. Ownership: .dots metadata.

Claim: agent=dot-purge workspace=.jj-ws/habu-dot-purge
