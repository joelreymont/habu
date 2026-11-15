---
title: Fix the clobber lint failure on master
status: active
priority: 2
issue-type: task
created-at: "2026-11-15T22:59:25.063676+01:00"
---

Problem: bin/hb --load tools/lint/clobber-lint.f exits 1 on master fe4613b3: three CLOBBER lines in src/habu/habu1.f (bcallback-entry x9 by lcallbackaddr; emit-find x17 and x13 by lheadrow) and 'routine census below floor'. Acceptance: the lint passes, each report fixed at its cause (code or lint model). Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/carl-clobber (fix).
