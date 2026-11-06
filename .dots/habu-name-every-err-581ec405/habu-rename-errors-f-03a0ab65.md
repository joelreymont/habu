---
title: Rename errors.f to error.f
status: open
priority: 2
issue-type: task
created-at: "2026-11-06T15:05:41.490236+02:00"
---

Problem: Joel 2026-11-06 put package ERROR in src/lib/error.f, errors.f renamed. Acceptance: src/lib/errors.f becomes src/lib/error.f with no content change; every path spelling src/lib/errors.f follows (444 requires, s" ..." required forms, src/habu/boot-files.f:115, tools/bootstrap.sh, tools/seed.f, test argv lists and fixtures, README.md, docs/*.md), and the 8 bare 'errors.f' that name this file (rg -P '(?<![-/\w])errors\.f'); tools/check-all-errors.f, order-errors.f and other files ending in errors.f are untouched; DECISIONS.md untouched. Files: every match of rg -l 'src/lib/errors\.f' outside DECISIONS.md. Verify: a candidate hb built from the tree (errors.f is a boot row) starts and runs test/file-objects.f-sized focused tests; the full suite runs once at the landing of this and the ERROR package together. Depends: none. Landing: tell kevin, phil and tim (Tender, Zen, Maki require src/lib/errors.f; their requires break at the next install). Ownership: carl, worker lane. Claim: unassigned.
