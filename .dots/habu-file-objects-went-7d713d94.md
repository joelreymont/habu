---
title: file-objects went red once inside a full suite run
status: active
priority: 1
issue-type: task
created-at: "2026-11-10T15:33:18.425500+02:00"
---

Problem: the landing run of the deferral-cause commit (0cb71d70 on master 5aa6f49a) ended 271 of 271 with 'RED: file-objects kind=exit code=67'; alone, twice, the test passes on the same engine and tree; the run before it on master's source passed. Other workspaces were running builds and the same test at the time. The suite removed the red test's logs. Suite log /Users/joel/Work/reviews/habu-10x-2026-11-10/cases8/fo67/land9-suite.log. Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: a hunt names exit 67 and the cause as path:line, or what remains unknown after its runs; then the cause is fixed and a test catches it. Files: test/file-objects.f, test/run.f, the build's shared paths. Verify: the hunt's reproduction. Depends: none. Ownership: the file linker's tests. Claim: agent=bob workspace=.jj-ws.noindex/bob-fo67
