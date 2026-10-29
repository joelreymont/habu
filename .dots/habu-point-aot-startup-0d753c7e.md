---
title: Point aot-startup-reach-lint at the startup emitter
status: open
priority: 2
issue-type: task
created-at: "2026-10-30T01:13:58.599194+02:00"
---

Problem: tools/aot-startup-reach-lint.f MAIN scans src/habu/aot-lib.f, now a 22-line target loader; the startup emitter lives in src/habu/aot-link-arm.f, so the lint checks none of it and its clean result means nothing. Acceptance: the lint scans the file that holds the emitter and reports a planted unreachable word. Files: tools/aot-startup-reach-lint.f. Verify: bin/hb --load tools/aot-startup-reach-lint.f on master, then with a planted defect. Depends: none. Ownership: tools/aot-startup-reach-lint.f. Claim: unassigned.
