---
title: "Label every errno a source's open meets"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-16T03:01:48.096871+01:00"
closed-at: "2026-11-16T04:07:35.633593+01:00"
---

Problem: src/core/include.f OPEN-ERRNO$ labels every errno but 2, 13, 20, 21 and 5 'cannot open', with the help 'check that the file exists and is readable', wrong for EMFILE, ENFILE and ENOMEM. Acceptance: those get their own label and help; any other errno's label names it (os error N). Depends: none. Claim: agent=W165 workspace=.jj-ws.noindex/jerry-w165.
