---
title: "Save where a requester's source resumes"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-10T15:39:05.638873+02:00"
closed-at: "2026-11-10T23:38:05.220820+02:00"
close-reason: its commit of the same subject lands with the word-level stack; suite 273 of 273 and the measure green on the landing tip
---

Problem: a REQUESTS row (src/habu/linker.f NEST) holds no place in the requester's source and no fact about the request being a clean cut, so a file cannot compile from a request on (design .out/design-requester-by-words.md 4.4; the cursor field is cut: the event's token end is the place). Acceptance: the loader's event holds the stack depth (EVENT-DEPTH@); NEST writes the resume offset into the request row, -1 and UNLINKABLE unless top scope, depth 0, no open definition, token inside the source and a loader word dispatched; every engine request stays linkable. Files: src/core/include.f, src/habu/linker.f. Verify: ev.sh, warm rewritten 0. Depends: none. Ownership: carl. Brief: .out/briefs/lane-request-resume-place.md. Claim: merged into the stack as oztxvroz eea216fd (workspace retired), lands with the feature. Report: carl-layout/.out/reports/lane-request-resume-place.md.
