---
title: "Find whether a build-held write can amend one of hb's own records"
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T11:07:49.301695+02:00"
---

Problem: src/core/checker.f HOLD-AMENDED (checker.f:12166, called at :15455 and :19726) holds an amended record's index and symbol for the build to write. The review of 'Bound hb's own facts texts by hb's data' did not examine whether that index can be one of hb's own records (below BUILD-FIRST), which would put an offset from IMAGE-BASE, an address in the build's space, into a record hb itself reads. The question predates that change. Acceptance: answered with path:line either way: the refusal or bound that keeps a build-held write off hb's records is named, or a program built with tools/hb-build.f shows the write and a fix dot is filed. Files: src/core/checker.f, src/habu/linker.f (read). Verify: the cited lines, or the reproducer's output. Depends: none. Ownership: none; a read. Claim: unassigned.
