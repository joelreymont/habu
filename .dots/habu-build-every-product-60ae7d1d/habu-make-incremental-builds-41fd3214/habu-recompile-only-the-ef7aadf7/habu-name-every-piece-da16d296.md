---
title: "Name every piece loaded inside a file's load"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-10T12:52:24.892580+02:00"
closed-at: "2026-11-10T23:38:05.156872+02:00"
close-reason: its commit of the same subject lands with the word-level stack; suite 273 of 273 and the measure green on the landing tip
---

Problem: a requester names only the requested file's last piece (src/habu/linker.f NEST), so one that inlined a constant from an earlier piece links stale when another build rewrote that object and the last piece kept its time (design-word-level.md 3.5). Acceptance: file-objects case N: r.f's program prints 2 after another build rewrote x.f's object; it prints 1 before the fix. Files: src/habu/linker.f, test/file-objects.f. Verify: file-objects PASS with case N; ev.sh on the lane workspace. Depends: none. Ownership: those files. Brief: carl-layout/.out/briefs/lane-nested-rows.md. Claim: agent=carl workspace=.jj-ws.noindex/carl-nest

2026-11-10: merged into the stack as tzoxpmtw (carl-nest retired). Before the fix file-objects failed only "N3: the program prints K" (got 1, wanted 2); after, PASS. ev.sh: cold 12.9 s, warm 1.6 s, probe 21, cold = warm = restored unsigned. The rows are added in NEST as the request returns, not at WRITE-OBJECT: no framed variable.
