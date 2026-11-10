---
title: file-objects.f does not check that L2 and P2 link their files
status: closed
priority: 2
issue-type: task
created-at: "2026-11-10T12:52:22.279202+02:00"
closed-at: "2026-11-10T13:43:03.591851+02:00"
close-reason: "landed: KEEP holds log-first.f's and path-main.f's object size and change time after L and P, and LINKED's labels under L2 and P2 hold when they are unchanged (a compile replaces the object file, a link leaves it); the fixture builds use their own out root, new in each run; PATH-CASE is its own word; with the kept object removed only that pair's label fails; test/file-objects.f 7.7 s to 17.8 s in the suite (build L compiles everything in the empty root); two reviews, second ACCEPT with comment fixes; suite 270 of 270"
---

Problem (review of c1c72c21, 2026-11-10): test/file-objects.f LOG-CASE :524-533 shows a carried cell only while L2 and P2 link log-first.f and path-main.f from their objects; nothing asserts that they link, so a linker change that compiles them again would make both labels pass with or without the fix (the order trace showed such passing runs). Also LOG-CASE runs the path case under the log case's name, so a failed L build skips P; and test/fixtures/file-objects/bare-main.f:1-2 names only log-main.f though it is also built between P and P2. Acceptance: a label per pair fails when the program's file compiles again in the second build; the path case is its own word; bare-main.f's comment is true. Files: test/file-objects.f, test/fixtures/file-objects/bare-main.f. Verify: with the M build removed the L2 link label fails (trace), restored test: ok, wall time before and after. Depends: c1c72c21 stacked under it. Ownership: LOG-CASE, CHECKS, the new words. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-links.
