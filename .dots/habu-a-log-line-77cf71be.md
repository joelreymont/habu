---
title: A log line cut by a failed write keeps its line
status: open
priority: 3
issue-type: task
created-at: "2026-11-10T10:14:07.783546+02:00"
blocks:
  - habu-a-log-line-839239b8
---

Problem: src/lib/log.f:7-8 says a line stays one line. When a write moves part of a line and then fails, the line is left without its LF and the next line joins it (log.f:156-160 OUT): case e6.f makes fd 2 a non-blocking pipe nobody drains and logs 256 KiB; 65536 bytes go through, EAGAIN becomes E-FS-IO, OUT swallows it, and the next line's stamp follows on the same line. A full disk partway through a line does the same. FD:WRITE-FULL throws E-FS-IO without saying whether any bytes moved, so OUT cannot tell a refused line from a cut one. Not rerun by the lead. Source: the code review of package LOG Joel asked for on 2026-11-10 ("check that LOG:CODE+ is implemented correctly, and the whole LOG package"), report ~/Work/reviews/habu-10x-2026-11-10/log-review.md, cases ~/Work/reviews/habu-10x-2026-11-10/log-review/, run as `cd <installed Habu tree> && bin/hb --load <case>`. Lines are as of 92c8b01b. Acceptance: after a write that moved bytes and then failed, LOG ends the cut line with a lone LF, so the next line starts on its own line; a test shows it. FD says whether bytes moved: that is a public change to FD, named and shaped in the design and reported to Joel as a fact. Files: src/lib/fd.f, src/lib/log.f, their tests. Verify: src/lib/log-test.f and FD's test. Depends: habu-a-log-line-839239b8 (both change OUT in src/lib/log.f); Joel, 2026-11-10: "dot and have agents fix". Ownership: src/lib/log.f, src/lib/fd.f. Claim: bob's chain; unassigned until its blocker lands.
