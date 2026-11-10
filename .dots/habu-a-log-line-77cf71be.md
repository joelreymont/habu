---
title: A log line cut by a failed write keeps its line
status: active
priority: 3
issue-type: task
created-at: "2026-11-10T10:14:07.783546+02:00"
blocks:
  - habu-a-log-line-839239b8
---

Problem: src/lib/log.f:7-8 says a line stays one line. When a write moves part of a line and then fails, the line is left without its LF and the next line joins it (log.f:156-160 OUT): case e6.f makes fd 2 a non-blocking pipe nobody drains and logs 256 KiB; 65536 bytes go through, EAGAIN becomes E-FS-IO, OUT swallows it, and the next line's stamp follows on the same line. A full disk partway through a line does the same. FD:WRITE-FULL throws E-FS-IO without saying whether any bytes moved, so OUT cannot tell a refused line from a cut one. Not rerun by the lead. Source: the code review of package LOG Joel asked for on 2026-11-10 ("check that LOG:CODE+ is implemented correctly, and the whole LOG package"), report ~/Work/reviews/habu-10x-2026-11-10/log-review.md, cases ~/Work/reviews/habu-10x-2026-11-10/log-review/, run as `cd <installed Habu tree> && bin/hb --load <case>`. Lines are as of 92c8b01b. Acceptance: after a write that moved bytes and then failed, LOG ends the cut line with a lone LF, so the next line starts on its own line; a test shows it. FD says whether bytes moved: that is a public change to FD, named and shaped in the design and reported to Joel as a fact. Files: src/lib/fd.f, src/lib/log.f, their tests. Verify: src/lib/log-test.f and FD's test. Depends: habu-a-log-line-839239b8 (both change OUT in src/lib/log.f); Joel, 2026-11-10: "dot and have agents fix". Ownership: src/lib/log.f, src/lib/fd.f. Claim: agent=bob workspace=.jj-ws.noindex/habu-a-log-line-77cf71be

Design: FD gains one public word, `TRY-WRITE-FULL ( fd ptr u8 n -- n n )`, which writes as WRITE-FULL does and answers the bytes moved and 0 or E-FS-IO in place of throwing; WRITE-FULL is defined over it. LOG keeps one process-wide state under LOCK, whether fd 2's last line is whole or cut: a write that moved bytes and then failed leaves it cut; a lone LF is written at once and, while still cut, before each later line; the LF moving makes it whole; a line whose LF is refused is lost. The pipe in case e6.f is full when the line is cut, so the LF can only go before the next line.
