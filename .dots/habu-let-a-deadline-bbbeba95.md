---
title: Let a deadline write end on a cancel descriptor
status: open
priority: 2
issue-type: task
created-at: "2026-11-12T16:04:54.208491+02:00"
---

Problem: FD:WRITE-FULL-UNTIL (src/lib/fd.f) waits for POLLOUT on one descriptor until its deadline and takes no cancel descriptor, so a writer to a stopped reader cannot be cancelled: Maki's kernel client writes requests of up to 8 MB to its worker with deadlines up to 300 s, and a job's Cancel then waits out the deadline and the job ends lost, not cancelled (request from Maki, tim, 2026-11-12; reproducer /tmp/tim-habu-write-cancel.md: a 200,000-byte write to an unread pipe with a 10 s deadline and a cancel pipe already holding a byte returns E-FS-TIMEOUT after 10 s). src/lib/process.f's capture loop has the pattern (ARM-CANCEL-PFD, CANCEL-HEARD). Acceptance: a write of n bytes with a deadline and a cancel descriptor (PROCESS:NO-FD for none, behaving as WRITE-FULL-UNTIL) throws E-PROC-CANCELLED once the cancel descriptor has a byte or a closed writer, restoring the descriptor's flags as WRITE-FULL-UNTIL does; the reproducer ends within a poll. Design first: Joel, 2026-11-11, asked for a wait on a second descriptor, chose the general PROCESS:POLL ( n ms -- count ) over a word that knows which descriptor means cancel ('don't you normally wait on at least 2 descriptors in this case, passing both the real one and the cancel one to the underlying OS poll mechanism?'), so the write's wait is designed the same way and its public words' names are his. Depends: none. Claim: unassigned.
