---
title: Move the poll(2) waits onto the AIO loop
status: open
priority: 2
issue-type: task
created-at: "2026-09-21T18:07:21.888347+03:00"
---

Problem: lib/net/tcp4.f (POLL-UNTIL, one pollfd per task), lib/net/udp4.f, lib/serial.f, lib/pty.f, lib/process-pty-io.f (PROC-POLL-RESTART) and lib/signal.f each park the calling thread in poll(2). Design: each readiness wait becomes AIO:POLL-ADD with the same timeout and AIO:AWAIT, the outcome mapped exactly as the pollfd revents were (POLLIN|POLLERR|POLLHUP|POLLNVAL is readiness; timed-out is the old timeout answer; EINTR restarts disappear because the loop absorbs signals); the per-task pollfd rows and the poll FUNCTION: declarations retire where nothing else reads them; every public effect and every result variant of the six packages stays as it is. The loop is explicit (parent dot): a program must AIO:LOOP-START before the first wait, and a wait with no loop is E-AIO-STATE by name - no fallback to poll(2). Announce the requirement to loom, maki, kiba, radar and tender before landing (memory: API breaks) and update every suite and tool in this tree that waits. Acceptance: the six suites pass with the loop started in their setup; the thread count during lib/net/tcp4-test.f grows by the loop alone; rg -n 'FUNCTION: [A-Z-]+ poll' lib finds none. Files: the six libraries and their tests, docs of each, docs/aio.md. Verify: their suites, test/run.f. Depends: habu-add-the-io-0c5a9630. Ownership: hazel. Claim: unassigned.
