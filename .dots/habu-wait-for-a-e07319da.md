---
title: Wait for a TCP connection to become readable within a deadline
status: open
priority: 1
issue-type: task
created-at: "2026-09-17T01:57:14.993705+03:00"
---

Problem: lib/net/tcp4.f publishes READABLE? and PENDING?, both zero-timeout polls (POLL-ONCE calls 0 >MS POLL-RAW), although POLL-RAW already takes a millisecond timeout and POLLFD is task-local; a server keeping connections alive can only wait with a TASK:PAUSE loop around READABLE?, which burns a core per idle connection (Tender server/http/request.f WAIT-READABLE, 2026-09-17). Acceptance: TCP4:READABLE-WITHIN? ( connection ms -- ready-result ) parks the task in poll(2) for at most ms milliseconds and answers ready, idle (deadline passed) or failed with errno, EINTR retried against the remaining time; the same for a listener, TCP4:PENDING-WITHIN? ( listener ms -- ready-result ); tests: a peer that writes after 50 ms is ready within 500 ms and the wait took at least 40 ms; a silent peer answers idle after 100 ms and the wait took at least 90 ms; a closed peer answers ready (EOF is readable). Files: lib/net/tcp4.f, lib/net/tcp4-test.f, docs/tcp4.md. Verify: lib/net/tcp4-test.f on a rebuilt engine (no engine change expected). Depends: habu-add-tcp-sockets-fb1d351e. Ownership: lib/net/tcp4.f. Claim: agent=aspen workspace=.jj-ws/habu-wait-for-tcp
