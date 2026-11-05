---
title: spawn-test fails when the shell inherits fd 8 or 9
status: active
priority: 2
issue-type: task
created-at: "2026-11-05T11:39:16.951341+02:00"
---

Problem: src/lib/spawn-test.f names a child's fd in a /bin/sh script by one digit (`>&@`), because dash and POSIX sh redirect only fds 0-9, so CLOSED-FD (:65-67) asserts each closed parent fd it finds is below FD-CEILING 9 (:21). TEST-CROSSING (:153) takes two, the second searched above the first; on spark (Linux arm64) Tailscale SSH leaves a journald socket on fd 8, so b is 9 and assert 2 fails (rc 1, 'F2 / TFAIL assert 2'). Not the engine, src/os/linux or spawn.f: spark with `8>&-` gives rc 0, and macOS `bin/hb --load src/lib/spawn-test.f 8</dev/null` gives the same F2. TEST-GAP (:182), TEST-REFUSALS (:340, :344) and TEST-CHECK-FD (:417) call CLOSED-FD too. Probes: spark:/tmp/carl-linux/hunt-spawn-probe/. Fix (test only): scripts name a child fd as /dev/fd/N, which dash and macOS sh open for any N (measured: a pipe on fd 12 written through /dev/fd/12, rc 0 on both); CLOSED-FD loses its ceiling and PATCH writes a number of any width. Taking fixed fd numbers with dup2 was rejected: hb's own fds (AIO host, wake pipes) may sit on any number. Acceptance: spawn-test rc 0 on macOS plain, with `8</dev/null`, and with fds 5-9 inherited; rc 0 on spark. Files: src/lib/spawn-test.f. Verify: those runs. Depends: none. Ownership: carl. Claim: agent=worker workspace=.jj-ws.noindex/carl-spawn
