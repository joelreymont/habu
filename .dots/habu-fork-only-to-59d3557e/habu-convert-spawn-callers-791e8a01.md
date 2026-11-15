---
title: Convert spawn callers to SPAWN requests
status: open
priority: 1
issue-type: task
created-at: "2026-10-28T20:27:02.244427+02:00"
blocks:
  - habu-spawn-through-a-6227aeda
---

Fork-to-exec lane 1c: the 96 Habu call sites fill a SPAWN request; then the 17 PROC-SPAWN-* words and four PROC-CWD spawn words are deleted (hard cutover). Client callers (etch 1, maki 5, tender 1, zen 5) are their owners' to convert after it lands; installs never wait. Claim: unassigned.

phil found two defects that this lane removes, so neither gets its own fix. (1) IO-FORK-TARGET (src/lib/process-pty-io.f:174): on an empty path the forked child can throw into the parent's copied catch frame (repro below). The PTY target moves to spawn-fds (design item 4), and spawn validates the path in the parent. Make the repro a case of the PTY conversion. (2) pty-harness.f OPEN-SLAVE opens the slave without close-on-exec and outside FD-LOCK, so a concurrent fork inherits it. Once every child is a spawn that closes the fds past its map (1b C2), the leak cannot happen. Check that OPEN-SLAVE needs no lock after lane 6.

Repro of (1): on the installed hb it prints the child's '-2503' and 'after' as target output, then the parent's '0' and 'after', rc 0: the child threw -2503 into TRY's copied catch and ran the parent's code.

```
require src/lib/process-pty-io.f
require src/lib/aio.f
256 BUFFER: OUT
: BODY ( process-pty-handle -- process-pty-handle )
   PROCESS-PTY:LAUNCH OUT 256 2000 PROCESS-PTY:AWAIT-BYTES
   dup 0 > if ." child wrote: " OUT swap type else ." read " . then cr ;
: EP ( -- ) s" " [: BODY ;] PROCESS-PTY:WITH-TARGET ;
: TRY ( -- ) [: EP ;] catch . ." after" cr ;
AIO:START TRY
```
