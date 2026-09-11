---
title: Give PROCESS-PTY a real PTY and a public write/await surface
status: open
priority: 2
issue-type: task
created-at: "2026-09-12T00:55:05.101451+03:00"
---

Problem: lib/process-pty-io.f supervises a child over ordinary pipes (its header: nothing here opens a PTY device), so the engine REPL never installs (src/habu/repl.f INSTALL is TTY?-gated) and a batch REPL stops at the first refused definition instead of recovering; and the handle's master/done descriptors are private (PROCESS-PTY:MASTER undefined outside the package), so no caller can interleave a write with a read. Reported by tender-b3 on 2026-09-11 (message 20260911-215435.840-tender-b3-4873) for Tender's Habu-written standalone gate, which cannot reproduce the old Python gate's REPL-recovery check: send a wrong-identity definition to 'tender repl', expect the refusal, then evaluate '6 7 * . cr' and see 42 and the next prompt. Acceptance: a PTY-backed session (openpty or posix_openpt through the existing FFI/syscall seams, one per handle, torn down with the child) under which the child engine installs its line editor; public words to write a line and to await bytes with a timeout on the handle, typed and fail-closed; a regression that spawns bin/hb under the PTY, sends a refused definition then an arithmetic line and reads the refusal, 42 and the next prompt; the pipe-backed path stays for non-interactive callers. Files: lib/process-pty-io.f, lib/process-pty-handle.f, lib/process-pty-handle-test.f, src/os/linux/sys.f (ioctl/openpt numbers if missing), docs/process-pty.md. Verify: the process-pty suites plus the new regression on bin/hb. Depends: none. Ownership: hazel (lib/ and src/os are the second maintainer's); nothing in rowan's files. Claim: unassigned.
