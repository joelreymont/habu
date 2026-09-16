---
title: Route text I/O through generic devices
status: open
priority: 2
issue-type: task
created-at: "2026-09-16T13:16:24.306550+03:00"
---

Problem: EMIT, TYPE, KEY and ACCEPT are bound to the process terminal, so a socket or serial link cannot become the REPL's or a report's I/O without rewriting those words. VFX's generic I/O layer makes every device a vector table (sd-emit, sd-key, sd-key?, sd-read, sd-write, sd-accept, sd-flush, sd-close) selected per task through IP-HANDLE and OP-HANDLE, and its socket device gives a remote REPL for free (docs/socket-models.md section 2). Acceptance: a checked device record type with those operations, per-task current input and output device cells, the terminal as the default device, a TCP connection from habu-add-tcp-sockets-fb1d351e as the second device, and the REPL usable over a loopback TCP connection with no change to REPL code; a device error fails with a named code and never hangs. Files: new lib/genio.f, the REPL I/O seam under src/habu, lib/net/tcp4.f, tests, docs. Verify: a test that sends a checked definition and a query through a loopback socket into the REPL and reads the answer back; existing terminal tests unchanged; bin/hb --load test/run.f green. Depends: habu-add-tcp-sockets-fb1d351e. Ownership: lib/genio.f, the REPL I/O seam and tests. Claim: unassigned.
