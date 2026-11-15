---
title: "BUF:buffer: a typed byte-buffer header; HDR-BYTES deleted"
status: open
priority: 2
issue-type: task
created-at: "2026-11-13T07:46:36.456009+02:00"
---

Problem: src/lib/byte-buffer.f makes callers reserve a raw three-cell header (create X BUF:HDR-BYTES allot, HDR-BYTES sums in hand-laid layouts, TASK:+USER and SCRATCH-ALLOT byte counts), against docs/design.md:19-21 typed containers; Maki's first typed-container site (maki-hold-a-project-70f4b714) waits. Joel 2026-11-13 named the type BUF:buffer. Acceptance: BUF exports the record type BUF:buffer; INIT, DISPOSE and every BUF word take ptr BUF:buffer; BUF:HDR-BYTES is gone; every Habu caller (52 files: src, tools, test incl. test/xmodem.py, test/serial-xmodem.py) holds its header in a typed container; the engine builds and the suite passes; Zen, Tender and Maki owners are sent the cutover. Design: the root checkout's .out/designs/design-bufhdr.md. Depends: habu-user-band-cannot-4ca18d06 (typed task slots). Claim: unassigned.
