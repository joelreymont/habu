---
title: Let the REPL terminal accept lines past 255 bytes
status: open
priority: 3
issue-type: task
created-at: "2026-10-27T22:03:26.656842+02:00"
---

Problem: the tty editor in src/habu/repl.f holds the line in a fixed 256-byte LBUF (LLINE-MAX 255) and history in HIST, 16 fixed 256-byte slots with a one-byte length. RD-LINE refuses a longer line by name and reads again from an empty buffer, and GENIO:READ-LINE routes the terminal to RD-LINE, so terminal REPL lines are still capped while stream devices grow (habu-grow-genio-lines-4b5c0afb). The refused line cannot be re-read: the editor consumes keys in raw mode and counts the overflow (INSCH bumps LOVER) instead of storing it, so the tail is gone once it is refused. Counting is deliberate: refusing mid-line would leave a pasted tail to run as the next line. Removing the cap means the line under edit becomes a growable buffer (BUF, src/lib/byte-buffer.f, or an engine-side equivalent, since repl.f is engine source and loads before the library), INSCH and DELCH and REDRAW work on it, history slots hold variable-length lines with a length wider than a byte (or a growable ring), LOVER, LINE-FULL and the refusal go away, and the engine is rebuilt. Acceptance: a pasted line of 64 KiB over a pty is accepted whole and executed once; history recalls it; no fixed line cap remains in repl.f. Verification: the pty REPL rows registered in test/gate-stdlib-cases.f for repl.f, plus an engine build and two-generation build.
