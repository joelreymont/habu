---
title: Let the REPL terminal accept lines past 255 bytes
status: open
priority: 3
issue-type: task
created-at: "2026-10-27T22:03:26.656842+02:00"
---

Problem: the tty editor in src/lib/line-editor.f holds the line in a fixed 256-byte LBUF (LLINE-MAX 255, :17-34) and history in HIST, 16 fixed 256-byte slots with a one-byte length. LINE-EDITOR:READ-LINE refuses a longer line by name and reads again from an empty buffer, and GENIO:READ-LINE routes the terminal to it (src/lib/genio-core.f:523-524), so terminal REPL lines are still capped while stream devices grow. The refused line cannot be re-read: the editor consumes keys in raw mode and counts the overflow (INSCH bumps LOVER) instead of storing it, so the tail is gone once it is refused. Counting is deliberate: refusing mid-line would leave a pasted tail to run as the next line. Removing the cap means the line under edit becomes a growable buffer (BUF, src/lib/byte-buffer.f), INSCH and DELCH and REDRAW work on it, history slots hold variable-length lines with a length wider than a byte (or a growable ring), LOVER, LINE-FULL and the refusal go away, and the engine is rebuilt. It is an acceptance case of its parent, habu-move-the-interpreter-d35780fa: fix it in the code that stays, never in the machinery the parent replaces. Acceptance: a pasted line of 64 KiB over a pty is accepted whole and executed once; history recalls it; no fixed line cap remains in src/lib/line-editor.f. Depends: none. Claim: unassigned.
