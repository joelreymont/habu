---
title: Report a refused long name without overrunning
status: open
priority: 3
issue-type: task
created-at: "2026-10-02T08:48:13.092519+02:00"
---

Problem: src/compiler/native/compiler.f:217-221 KEEP-TAPE-NAME stores NAME-U before checking it against NAME-CAP (64, :85; NAME-BUF :88), then throws E-NCOMP-TEXT; REPORT-FAILURE (:674 NAME-BUF NAME-U @ ERROR-TEXT) then reads NAME-U bytes from the 64-byte NAME-BUF: a refused definition whose name is longer than 64 bytes prints the previous name, NUL bytes and whatever follows the buffer (r4-ncname lane c2bef4af, probe $HOME/.cache/tmp/kestrel-r4-ncname/repro3.f TRY-LONG). The same cap makes tier 1 refuse a name tier 0 accepts. Acceptance: no read past NAME-BUF on any path (every NAME-BUF NAME-U use bounded by construction); a definition with a name longer than 64 bytes either compiles at tier 1 or is refused with its full name printed and the reason (the name's length against the limit), the choice stated with the length limits tier 0 and the dictionary actually have; a case through the real load path seen failing first. Base: after c2bef4af lands. Baked: rebuild, g1 == g2 with .names, two-generation build.
