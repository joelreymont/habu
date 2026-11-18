---
title: A caught failed text keeps what it made and retires its words
status: active
priority: 2
issue-type: task
created-at: "2026-11-12T14:46:03.380434+02:00"
blocks:
  - habu-recover-throws-across-18bd36f9
---

Problem: a caught failed text (`evaluate`, `included`, a REPL line) gives back its code, records and data (src/habu/interpreter.f FRAME-ROLLBACK-CODE: `ndict!`, TRUNCATE, DATA-SPACE:REWIND), so whatever it stored into older data points into space that is laid again: a list row it added to a program's own list is walked into new data (E-MEMORY-FAULT, rc 110), and a `defer` it set runs the next word compiled. The ROLLBACK-CELLS marks put back 16 cells only and cannot cover execution tokens. A later `require` of a file the failed text required loads nothing, so that file's words stay undefined: REQUIRE-REG is not cut back. Only while a task is live does recovery retire the records and keep the space.
Acceptance: after a caught failed text that began with no definition open, its records are retired so no name finds them, the texts it interned leave the string table (INTERN:KEEP-BELOW at the frame's DP), and nothing is lowered, on every machine; a private XREF word retires the records (INTERPRETER's `retire-from` row goes); REQUIRE-REG is cut back to its count as the frame began. Tests: a list row and a `defer` a caught failed text set stay valid after new data is laid; a later `require` of the file it required loads it. docs/forth.md and the card's §3 say a caught failed text keeps its storage and loses its names. The parent habu-move-the-interpreter-d35780fa's cases on a failed line's address cells and on the ADDRMAP bit above a restored CP are cut. Lands with habu-keep-a-build-09b103ba.
Depends: habu-recover-throws-across-18bd36f9.
Claim: agent=jerry workspace=.jj-ws.noindex/jerry-undo.
