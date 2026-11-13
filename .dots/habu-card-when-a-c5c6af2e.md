---
title: "Card: when a file can open a package"
status: open
priority: 3
issue-type: task
created-at: "2026-11-01T10:26:42.448081+02:00"
---

docs/forth-card.md section 1 says a package opens 'in the file or string A is open in'. On the installed engine: a file can open a part inside a package another file opened (x.f `package DEMO`; entry.f `require x.f package P public : X ( -- n ) 7 ; ;package : Y ( -- ) P:X . cr ; Y ;package : Z ( -- ) P:X . cr ;` prints 7, then Z is `E-UNDEFINED ... 'P:X'`, rc 70; the same when x.f only requires a w.f that opens DEMO), and a file cannot open a package while one that was open when it started loading is still open (e3.f `package DEMO require k.f ;package` with k.f `package K ;package`: `E-PKG-CONTEXT habu: k.f:1: package`, rc 75). State the rule in the card and docs/forth.md at the heading that holds package scope. Workspace: none yet.
