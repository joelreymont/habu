---
title: Make a value record an entry so a build knows it
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T09:18:04.185602+02:00"
---

Problem: VALUE-RECORD (src/core/roles.f:199) is missing from src/habu/interpret.f DECLARATION?, so in a build the target's copy runs and writes the target's table while the running hb checks signatures against its own: a program requiring a file that declares a value record and names it in a later signature fails to build (measured 2026-11-10 in the linear type lane: fails on master, builds cold once 'value-record' is in the list). Two more gaps keep the line out: the VREC stores (src/core/checker.f near VREC-NAME-A-P) are not registered with CHECK:STORE+, so a value record declared in a build would stay in hb's table after the build closes; and a file's object does not carry a value record, so a warm build that links the declaring file would not know it. The linear type lane (dot habu-make-a-linear-953febee) shows each step for a linear type: registered columns, a type entry at the piece's end, the add when a link remakes the entry. Acceptance: a program requiring a file that declares a value record builds cold and warm, warm equals cold byte for byte, and after the build hb's own table holds what it held before; the engine rebuilds itself byte for byte. Files: src/habu/interpret.f, src/core/checker.f, src/habu/linker.f. Verify: the reduced program cold, edited and warm; engine cold and warm cmp; test/run.f -- file-objects; full suite at landing. Depends: habu-make-a-linear-953febee landed. Ownership: carl. Claim: unassigned.
