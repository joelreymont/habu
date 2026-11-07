---
title: "File objects L2e: pieces cut at each load request"
status: open
priority: 2
issue-type: task
created-at: "2026-11-06T21:39:01.493231+02:00"
---

Problem: RUN-FILE refuses a load made during a file's compile (src/habu/linker.f:695 LOADER$ nip 0<> E-COMP-STATE), so every object has one piece and no REQUEST row before its final -1; real library files, which require others, cannot be built through the linker, and Habu itself cannot build incrementally. Design: .out/carl-file/design-fobj.md section 2 item 1 and section 3 PIECES/REQUESTS: a file's object is its top level cut at each load request, each piece with its own records, CODE, DATA, STRINGS and (after L3) STORES, and the REQUESTS row between pieces. Acceptance: RUN-FILE keeps a stack of the open files' marks (FILE-REC, FILE-ROW, FILE-EM, FILE-DP, FILE-WID, FILE-END) so a nested load closes the loader's current piece, runs the requested file's compile with its own object, and opens the loader's next piece on return; a request already satisfied still writes its REQUEST row; a failed nested file ends the build (DECISIONS 2026-11-06 'a failure caught inside a build'). Verify: test/file-objects.f with a fixture that requires another inside the build and one require already satisfied: the loader's object has two pieces and two REQUEST rows, the required file its own object, and two fresh-process builds stay byte-identical. Depends: product text's load words performed by hb's loader during a build (carl's finding to jerry, /tmp/carl-loads.md, S5b3 3.1; dot habu-s5c-load-hb-6aae35fd), L2d. Ownership: carl (lead), worker lane. Claim: unassigned.

Acceptance case (error names): a reused file whose ERROR:CODE rows follow an earlier declaring file that recompiled still answers ERROR:NAME$ and ERROR:TEXT$ for every code; it follows from S5c's error.f case (dot habu-s5c-load-hb-6aae35fd), which holds the design.
Acceptance case (does> slot, from L4): the link refuses an object whose created body's does> slot a clause patched (E-COMP-STATE in LINKER's NEMIT-FILL); creating through a does> definer in a build ends the process at rec-wide-publish until S5c. Once it works, the link files the slot's clause target (NSHADOW:PATCH-DOES) and a fixture covers it, with fixture A reaching reopen.f's MAKER;does once habu-find-why-a-9799a46a's 198 KB growth is fixed.
