---
title: "Show a built program's header cells carry nothing a top level left"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-10T14:31:35.986118+02:00"
closed-at: "2026-11-14T11:28:50.538086+01:00"
---

Problem (by reading, not reproduced): src/habu/link-copy.f:10-12 says the header is live and an address a file stored into a header cell is that file's, as its object carries it. GENIO-ABI's routing cells (OUT, IN, ACTIVE, BUSY, the write table) and the main task's TASK:+USER rows are header cells a top level can change (a top level that logs, or redirects output). genio-core.f's SELF, SELF-STATE, FAULT! and the RUN-*-OP words read the active row unchecked, safe only while ACTIVE reads 0 or a live row. test/file-objects.f L and L2 pass, which shows no failure, not that nothing is carried. A store a file's top level makes into a main-task TASK:+USER row reads 0 in the program, cold and warm (probes .jj-ws.noindex/jerry-wasm/.out/probes/hdr-first.f, hdr-main.f): the build's main task is hb's own DATA. log.f, zip-ffi.f and c2-memory/owner-runtime.f keep their main-task state in such rows. Acceptance: a case of test/file-objects.f whose earlier file's top level redirects output or leaves a row active, built cold and warm, shows the program starts with fresh routing cells and user rows in both; or the cells that are carried become scratch or are released at the file's end. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-hdr.
