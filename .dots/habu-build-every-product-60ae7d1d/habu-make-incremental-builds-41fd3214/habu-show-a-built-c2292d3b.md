---
title: "Show a built program's header cells carry nothing a top level left"
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T14:31:35.986118+02:00"
---

Problem (by reading, not reproduced): src/habu/link-data.f:102-110 says the header unit is live and a file's stores into header cells ride its object. GENIO-ABI's routing cells (OUT, IN, ACTIVE, BUSY, the write table) and the main task's TASK:+USER rows are header cells a top level can change (a top level that logs, or redirects output). genio-core.f's SELF, SELF-STATE, FAULT! and the RUN-*-OP words read the active row unchecked, safe only while ACTIVE reads 0 or a live row. Shown so far: test/file-objects.f L and L2 pass (log-first.f logs at top level, the program logs in MAIN) with the main task registering no release; that shows no failure, not that nothing is carried. Acceptance: a case of test/file-objects.f whose earlier file's top level redirects output or leaves a row active, built cold and warm, shows the program starts with fresh routing cells and user rows in both; or the cells that are carried become scratch or are released at the file's end. Files: test/file-objects.f, test/fixtures/file-objects/, src/lib/genio-core.f, src/habu/link-data.f. Verify: test/file-objects.f. Depends: habu-end-a-file-5555197c landed. Ownership: those files. Claim: unassigned.
