---
title: Rename GENIO to CONSOLE
status: open
priority: 2
issue-type: task
created-at: "2026-10-30T17:10:51.876146+02:00"
---

Problem: package GENIO (src/lib/genio-core.f, src/lib/genio.f) is the device table behind EMIT, TYPE, KEY and ACCEPT: each task's console device (the terminal, or a TCP connection for a remote REPL). 'GENIO' (generic I/O) says nothing about that and claims more than it holds; files and descriptors live in fs.f and fd.f. Joel 2026-10-30: call it CONSOLE. Acceptance: package GENIO becomes CONSOLE, GENIO-ABI (src/habu/layout.f, baked) becomes CONSOLE-ABI, E-GENIO-* become E-CONSOLE-* (src/lib/errors.f), the files become src/lib/console-core.f, src/lib/console.f and src/lib/console-test.f, engine labels and comments that say GENIO or generic I/O follow (src/habu/rt.f LGENIOOUT, src/arch/x86-64/rt.f, src/habu/kernel-x64.f, habu2.f, data-claims.f, src/lib/task.f, tools/native-definition-cache.f, the tests); hard cutover, no alias; rg -i genio finds nothing in src, tools, test, docs or bootstrap. No Maki or Etch file names GENIO (rg 2026-10-30). Files: the 15 files rg -l GENIO lists, plus every require of the renamed files. Verify: engine build to two matching generations; the console test; bin/hb --load test/run.f (layout.f is baked). Depends: the W^X landing (same engine files). habu-write-private-stdio-b549d86d edits genio-core.f and rebases onto this. Ownership: the console package and its engine cells. Claim: carl.
