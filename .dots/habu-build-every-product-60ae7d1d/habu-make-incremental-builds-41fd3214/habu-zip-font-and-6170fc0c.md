---
title: Zip, font and genio handle counters restart in a later file
status: active
priority: 2
issue-type: task
created-at: "2026-11-10T15:13:21.925090+02:00"
---

Problem: the process-state stack (unpushed, tip 259191a9) made src/lib/zip-state.f:54 and src/lib/font.f:98 SERIAL scratch, and src/lib/genio-core.f SLOT-STATUS scratch while SLOT-GEN is carried and bumped only at release (:171-173). A later file and the program read scratch as zero, so the next object opened gets the number an earlier one had: a handle a file's top level kept names the new object instead of being refused. Rule: a counter that makes an old handle stale is not process state; it is carried (as src/lib/pg.f GENERATION). Acceptance: SERIAL is a plain variable in both; genio's claim bumps the row's generation before the row is LIVE (FREE -> CLAIMING -> LIVE); build K of test/file-objects.f (fixtures kept-zip.f, kept-genio.f, kept-main.f) prints 'zip refused' and 'genio refused', cold and warm, and printed otherwise before the change. Files: src/lib/zip-state.f, src/lib/font.f, src/lib/genio-core.f, test/file-objects.f, test/fixtures/file-objects/. Verify: suites zip, genio, log, file-objects. Depends: none. Ownership: those files. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-ida (brief .jj-ws.noindex/jerry-wasm/.out/briefs/ida.md).

Revision (2026-11-10): genio-core.f needs the REPL's KEY1 (dot habu-a-stripped-program for the finding), so the genio case is its own program: build K is kept-main.f (zip, stripped) and build G is kept-genio.f built with --repl; each prints its 'refused' line.
