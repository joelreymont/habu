---
title: Deliver standalone native application images
status: open
priority: 1
issue-type: task
created-at: "2026-09-10T18:03:13.378322+03:00"
blocks:
  - habu-relocate-quotations-stored-f112fc21
---

Owner: Cedar; paused Astra app_image work .jj-ws/cedar-app-writer revision 4c58d6dd5b035ecb14c3fa74a16b9131b2149d2d (utnpkrmx). Files: src/habu/app-image.f, snap-lib.f, habu2.f, layout.f, src/os/script-argv.f, tools/hb-build-lib.f/hb-build-test.f, test/app-image.f/app-image-subject.f. Preserve implementation of tools/hb-build --repl source.f -o binary: load once, save compiled dictionary/DATA plus native checker/compiler/REPL, no source replay or shell launcher. Verified 24MB image write, restore after RX-order fix, fresh invalid definition rejection, and source-free recapture. Stored quotation execution remains broken (prerequisite). MAIN startup code builds but is unverified: APP-ENTRY XT cell $43A0, format v8, checked START!, current-process script argv before MAIN, stdin/TTY after MAIN returns. Test no args, app args, optional --, piped input, arbitrary cwd, valid/rejected new definitions, retained data and repeated save. Update old replay/cache-based --repl test expectations; preserve non-repl tests. Independent Astra review and combined runtime tests required before landing. Probe /tmp/cedar-subject-app and /tmp/cedar-subject-save.log may remain from session1106.

Standalone public hb-build --repl and --report-json production runs pass on app candidate5d50b70b plus follow-up startup fixes. MAIN runs once with argv, returns to a working checked REPL, fresh recapture works without source; incorrect MAIN effects reject70 and throws before input initialization fail with original diagnostics instead of crashing. Final persistent PTY regression is running after adding O_NOCTTY to test-only slave opens (avoids controlling-terminal SIGHUP). Freeze/review follow-up before combining with strict/cleanup. No final combined application acceptance yet.
