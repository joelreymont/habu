---
title: Fail a build whose source exits while it loads
status: open
priority: 3
issue-type: task
created-at: "2026-11-09T18:37:14.531033+02:00"
---

Problem: hb-build loads the source in its own process (DECISIONS 'where hb-build builds'), so a source whose top level ends the process ends hb-build with that code. A top-level `s\" \" 0 die` gives rc 0, no 'hb-build OK' line, empty streams, and the old -o left as it was; callers trust rc 0 (tools/chain-run.f BUILD-IN checks only FS:EXISTS? after it, so a gN from an earlier run passes; tools/seed.f and tests too). A tool-like source whose top level does its work and exits 0 builds into nothing silently. Path: tools/hb-build.f -> HB-BUILD-CORE:REPL/APP/ENGINE (tools/hb-build-core.f REPL-BODY, APP-BODY) -> the source's top level -> die (src/habu/main.f REPORT) -> EXIT-HOOK (src/lib/fs-mutate.f CLEANUP-AT-EXIT removes the temp directory; the engine keeps the exit code across it). Top-level `bye` is already refused (undefined in the build). With the old maker child the parent checked the product file after the child exited (base HBB-INSTALL-OUT FS:FILE?). Found by the step-5 build-path review (2026-11-09). The fix changes failure handling, so its design goes to Joel before a worker starts (CLAUDE.md Authority). Acceptance: an exit while LINKER:WITH's space is open fails the build with rc 74 naming the source file, -o untouched; a suite case shows it. Files: decided by the approved design. Verify: the case passes. Depends: Joel's approval of the design (lands after arm64). Ownership: hb-build and the linker files the design names. Claim: unassigned.
