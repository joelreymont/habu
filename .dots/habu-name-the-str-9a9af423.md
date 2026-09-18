---
title: Name the string pool close in the retained compiler
status: open
priority: 3
issue-type: task
created-at: "2026-09-18T10:44:43.546905+03:00"
---

Problem: src/compiler/native/string.f:186 NSTR:WINDOW-OPEN starts a fresh 512 KB intern pool at here and clears the intern hash, and the stripped linker (habu-open-the-stripped-3d79f2fa) now calls it twice: once to open the application's pool inside the capture span and once, from AOT-DATA-SPAN, to move the retained compiler's interning above BLOB-END so linker and library literals do not travel in the image; the second call is a close spelled with the open word, because string.f is compiled into the engine and adding a word there costs a three-generation rebuild the fix could not wait for. Acceptance: NSTR:WINDOW-CLOSE ( -- ) with the honest name and a comment stating both halves of the contract (the span's pool is the application's; the pool after the close is the builder's and never travels), AOT-DATA-SPAN calling it, the fixpoint (gen1 differs, gen3 == gen2), the stripped fixtures unchanged in size. Files: src/compiler/native/string.f, src/habu/aot-window-latch.f, docs/native-applications.md. Verify: three generations with cmp; tools/hb-build-stripped-lib-test.f; test/run.f. Depends: habu-open-the-stripped-3d79f2fa. Ownership: native compiler. Claim: unassigned.
