---
title: "Wasm 6: a Wasm build links unchanged files' objects"
status: active
priority: 2
issue-type: task
created-at: "2026-11-10T21:32:06.928239+02:00"
---

Problem: a build for another machine than hb's (--target wasm; x86-64 next) compiles every file on every build: src/habu/linker.f CROSS? (:124) and its callers keep it from writing or linking objects. On a copy of Maki's viewer a Wasm build takes 3.79 s every time and writes 0 objects; an unchanged arm64 build takes 0.98 s. Joel's ruling (dot habu-run-a-cross-f6c97962, docs/design.md "Machinery being replaced"): an object holds only the target's machine code, and a cross build's top level runs host words only, so the cross build compiles each definition once, for the target. Steps: 6b-6d (child dots, commits in .jj-ws.noindex/jerry-s3w) make the linker keep, read and test a call site by its emission's machine; 6g removes the CROSS? guards once f6c97962's single compile lands. Acceptance: a Wasm build of an unchanged program links every file and compiles none; after an edit it compiles what an arm64 build compiles after the same edit; the module is byte-equal to a cold build's; an arm64 build's objects and bytes are unchanged. Depends: habu-run-a-cross-f6c97962. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-s3w.
