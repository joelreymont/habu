---
title: "Wasm 6: a Wasm build links unchanged files' objects"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-10T21:32:06.928239+02:00"
closed-at: "2026-11-13T00:29:17.953770+02:00"
close-reason: "a Wasm or linux-aarch64 build links its unchanged files' objects; arm64 code is unchanged"
---

Problem: a build for another machine than hb's (--target wasm; x86-64 next) compiles every file on every build: src/habu/linker.f CROSS? (:124) and its callers keep it from writing or linking objects. On a copy of Maki's viewer a Wasm build takes 3.79 s every time and writes 0 objects; an unchanged arm64 build takes 0.98 s. The design (docs/xcomp.md "Host and target words", dot habu-run-a-cross-f6c97962): a target word is compiled once, for the target; a host word is compiled for the host and the target, and its object entry holds both emissions; linking a file places its host emissions in the build's process and its target emissions in the output. That removes linker.f LOAD's reason for writing no cross-build object (its header: an object holds one emission). Steps: 6b-6d (commits in .jj-ws.noindex/jerry-s3w) make the linker keep, read and test a call site by its emission's machine, and must keep a host word's two emissions in one entry; 6g removes the CROSS? guards once f6c97962's single compile lands. Acceptance: a Wasm build of an unchanged program links every file and compiles none; after an edit it compiles what an arm64 build compiles after the same edit; the module is byte-equal to a cold build's; an arm64 build's objects and bytes are unchanged. Depends: habu-run-a-cross-f6c97962. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-s3w.
