---
title: "Wasm 6g: a cross build links its unchanged files"
status: open
priority: 2
issue-type: task
created-at: "2026-11-12T09:00:02.866914+02:00"
blocks:
  - habu-run-a-cross-f6c97962
---

Problem: src/habu/linker.f LOAD's CROSS? guards (the link turn's 'CROSS? 0= and', the compile turn's 'CROSS? UNLINKABLE !') keep a cross build from linking any object. Removing them is change smnnxyss in .jj-ws.noindex/jerry-s3w, on 6d; it waits for the cross build to compile a target word once and a host word for both machines (habu-run-a-cross-f6c97962), so an object holds one emission per machine; linking places a file's host emissions in the build's process. Acceptance: a second Wasm build of Maki's viewer copy compiles 0 files and links every file, its module cmp-equal to a build into an empty out root; after each edit the arm64 file-objects fixture makes, the Wasm compiled count equals arm64's; a create-does> definer and a callback definition in linked files; test/wasm/hb-build.f and the browser tests pass; a linux-aarch64 cross build cold and warm byte-equal. Depends: habu-run-a-cross-f6c97962. Claim: unassigned.
