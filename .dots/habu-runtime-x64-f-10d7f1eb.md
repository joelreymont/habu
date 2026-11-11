---
title: "runtime-x64.f names CHECKER-REG:SEAL and SEAL-TARGET, which no file defines"
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T20:49:40.831065+02:00"
---

Problem: src/habu/runtime-x64.f :2 and :12 name CHECKER-REG:SEAL and SEAL-TARGET; rg -i --no-ignore finds no definition of either in src, tools or test, so the x86-64 runtime cannot load as written. Acceptance: the two lines call words that exist or are deleted with what only they served; the x86-64 engine builds. Files: src/habu/runtime-x64.f. Verify: the x86-64 engine build, when x86-64 work resumes. Depends: x86-64 is set aside until arm64 and Wasm are done. Ownership: that file. Claim: unassigned.
