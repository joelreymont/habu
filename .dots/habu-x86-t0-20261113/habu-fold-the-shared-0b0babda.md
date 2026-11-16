---
title: Fold the shared-code review findings
status: active
priority: 2
issue-type: task
created-at: "2026-11-17T10:44:56.470831+01:00"
---

Problem: the merge review found a broken FFI rejection test (ffi-test.f:127), dead ARM64:PLACE, a public ARM-ADOPT with a repeated check, and in src/os/linux/elf.f a bare relocation number and a require of the x86 target layout that makes every arm64 warm build rewrite three objects (file-objects, arm64-link-run red). Acceptance: ffi-cabi, arm64-link-run and file-objects pass; x86 cross-build exits 0. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-x86 (merged, lands with the feature).
