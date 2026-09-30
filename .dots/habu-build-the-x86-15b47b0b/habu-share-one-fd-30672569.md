---
title: Share one fd-2 refusal helper in the x86 kernel
status: closed
priority: 3
issue-type: task
created-at: "2026-09-30T13:02:53.356273+03:00"
---

Problem: the x86 kernel writes a fixed text to fd 2 and exits under several names: `INDEX-FAIL,` (K9d), `HOOK-DIE,` (K9c), `REFUSE-BODY` and `STDERR-EXIT,` (K9a) all repeat the `STDERR-WRITE,` then exit shape with different statuses.
Acceptance: one `X64KERNEL` helper takes the text and the status; every caller uses it; emitted images keep their bytes on fd 2 and their exit statuses.
Files: `src/habu/kernel-x64.f`, `docs/x86-64.md` if it names the helpers.
Verify: ThinkPad: every x86 kernel suite and every image natively with its stated status.
Depends: the K batch (K6b, K8b, K9a, K10a) landed.
Route: direct (x86-only files).
Ownership: krait (Intel lane).
Claim: unassigned.
