---
title: Review checker loader preflight
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-17T03:06:21.072389+02:00\""
---

Independent destruction review of revision 71f0c56e and dot habu-checker-reject-compile-c8805039. Read spec and code only; verify compile-only parsing immediates cannot bypass load preflight, sealed-cell lifecycle is privileged without broadening mutation authority, raw/native/recovery/AOT/fixpoint paths agree, exact bad-loader regressions fail closed with diagnostics, and no unrelated compiler machinery was added. Read-only review in an isolated jj workspace; report prioritized file:line findings. Do not edit implementation or close the implementation dot.
