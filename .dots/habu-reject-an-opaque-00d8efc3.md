---
title: Follow the top-level stack under --verify-only
status: active
priority: 2
issue-type: task
created-at: "2026-11-16T16:26:06.254389+01:00"
---

Problem: --verify-only judges top-level tokens without the stack they leave, so it passes top-level code --load and tools/check.f reject: `s" 1 drop" ' evaluate-closed execute` and `1 ' dup execute` (E-EXEC-OPAQUE-XT), `s" x" 1 +` (E-EVAL-RESIDUE), each rc 70 there and rc 0 with no record under --verify-only. Nor can it tell which word a top-level execute, catch or finally runs, which habu-carry-a-may-c9f6fc75 needs. Acceptance: the three readers agree on these programs and on top-level code generally; a test pins the --verify-only outcomes. Depends: none. Claim: agent=Plan (design) workspace=.jj-ws.noindex/carl-o8 (read only).
