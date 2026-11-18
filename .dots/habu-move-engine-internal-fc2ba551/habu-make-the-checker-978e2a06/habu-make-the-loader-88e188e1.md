---
title: Delete XREF-REQUIRE-INDEX
status: closed
priority: 2
issue-type: task
created-at: "2026-11-18T16:11:36.433984+01:00"
closed-at: "2026-11-18T22:10:18.658641+01:00"
---

Problem: XREF-REQUIRE-INDEX (src/habu/xref.f, no package) is a global word nothing in the engine calls; a program calls it (' XREF-REQUIRE-INDEX drop passes tools/check.f -- --verify-only, rc 0). Its only caller is test/x86-64-kernel-prof.f :106-110, carl's. Acceptance: the word is deleted and that test passes. Depends: carl's change of test/x86-64-kernel-prof.f to stop calling it. Claim: unassigned.
