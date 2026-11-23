---
title: Register owned primitives by their rows alone
status: open
priority: 2
issue-type: task
created-at: "2026-11-23T13:10:48.687668+01:00"
---

Problem: once an owned primitive's kernel row is hidden by its EPPRIM: row (habu-add-str-data-81aad07f; .out/designs/design-ownhide.md Design item 5), the registration's own wordlist (GLOBAL-INT-WID with FPRIM-WID on arm64 and PRIM-WID on x86-64), DNAME-OWNED, CK-REC-BIND's owner branch (src/core/checker.f) and REC-CALL's DNAME-INT exemption (src/compiler/native/dict.f) are a second, ignored statement of ownership or unreachable. Acceptance: all five are gone, their call sites use FPRIM and PRIM, kernel code bytes are unchanged, and dot habu-delete-the-internal-3a818b8a no longer names DNAME-OWNED. Depends: habu-add-str-data-81aad07f. Claim: unassigned.
