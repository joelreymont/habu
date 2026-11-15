---
title: Quote no empty signature in a depth refusal
status: open
priority: 2
issue-type: task
created-at: "2026-11-15T18:14:01.632958+02:00"
---

Problem: a generated word whose effect is too deep is refused with an empty quoted signature. A STRUCTURE of 4095 n FIELDs and one option<n> FIELD prints "habu: in PBIG:MAKE: bad stored signature '' effect too deep to record (depth 4097, at most 4096)": USIG-DEPTH-FITS? (src/core/checker.f:10884) passes the signature text, which is empty for MAKE. Acceptance: the refusal names the word and its depth without an empty quote; a test covers it. Depends: none. Claim: unassigned.
