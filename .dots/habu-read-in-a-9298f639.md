---
title: Read .( in a body alike in load and check
status: open
priority: 3
issue-type: task
created-at: "2026-11-25T17:13:53.950845+01:00"
---

Problem: in a definition body the load reads the token after `.(` as a word (`.( X )` gives E-UNREQUIRED for an unrequired X) while check.f --verify-only skips to `)`, so the check passes a program the load rejects. Acceptance: load and check agree on `.( X )` in a `:` and in a TRUSTED: body (same rc, first error and place); a require-check case fails without it. Depends: the require feature's landing. Claim: unassigned.
