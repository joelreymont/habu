---
title: Say which cause an E-CAP-TRUSTED refusal has
status: open
priority: 2
issue-type: task
created-at: "2026-10-31T13:28:26.236280+02:00"
---

Problem: E-CAP-TRUSTED renders 'is a trust-boundary primitive' both for a trusted-only primitive and for a sealed row without authority (checker.f CALL-AUTHORITY), so a reader cannot tell a real boundary from a missing row; the TRUSTED: census misread both as one. Acceptance: the two causes print distinct reasons; a rejected program for each. Files: src/core/checker.f CALL-AUTHORITY, src/core/render.f. Verify: rejected programs. Depends: none. Ownership: those files. Claim: unassigned.
