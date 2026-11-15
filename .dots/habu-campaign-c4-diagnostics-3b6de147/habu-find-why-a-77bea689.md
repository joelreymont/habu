---
title: "Find why a quiet TRUSTED: row goes unrecorded"
status: open
priority: 2
issue-type: task
created-at: "2026-11-15T14:03:23.654186+01:00"
---

Problem: when TRUST-DECL? (src/core/checker.f:20638) ends quiet on an unresolved type that a type-making word may still declare (USIG-ADD-ROWS :11359, SIG-UNRES), DL-TAKE runs with DL-NEW 0, so the TRUSTED: row is not recorded on the native path, whether by design is unknown. Acceptance: a reported cause with path:line: either the reading that shows the row is recorded once the type arrives, or a reproducer whose call of the word misbehaves. Depends: none. Claim: unassigned.
