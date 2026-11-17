---
title: Say where an XML document was rejected
status: open
priority: 2
issue-type: task
created-at: "2026-11-17T15:34:08.698043+01:00"
---

Problem: a failed XML:NEXT (src/lib/xml/read.f:382) poisons the reader and no word says where it stopped, so a rejection of a malformed document has no place (diagnostics are Rust-style). Acceptance: XML's rejection of a malformed document carries its line and column. Depends: none. Claim: unassigned.
