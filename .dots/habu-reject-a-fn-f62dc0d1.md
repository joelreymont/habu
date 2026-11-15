---
title: "Reject a FUNCTION: left open at end of file"
status: open
priority: 2
issue-type: task
created-at: "2026-11-15T04:52:35.288724+01:00"
---

Problem: a FUNCTION: declaration with no ;FUNCTION before the end of its file leaves FFI's declaration open, with no rejection. Acceptance: the load rejects it at the end of the file with a Rust-style record that names the FUNCTION: line, and tools/check.f prints the same record. Depends: habu-print-the-load-2dad70bd. Claim: unassigned.
