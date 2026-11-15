---
title: Check a versioned library name against the path limit exactly
status: open
priority: 2
issue-type: task
created-at: "2026-11-15T08:09:22.151369+01:00"
---

Problem: src/lib/ffi-abi.f BASE! checks a base against the path limit at the longest version (AFFIX-BYTES), rejecting a base 1-3 bytes under the limit that fits its real version. Acceptance: the made name is checked where it is composed, placed at the base token. Depends: the generated-source buffers dot. Claim: unassigned.
