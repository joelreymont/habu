---
title: "Make a compare's flag in one step"
status: open
priority: 2
issue-type: task
created-at: "2026-11-25T17:53:57.749693+01:00"
---

Problem: T0 makes a flag from a compare in two seam steps, so an x86 flag costs test or cmp, setcc, movzx and neg (13 bytes) where ARM has cmp and csetm, tenderd's x86 code settles its stack pointer 3,600 times before a compare no branch reads, and arm64's 0< is two instructions instead of one asr. Acceptance: tenderd's x86 code shrinks, arm64's does not grow, code-bench is no slower on either host. Depends: habu-reuse-a-register-ea2c40ba. Claim: unassigned.
