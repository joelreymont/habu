---
title: Check whether a stub that always throws stops its callers loading
status: open
priority: 3
issue-type: task
created-at: "2026-11-10T22:48:08.090766+02:00"
---

Problem: reported by Zen's agent (phil), not reproduced and with no program kept: the checker refuses code after a call to a word that always throws, so a file cannot load while a stub it calls is ': W ... E-UNSUPPORTED throw ;'. Acceptance: a two-word program (a stub that only throws, and a caller with code after the call) either loads, and the dot closes with it as evidence, or is refused, and then the dot states the refusal's text and site and whether it is the intended rule (docs/forth.md) with a remedy the message names. Files: none until the program decides. Verify: first, write and load that program under bin/hb; ask phil for Zen's program if it loads. Depends: none. Ownership: unassigned. Claim: unassigned. Consumers: met by Zen once, worked around.
