---
title: Check what x86 peer images write
status: open
priority: 2
issue-type: task
created-at: "2026-11-25T16:28:23.844132+01:00"
---

Problem: test/x86-64-kernel-{engine,policy,callback}.f describe the text each image writes to fd 1 and 2 in comments; the suite checks only its ending, so the text drifts unseen (the kernel-crash images compare their own). Acceptance: the suite checks that text where the image writes it, or the comment claims nothing. Depends: none. Claim: unassigned.
