---
title: Compile locals in T0
status: closed
priority: 2
issue-type: task
created-at: "2026-10-28T11:21:12.851698+02:00"
closed-at: "2026-11-02T11:30:13.917027+02:00"
close-reason: "Landed 007d6032a1 'Compile locals in T0' on master."
---

Port L1 commit 2b on oylsqluo: {: ... :} groups at entry and mid-body, local reads, multi-cell locals; machine-stack frame sized at CLOSE. Claim: jerry, workspace jerry-t0-locals.
