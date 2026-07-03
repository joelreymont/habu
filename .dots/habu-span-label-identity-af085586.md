---
title: Span label identity beyond string equality
status: open
priority: 2
issue-type: task
created-at: "2026-07-02T00:18:23.161682+02:00"
---

MEDIUM from harness review: span dedupe (GS-CHILD-OWNED?) and subject attribution (GS-LABEL-SUBJ) key on raw label bytes. Two different subjects reusing one label text collide: the fork child suppresses a span the pool does not own, and attribution joins the wrong test row. Fix direction: qualify labels with the pool generation (GT-POOL-GEN$) or a phase id in both the test row and span records, or lint/reject duplicate labels at GT-POOL-START*/GS-TEST time. Add a negative fixture with two same-label entries proving current miscount first.
