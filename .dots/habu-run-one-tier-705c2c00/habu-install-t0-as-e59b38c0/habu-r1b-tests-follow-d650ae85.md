---
title: "R1b: tests follow the arm64 engine baking byte-buffer.f and deftype.f"
status: active
priority: 2
issue-type: task
created-at: "2026-10-30T17:26:50.947256+02:00"
---

R1 review H1: T0's closure bakes BUF and VNOM (sealed, exit 84 on reopen) and the global DEFTYPE on arm64. R1 full suite: 728 pass, reds exactly tail-pure-fixtures, deftype, check-cli-boundary, check-verify. Workspace .jj-ws.noindex/jerry-r1b on ulzktzrp; brief /tmp/jerry-big/t0-r1b-baked-libs-brief.md: WHITEBOX-SUITE rows for byte-buffer and deftype, check.f fixtures off DEFTYPE, uses-visit count, LOAD-T0 comment. Fold into ulzktzrp. Naming interview: row name byte-buffer.
