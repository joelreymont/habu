---
title: Find why compiling is twice as slow on x86
status: active
priority: 2
issue-type: task
created-at: "2026-11-25T15:52:54.057605+01:00"
---

Problem: hb --load of Tender's sources takes 4.6 s on spark and 8 to 10 s on pulsar; Zen and Maki show 1.6x to 2.4x on compile-heavy runs, against 1.13x for a Python loop and 1.67x for C++; the merged code-size lanes moved it 4%. Acceptance: each cause is named with its share, and compile-heavy runs on x86 take no more than the C control's ratio to ARM. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-x-hunt.
