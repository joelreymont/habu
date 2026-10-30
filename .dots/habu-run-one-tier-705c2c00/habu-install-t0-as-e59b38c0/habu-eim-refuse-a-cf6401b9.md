---
title: "eim: refuse a failed token inside an evaluate on arm64"
status: active
priority: 2
issue-type: task
created-at: "2026-10-30T18:09:27.459198+02:00"
---

Problem: an immediate's catch around evaluate-closed inside an open definition never sees the refusal on arm64 native tiers: the kernel capture (habu2.f EM-COMPILE) lacks x86-64's DEF-PREFIX-BIND (definers.f :597-612), so HOST's ; refuses (rc 70) where the JIT prints 70 and 5. Acceptance: /tmp/rev-rows/s/eim-t1.f prints 70 and 5 rc 0; test/compiler/native-eval.f CALLER-OPEN-REFUSED passes at the default tier with T0. Files: src/habu/habu2.f (+ layout.f cell if needed). Brief /tmp/jerry-big/t0-eim-brief.md. Base wsuunvut (me2). Claim: agent=jerry workspace=.jj-ws.noindex/jerry-eim
