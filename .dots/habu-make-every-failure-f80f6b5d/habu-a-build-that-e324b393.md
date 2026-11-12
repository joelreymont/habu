---
title: "A build that ends in LINKER:E-FAILED says why"
status: active
priority: 2
issue-type: task
created-at: "2026-11-12T18:10:30.648428+02:00"
---

Problem: a build whose program caught a refusal (/tmp/hunt-rewind/w4.f: a caught E-CTOR-PROTECTED from `undefine SPAN-SPAN:MAKE`, no string literal after it) exits 74 with LINKER:E-FAILED and prints no reason (src/habu/linker.f near :4898). Acceptance: the build's last line names what failed, which file and the code it caught. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/linker-failed-why.
