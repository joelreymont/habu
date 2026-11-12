---
title: "A build that ends in LINKER:E-FAILED says why"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-12T18:10:30.648428+02:00"
closed-at: "2026-11-12T18:46:14.567728+02:00"
---

Problem: a build whose program caught a refusal (/tmp/hunt-rewind/w4.f: a caught E-CTOR-PROTECTED from `undefine SPAN-SPAN:MAKE`, no string literal after it) exits 74 with LINKER:E-FAILED and prints no reason (src/habu/linker.f near :4898). Acceptance: the build's last line before E-FAILED names the file and why it failed; naming the caught code is an acceptance case of habu-move-the-interpreter-d35780fa. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/linker-failed-why.
