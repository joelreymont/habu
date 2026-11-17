---
title: Answer only the reasons each NUM validator has
status: open
priority: 2
issue-type: task
created-at: "2026-11-17T11:44:45.812894+01:00"
---

Problem: one NUM:numeric-result<a> with seven variants serves every validator (src/lib/num-types.f:37-45), though BYTE-LEN answers only ok or negative and ALIGNMENT only ok or bad-alignment. The run-time wrappers (src/lib/string-roles.f:39-66, elf32.f:53, net/datagram.f:120, net/udp4.f:65, net/tcp4.f:167, xmodem.f:47, serial.f:67) carry about 49 arms for reasons their validator never answers. Acceptance: each wrapper matches only reasons its validator can answer, and no arm remains that no input reaches. Depends: a design; habu-eliminate-a-parameterised-7ce778f8 may carry the eliminator. Claim: unassigned.
