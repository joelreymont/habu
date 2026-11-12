---
title: "Refuse a file that patches an earlier file's does> slot"
status: open
priority: 2
issue-type: task
created-at: "2026-11-07T19:47:12.555947+02:00"
---

Problem: a file whose compile runs does> on the newest created word when that word is an earlier file's patches the earlier emission's slot (NSHADOW:PATCH-DOES, src/compiler/native/shadow.f:484), which a warm link would drop. The link refuses it with LINKER:E-STORE (src/habu/linker.f:2209-2210), but no test holds the refusal. Shape: a.f defines BOX through a does> definer, RE (which runs does>) and 7 BOX RGC-X; p.f's only act is RE; r.f prints RGC-X. Acceptance: a test/file-objects.f case where p.f's RE ends the build with E-STORE, and with p.f's RE removed the build passes (a file's patch of its own word is allowed). Depends: none. Claim: unassigned, owner carl.
