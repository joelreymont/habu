---
title: Give x86 the ARM crash texts
status: open
priority: 2
issue-type: task
created-at: "2026-11-13T14:40:35.743228+02:00"
---

Problem: src/habu/boot-x64.f:538 says LOOP-HIT$ and its siblings match src/lib/crash.f CRS-*$ byte for byte, but x86 writes 'hb: stack bounds exceeded (loop)' where ARM writes 'E-STACK-BOUNDS habu: ... lr' (test/engine-stack-lifecycle-lib.f expects each); one machine-independent text should come from one place. Acceptance: x86 and ARM print the same crash texts from crash.f, the comment is true, the lifecycle test expects one text for both. Depends: the x86-64 step (x86-64 is not built until then). Claim: unassigned.
