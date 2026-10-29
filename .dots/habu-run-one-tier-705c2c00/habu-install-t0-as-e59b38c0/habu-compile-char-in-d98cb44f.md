---
title: Compile char in a T0 body
status: active
priority: 2
issue-type: task
created-at: "2026-10-29T02:44:17.192994+02:00"
---

T0 refuses `char x` inside a body with E-T0-UNSUPPORTED; tier 1 compiles `: CH1 ( -- n ) char 1 ;` and it answers 49. Found by the A3b1 review (2026-10-29). T0 already counts the swallowed operand (PARSING-KEYWORD? covers char); lower it as [char] does. Test: a row in test/t0-install.f, char of an ASCII byte and the refusal past ASCII as DO-CHAR has it.
