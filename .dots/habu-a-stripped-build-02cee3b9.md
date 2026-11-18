---
title: A stripped build faults on undefine of a primitive
status: active
priority: 2
issue-type: task
created-at: "2026-11-18T22:22:43.015753+01:00"
---

Problem: a program whose first line is 'undefine dup', followed by a MAIN, stops its stripped build on the installed hb with E-MEMORY-FAULT in DICT-CELL@ at 0x23fffe0 (/tmp/jerry-uf-repro/u7.f); the same line under hb --load works, and undefine of a colon word in a build works. Acceptance: the build rejects the line with a reason or builds, by the root cause; a test holds the case. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-uf.
