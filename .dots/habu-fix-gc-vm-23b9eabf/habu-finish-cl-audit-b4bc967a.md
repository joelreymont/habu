---
title: Finish CL audit
status: open
priority: 2
issue-type: task
created-at: "2026-02-03T11:04:05.068193+01:00"
blocks:
  - habu-unify-stdlib-paths-b99d770e
---

docs/cl-symbols.md:5-6: 18 symbols pending audit; locations reference stdlib.habu but runtime loads lib/stdlib.habu. Fix: complete audit rows to reach 978; update Location/Notes to real impl sites; keep counts consistent (scripted check). Verification: python row counter (960/978), rg for missing symbols, zig build test.
