---
title: Drop the unused control-flow stack region
status: open
priority: 2
issue-type: task
created-at: "2026-11-18T14:32:25.068410+01:00"
---

Problem: src/habu/layout.f:75 and :149 size DICT-SIZE as CFSTK-OFF plus a $1000-byte control-flow stack region, but nothing reads or writes that region: habu1.f:2619 and kernel-x64.f:1043 use CFSTK-OFF only as the dictionary's end, and the compilers' control stacks grow on their own (t0-label.f:83, t0-wasm.f:384). Acceptance: DICT-SIZE is the dictionary's end, the code area starts there, layout.f's comments state it, the build converges and the suite passes. Depends: none. Claim: unassigned.
