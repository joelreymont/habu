---
title: Carry two loop values through an until back edge
status: open
priority: 2
issue-type: task
created-at: "2026-08-13T18:45:01.490700+02:00"
---

Found pre-existing by the return-stack join landing (8db52e9e, proved on master WITHOUT the lane): begin...until carrying 2+ loop-carried values refuses E-A64RA-EDGE (-8507) - measured minimal : RV1 ( n -- n ) 0 begin 1 + dup 40 > until + ; on master. begin/while/repeat and counted loops carry the same width fine; only the until back edge is affected. The join suite could not include an until differential because of it. Files: src/compiler/native/regalloc.f or the edge staging in elaborate.f - find the owner first. Depends: none.
