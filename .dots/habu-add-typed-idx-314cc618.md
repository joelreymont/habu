---
title: Add typed index byte arithmetic
status: active
priority: 1
issue-type: task
created-at: "2026-07-28T01:34:31.462188+02:00"
---

Problem: the checked GPT-2 F32 reader cannot express index below item count or index times element width without erasing nominal values. Owner and exact interface: package CAD-NUM in lib/cad-num-arithmetic.f owns INDEX-IN-COUNT? ( CAD-NUM:index CAD-NUM:item-count -- bool ), INDEX-BYTE-OFF ( CAD-NUM:index CAD-NUM:byte-len -- CAD-NUM:numeric-result<CAD-NUM:byte-off> ), and BYTE-OFF-IN-LEN? ( CAD-NUM:byte-off CAD-NUM:byte-len -- bool ). Comparison is strict. The product returns ok only when representable and overflow otherwise. Implementation reuses only CAD-NUM private projections and numeric-result constructors; the existing DIV-BYTES-FLOOR and ADVANCE-BYTE-OFF remain the other F32 composition operations. It adds no TRUSTED word, nominal role, public raw projection, generic n arithmetic, pointer, MEM, or GPT-2 code. Dependency: none. Checkpoint: compile the exact checked F32 bounds and offset shape through bin/hb and record that the three nominal operations are unavailable; prove CAD-NUM package ownership on the first representative diff. Tests in lib/cad-num-arithmetic-test.f cover zero, one, equality boundaries, maximum values, first overflow, zero and unit scales, result arms and nominal role swaps, plus zcheck properties for comparison truth, representable multiplication, overflow, and result roles. Mutations accepting index equal to count or byte offset equal to length, wrapping the product, swapping roles, or exposing raw public values must fail. Files: lib/cad-num-arithmetic.f, lib/cad-num-arithmetic-test.f, FILEMAP.md only. Acceptance: focused arithmetic suite, exact owning load, typed-local, package, file-map, trust, and owning standard-library gates pass. Smallest owning-path check: the checked F32 offset candidate certifies without any cast when these three operations combine with the existing division and offset-advance operations.

Claim: agent=cad-index-bytes workspace=.jj-ws/habu-add-typed-idx-314cc618
