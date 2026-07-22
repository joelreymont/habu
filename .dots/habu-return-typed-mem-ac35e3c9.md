---
title: Return typed memory allocation outcomes
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-22T16:06:15.121298+02:00\""
---

Why: safetensors needs nonthrowing record allocation before an owner exists, but a public raw mmap n-plus-length sentinel would expose address refinement and force every caller to repeat it. Exact interface: package MEM owns allocation-result with allocated(ptr u8, CAD-NUM:alloc-byte-len) and refused(rc, CAD-NUM:alloc-byte-len); MEM:TRY-ALLOC-BYTES consumes one validated positive allocation length and returns that exhaustive result. Only MEM refines a successful mmap address to ptr u8; refusal preserves the exact negative operating-system result as rc and the exact typed length; no raw MEM:ALLOC-BYTES-RC API is published. Existing throwing MEM:ALLOC-BYTES remains a policy wrapper over the same primitive behavior. Owned result: the result family, total allocator, focused success and real refusal tests, manifest/docs/FILEMAP updates. Acceptance: callers must MATCH both arms; success can release the exact span; refusal returns no pointer; checked negatives reject treating refused as allocated or swapping rc and length; safetensors consumes this result and immediately mints its private record owner. Smallest check: bin/hb --load lib/memory-test.f. Depends: none. Ownership: lib/memory.f, lib/memory-test.f, lib/std.manifest, docs/stdlib.md, FILEMAP.md. Claim: agent=mem_alloc_result_impl workspace=.jj-ws/mem-alloc-result-impl.
