---
title: Give the language a pointer-valued constant
status: open
priority: 3
issue-type: task
created-at: "2026-09-18T03:36:35.212106+03:00"
---

Problem: a pointer constant is a raw constant cell, which the raw-storage rule does not read as an address, so a set-once pointer is written as 'PTR-VARIABLE POOL-A' plus ': POOL ( -- ptr u8 ) POOL-A @ ;', an accessor where a constant was, because no checked pointer-valued constant form exists. The cited site, test/engine-stack-lifecycle.f, has had no POOL since c9ee0d90d91. Acceptance: a declared form for an immutable pointer value (PTR-CONSTANT NAME with a chosen pointee, or constant gaining a typed spelling) that compiles to the literal like constant does, rejects a store, and certifies as the declared pointer; any src/lib or test site of the shape converted back; a rejected-program fixture for the store. Files: src/core/pointer-storage.f or the constant definer, test/pointer-storage-test.f. Verify: the fixture; the suite; fixpoint; test/run.f. Depends: habu-reject-a-ptr-5ad2734e. Ownership: declared storage. Claim: unassigned.
