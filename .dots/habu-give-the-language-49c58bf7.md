---
title: Give the language a pointer-valued constant
status: open
priority: 3
issue-type: task
created-at: "2026-09-18T03:36:35.212106+03:00"
---

Problem: a pointer constant ('POOL' in test/engine-stack-lifecycle.f, 'BWM-POOL' in test/bootstrap-wide-memory-src.f) is a raw constant cell, which the raw-storage rule refuses to read as an address; the test lane (2026-09-18) had to turn each into 'PTR-VARIABLE POOL-A' plus ': POOL ( -- ptr u8 ) POOL-A @ ;', an accessor where a constant was, because no checked pointer-valued constant form exists. Acceptance: a declared form for an immutable pointer value (PTR-CONSTANT NAME with a chosen pointee, or constant gaining a typed spelling) that compiles to the literal like constant does, refuses a store, and certifies as the declared pointer; the two test sites and any src/lib site of the shape converted back; a rejected-program fixture for the store. Files: src/core/pointer-storage.f or the constant definer, test/engine-stack-lifecycle.f, test/bootstrap-wide-memory-src.f, test/pointer-storage-test.f. Verify: the fixture; the two suites; fixpoint; test/run.f. Depends: habu-refuse-a-ptr-5ad2734e. Ownership: declared storage. Claim: unassigned.
