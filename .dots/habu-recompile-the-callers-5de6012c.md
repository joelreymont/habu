---
title: Recompile the callers that copied a migrated body
status: open
priority: 3
issue-type: task
created-at: "2026-08-04T09:46:56.687688+02:00"
---

Full context: src/habu/habu2.f C-CALL copies a callee's body into its caller when the body is at most INL-MAX bytes and holds no pc-relative branch, so such a caller holds no call instruction and src/compiler/native/reach.f has nothing to move: REDIRECT refuses with E-NREACH-NONE, which is the honest answer and not a fix. Measured on the live image: TAG and PAY are 56 bytes each, copied, zero call sites. The only way to reach those callers is to recompile them - to re-elaborate each caller through the chain now that the callee's record holds the new code - which is a capability the chain does not have: it can migrate a definition whose SOURCE it is handed, and a caller in the image has no source in the process. Needed: either a migration that keeps a definition's source with its record, or a re-emission path that rebuilds a copied span in place. Depends: src/compiler/native/reach.f, src/compiler/native/migrate.f, src/habu/habu2.f C-CALL. Ownership: unassigned. Claim: unassigned.
