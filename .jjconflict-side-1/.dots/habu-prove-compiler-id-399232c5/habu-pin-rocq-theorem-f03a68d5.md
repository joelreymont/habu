---
title: Pin Rocq theorem statements, not just names
status: open
priority: 1
issue-type: task
created-at: "2026-07-28T13:23:17.856863+02:00"
---

Full context: test/compiler/ir-id-proof.f binds the compiler identity implementation to the Rocq proofs and is proven non-vacuous by twenty mutations plus an independent check. It has one real hole its author named. The gate binds theorem NAMES and their assumption sets against a committed manifest, but not the theorem STATEMENTS. Rewriting pure_run_unique in formal/Common/IdAllocatorLaws.v to state something trivial keeps the name, keeps its closed-under-the-global-context status, and passes the gate. The definitions are safe - 68 vector rows plus the constant rows execute Ids.* and IdAllocator.* directly - so this is specifically the lemma statements. Required result: each manifest row pins its theorem's TYPE, not only its name. Emit a Check obligation per theorem into the generated Rocq file so the compiler refuses a changed statement, or digest the statement types and compare against the committed manifest. Acceptance: rewriting any bound theorem to a weaker or trivial statement fails the gate, demonstrated by mutation; the existing twenty mutations still fail; the gate stays green unmutated. This is the highest-value follow-up on the parity gate.
