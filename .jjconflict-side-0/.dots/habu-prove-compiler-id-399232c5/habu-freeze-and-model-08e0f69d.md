---
title: Freeze and model NEW-MODULE issuance
status: open
priority: 1
issue-type: task
created-at: "2026-07-28T16:00:36.031458+02:00"
---

Full context: destruction review finding 1, HIGH, independently verified twice. NEW-MODULE (src/compiler/ir/id.f:113) is the only public issuer of module identities, and it is neither frozen as a token run nor modelled in Rocq. Mutating it to 'TAKE-SERIAL 1000 mod 1+ dup MINT-KEY swap MINT-MODULE' makes module serials cycle with period 1000 - falsifying the headline uniqueness theorems pure_run_unique and host_run_unique for the shipped API - while test/compiler/ir-id-proof.f, ir-id.f and ir-id-manifest.f all stay exit 0. A driver minting 1001 modules printed DUPLICATE MODULE IDS. The allocator proof is about a counter; the shipped API is about identities; the step from one to the other is exactly NEW-MODULE and no gate reads it. Required result: close the unpinned link. Freeze NEW-MODULE's body as an exact token run alongside SERIAL-NEXT/TRY-SERIAL/TAKE-SERIAL in the parity gate's frozen set, AND give it a Rocq counterpart the generated obligations exercise (issuance = attempt loop until Issued, minting key and id from the same serial), so both the textual and the semantic side pin it. Acceptance: the period-1000 mutation above fails the parity gate; a mutation minting key and id from different serials fails; the gate stays green unmutated; the twenty existing mutations still fail. Silently exploitable today - this outranks the other parity follow-ups.
