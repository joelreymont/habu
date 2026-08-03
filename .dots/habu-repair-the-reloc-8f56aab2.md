---
title: Repair the reloc proof red
status: active
priority: 2
issue-type: task
created-at: "2026-08-03T22:11:21.683470+02:00"
---

test/compiler/reloc-proof.f dies with uncaught throw -6705 on proofs (313cb1db), red in the scheduled compiler-reloc-proof suite. Same pattern as the earlier storage-manifest miss: a landed change (likely the 128K-to-256K scratch bump or a later layout change) moved what the Rocq model formal/Common/Reloc.v and its habu-side pins agree on, without moving them together. Find the landed commit that broke it, decide whether the model or the pin is stale, move schema and model together as one reviewed change (the storage-manifest repair lane is the template), and prove the refreshed gate by mutation before trusting it. -6705 is in the reloc error region of lib/errors.f — decode it first.

Claim: agent=ratchet-repair workspace=.jj-ws/habu-reconcile-the-drifted-48eefbd9
