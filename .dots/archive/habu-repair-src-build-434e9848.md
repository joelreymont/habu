---
title: Repair source build gate refusals
status: closed
priority: 2
issue-type: task
created-at: "2026-10-21T11:20:33.210967+03:00"
closed-at: "2026-10-21T11:45:17.592973+03:00"
close-reason: Verifier library-definer recognition now uses literal name plus relocatable code identity instead of mutable dictionary indexes across rewinds. Independent Astra review clear. Source and sandbox build gates, storage checks and restored native REPL verifier pass; complete matching native suite 486/486. Plain stripped verifier persistence remains unsupported and was not claimed.
---

Final incremental candidate3782448f/948295de passes real Maki cold/warm/fresh equality but full native gate has build-fixpoint-source and build-fixpoint-sandbox failures. Source phase generated stage2 child rejects78 and parent throws E-BUILD-CERTIFY(-2805); sandbox stale-crash diagnostic assertions3/4 fail. Reduce current final source through real source/sandbox rows, preserve exact diagnostics, fix the responsible source assembly/verification or fixture boundary without relaxing rejection/crash/install guarantees. Existing red rows are acceptance; independent review and focused matching execution required. No competing core/cache implementation or public promotion.
