---
title: Reconcile the drifted ratchet suites
status: open
priority: 2
issue-type: task
created-at: "2026-08-03T22:11:21.664069+02:00"
---

Five scheduled ratchet suites are red on proofs (313cb1db), found by the workload-bars lane and verified standalone by the orchestrator: (1) tools/stdlib-manifest-test.f — 'manifest word missing from public signatures: lib/codegen.f CODEGEN:CONTENTS (ptr a -- ptr u8 n)'; (2) tools/trusted-inventory-test.f — asserts 13/16/18 expect 0, get 91/91/81 (TRUST-site counts drifted); (3) tools/primitive-effect-inventory-test.f — added rows with no committed identity for src/core/checker.f prim xt! (:5050) and the pprim checker-cert install (:5327) and checker-tape family install/arm/disarm/k-name/k-int/k-real (:5333-5342), plus one removed row (old checker-cert install identity); (4) tools/lint/text-foundation-test.f — assertion 696; (5) tools/examples-test.f — throw -2106. Cause: landings ran touched suites only, and these pins were never refreshed. Repair discipline: for EACH drift, first find the landed commit that introduced it (jj log/annotate) and confirm the change was reviewed and intentional; only then refresh the committed pin to match. Any drift that does NOT trace to a reviewed landing is a real regression — report it, do not refresh over it. All five suites green standalone and in the gate-stdlib fork at the end, plus the usual lints.
