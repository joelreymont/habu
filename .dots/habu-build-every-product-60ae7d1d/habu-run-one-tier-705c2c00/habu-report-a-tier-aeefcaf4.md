---
title: Report a tier-1 body that ends inside a structure as a control fault
status: closed
priority: 2
issue-type: task
created-at: "2026-10-30T19:32:20.930052+02:00"
closed-at: "2026-11-04T11:48:49.826258+02:00"
close-reason: "Census 2026-11-04: goes with habu-delete-tier-1-f5c44fb4, T0 compiling every target, tier 1 and the JIT kept only for experiments. Tier-1 refusal defect at 1 set-tier; T0 already refuses as wanted (dot text)."
---

Problem: at 1 set-tier, a TRUSTED: or 0 set-check body ending inside a structure or quotation (TRUSTED: RXSF ( -- ) 0 if ; / [: 1 ; / gekt-semi) dies E-NCOMP-BINDING -8575 uncaught rc 67 (raised through checker-owner-abi BINDING-RC) before tier 1's skeleton end check (elaborate.f SKELETON ~:2713) reports E-NELAB-CTRL; errors.f says E-NELAB-CTRL covers a body that ends with a structure open. Repro /tmp/jerry-big/gaps/{rxsf,rxsc,gekt-semi}-t1.f. Acceptance: tier 1 refuses as T0 does after lane ctrl (rc 70, ncomp line with token). Claim: unassigned.
