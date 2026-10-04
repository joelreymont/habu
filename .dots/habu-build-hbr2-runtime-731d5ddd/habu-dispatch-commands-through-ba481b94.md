---
title: Dispatch commands through one registry
status: open
priority: 2
issue-type: task
created-at: "2026-10-04T05:10:11.471170+03:00"
---

Problem: HBR2 §18.1 mediates every execution (widgets, shortcuts, tests, AI) through one registry whose descriptors carry a stable Id128, schema version, argument and result schemas, permission class, preview policy, required entity classes, admission profile, inverse policy and automation exposure, with pure VALIDATE and PROJECT hooks; §7.4 rechecks availability at dispatch, and §3.3 queues effects at publication. Acceptance: in package RT-CMD, a registry of descriptors keyed by Id128, closed once sealed; every mutation, component-local state included, enters here (D7); dispatch takes a command and its immutable arguments, runs VALIDATE (Valid with a readset, Invalid with errors, NeedsData with IDs) and PROJECT (CandidateWrites or Deferred) against the snapshot of a habu-commit-txns-through-17bdbdab transaction, and publishes; an unknown command or schema version is refused before any write; command IDs are independent of wire opcodes (§27.4); REBASE, CANONICALIZE, INVERSE and REDACT stay with SYNC (habu-add-a-srv-845f35e8). Files: lib/runtime/command.f (new, package RT-CMD; mints E-RT-CMD-FIRST/LAST -9610..-9619 in its owning file), lib/errors.f (one comment line), test/browser/command-test.f (new), test/gate-stdlib-cases.f. Verify: bin/hb --load test/browser/command-test.f: Invalid and NeedsData publish nothing; a command whose readset changed before publication restarts; an unknown ID is refused by code; bin/hb --load test/run.f. Depends: habu-commit-txns-through-17bdbdab. Ownership: lib/runtime/command.f. Lane: tim. Claim: unassigned.
