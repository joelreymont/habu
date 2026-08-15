---
title: Own compiler IR context
status: closed
priority: 1
issue-type: task
created-at: "\"2026-07-26T22:54:38.107827+02:00\""
closed-at: "2026-08-15T14:07:28.866184+02:00"
close-reason: "Closed (vintage audit 2026-08-15, re-executed after the pool incident): context (serial/stale/ceiling negatives). Production-consumed by the native chain; suites dual-registered, green through the real entry."
blocks:
  - habu-bind-compiler-target-b3dfa307
---

Full context: design section 6.2 and PLAN.md lifecycle require one explicit owner for target/numeric policy, module serials, sources, diagnostics, scratch, modules, witnesses, and metrics. Implement context creation, nonzero monotonic serial allocation with exhaustion, stale-handle registry, and total teardown over the repository owned-release contract. Acceptance: serials never reuse; failure returns the owner; stale/double use rejects; teardown releases every live child. Dependencies: target policy (landed: src/compiler/digest.f, target.f, numeric-policy.f, binding.f). The earlier dependency on habu-make-owned-release-79de2b5c was investigated and removed: the fatal flip changes only the failure mode of a failed whole-range release (same stack effect), so context code written against the frozen disposal contract (straight-line release, no catch around it, no cleanup coordinators, no retained-state guards) is identical before and after that flip. Frozen interface constraint: context ownership is quotation-scoped over MEM:WITH-BYTES; the context must add no new direct MEM:RELEASE-BYTES call sites, originally to protect the frozen rename diff (that evidence has since been lost, see habu-rename-owned-release-5736ed92), and retained because quotation-scoped ownership is the structurally correct design regardless.

Claim: agent=ir-ctx workspace=.jj-ws/habu-own-compiler-ir-9d25246b
