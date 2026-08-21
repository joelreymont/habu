---
title: Intern compiler attributes
status: closed
priority: 1
issue-type: task
created-at: "2026-07-26T22:54:58.846831+02:00"
closed-at: "2026-08-15T14:07:28.837736+02:00"
close-reason: "Closed (vintage audit 2026-08-15, re-executed after the pool incident): attr interning (value-list reservation recorded, resolution dotted). Production-consumed by the native chain; suites dual-registered, green through the real entry."
---

Full context: design section 6.3 requires typed canonical attributes rather than byte/text conventions. Add closed attribute schemas, structural interning, target/numeric ownership, and deterministic reference identity. Acceptance: identical attributes intern; unknown kind, bad payload/type, illegal target, and cross-owner references reject; every attribute field participates in canonical identity. Dependency: compiler types.

Claim: agent=ir-attr workspace=.jj-ws/habu-intern-compiler-attrs-37cfbca5
