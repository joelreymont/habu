---
title: Store structure destructure policy
status: active
priority: 2
issue-type: task
created-at: "2026-07-26T20:49:46.951093+02:00"
---

Why: the later owner-only structure work needs one canonical place in each
type-family row to store its destructure policy. This leaf adds representation,
not authority. No parser, checker, generator, compiler, or runtime consumer may
act on the value until the later sealing and lowering leaves make the boundary
enforceable.

Owner and representation: the type-family registry appends one scalar policy
cell to its canonical row. Zero means invalid or unset; `PUBLIC=1` and
`OWNER=2`. Every family initializer writes `PUBLIC`; reading zero or any other
code fails closed with a destructure-policy error. Do not reuse a spare bit,
parallel table, manifest, or default-on-read fallback.

Mutation contract: policy can change only on a live provisional family owned by
the current declaration rollback frame. A frame records the lowest family id it
may still mutate. When a child frame commits, the parent advances that lower
bound so it cannot alter the committed child. Calls without a frame, calls on
outer or committed rows, and invalid codes fail with distinct policy and
ownership errors. The policy is immutable after publication.

Rollback and persistence: rollback zeroes every retired family row before
lowering the high-water mark. Growth and direct snapshot persistence copy the
complete row, including the last live row at the capacity boundary. Dead
capacity and an abandoned boot arena are zero before persistence. The existing
record stride remains the sole copy authority.

Write set: `src/core/type-family.f`, `test/type-family-suite.f`,
`test/type-family-rollback-suite.f`, and `TRUSTED.md` only. The policy query and
setter remain internal dormant registry plumbing. This leaf must not claim that
they are a security boundary and must not add source syntax, package sealing,
destructure lowering, generated-word behavior, a consumer, or rebuilt-image
proof. Rebuilt-image publication remains owned by
`habu-prove-destructure-policy-e85db290`.

Acceptance: production registry fixtures prove initialized `PUBLIC`, zero and
invalid-code rejection, frame-owned mutation, child-to-parent authority
handoff, sibling and outer-row rejection, publication immutability, exact
rollback bytes, retired-row zeroing, growth preservation, complete last-row
copy, direct snapshot persistence, dead-capacity zeroing, and boot-arena
zeroing. One-cell-short growth and persistence mutations must fail. The
existing type-family and rollback suites, exact diff lints, trust inventory,
error-code lint, and native engine gate pass.

Rejected commits `5fa4a6763dab` and `321b9fb1bdd0` are evidence only. A revision
must correct their fail-open zero encoding, parent-frame write authority, and
boundary-row proof gaps rather than preserving them.

Claim: agent=codex-destruct-registry workspace=.jj-ws/habu-store-structure-destructure-8c20c92a
