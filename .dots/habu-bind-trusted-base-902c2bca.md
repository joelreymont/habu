---
title: Bind trusted-base rows to live retiring dots
status: open
priority: 2
issue-type: task
created-at: "2026-07-25T14:42:11.641589+02:00"
---

Why this is needed: every TRUST row is a soundness cliff that exists because the checker cannot yet express something, and the repository rule is that the missing capability must be tracked by a dot and the trusted row removed when that capability lands. TRUSTED.md carries an Owner column for exactly that purpose, but nothing checks it, so the column has quietly rotted.

Measured on master 79c50e5a9dbf: TRUSTED.md holds 865 rows citing 60 distinct owner dot identifiers. Twelve of those identifiers, covering 14 rows, name dots that no longer exist anywhere in .dots. Another 14 identifiers, covering 196 rows, name dots whose status is closed. So 210 rows record a retiring capability that is either untracked or already declared finished while the trusted row is still in the trusted base. tools/trust-lint-core.f validates row shape and test citations but never resolves the Owner column against the tracker.

Owned result: the trust gate resolves every Owner value against the real tracker and fails closed on a value it cannot resolve. The rule must be aware of the row's Class, because the classes mean different things: prim-axiom and discharge-candidate rows describe a capability the checker is missing and must cite a dot that is still open, since a closed owner means either the row should have been retired or the capability was never delivered; stdlib-boundary, builder-emit and test-metaprog rows record a boundary that is expected to persist, so for those the owner must merely exist. Decide and write down that mapping in the TRUSTED.md header next to the existing Class explanation, then enforce it.

Do not fix the 210 rows by inventing owner values. Each unresolved row needs a real decision: retire the row because the capability landed, repoint it at the dot that actually owns the remaining work, or open a new dot for the capability that is still missing. Record that decision per owner identifier, not per row.

Forbidden: an allowlist of grandfathered rows, a warning that does not fail the gate, or a placeholder dot created only to satisfy the check.

Acceptance and smallest owning check: before the change, the gate passes on today's TRUSTED.md; after the change, a fixture row citing a dot identifier that does not exist is rejected with a named diagnostic, and a prim-axiom fixture row citing a closed dot is rejected as well, while a stdlib-boundary row citing that same closed dot is accepted. The real TRUSTED.md then passes with every one of the 210 rows resolved by a recorded decision. A mutation that removes the owner resolution must red the fixtures.

Verify: bin/hb --load tools/trust-lint-test.f, tools/trust-lint.f repository mode on the real tree, tools/trusted-inventory-test.f, bin/hb --load tools/dot-dep-lint.f, typed-local-diff-lint and package-diff-lint on the exact diff, host-lint and filemap-lint.

Files: tools/trust-lint-core.f, tools/trust-lint-test.f, TRUSTED.md, plus any dot additions the per-owner decisions require. Claim: unassigned.
