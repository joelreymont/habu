---
title: Shared generated-emission trust registry
status: open
priority: 2
issue-type: task
created-at: "2026-07-22T15:30:44.176626+02:00"
---

Why: trust-lint now accounts for the build-time TRUST emission (src/habu/habu2.f OWNER-WID-EMIT:EMIT-FINALIZE writes s" OWNER-WID:FINALIZE" s" --" TRUST into generated refresh source) via a tool-local, existence-gated registry, but the registry's effect string and line pointer are declared rather than cross-checked against the actual generator emission, the site is absent from the human TRUSTED.md audit ledger, and tools/trusted-inventory has both the same generated-emission blind spot and the same per-line cross-line scanner miss that trust-lint just fixed. Owned result: one shared generated-emission registry (package under tools/, e.g. TRUST-GEN) consumed by BOTH tools/trust-lint-core.f and the trusted-inventory scanner; a TRUSTED.md ledger row for OWNER-WID:FINALIZE keyed to the generator file; and a cross-check that parses the generator's actual emitted name/effect out of src/habu/habu2.f (structural parse of the emission site, not substring) so a drifted emission fails the lint. Acceptance: trust-lint and trusted-inventory -- strict both green on an unmodified tree; mutating the generator's emitted effect string in a fixture copy makes BOTH tools red; deleting the registry entry makes both red while the generator exists; repo counts stay finding-free. Owning gates: bin/hb --load tools/trust-lint-test.f, bin/hb --load tools/trusted-inventory.f -- strict. Depends: lands after habu-trust-lint-token-d522dbbc merges. Files: tools/trust-lint-core.f, trusted-inventory sources, TRUSTED.md, new shared registry file.
