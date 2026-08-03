---
title: Generate manifest columns from scanner
status: closed
priority: 2
issue-type: task
created-at: "2026-07-27T10:33:57.118380+02:00"
closed-at: "2026-08-04T00:22:52.671083+02:00"
close-reason: ledger retired with the governance mirror
blocks:
  - habu-pkg-public-signatures-e25db8b1
---

lib/std.manifest duplicates two machine-derivable columns (word, effect) by hand, so every future public word in lib/ reintroduces stdlib-manifest-test drift at a later, more expensive moment - proven 2026-07-27 when four VEC: rows went missing for days and were found by a worker on an unrelated dot. Owned result: a checked Habu refresh tool (bin/hb --load, package-owned, in tools/) that runs the public-signatures scanner over the manifest's own kind=module rows and rewrites ONLY the word and effect columns from that emission, preserving the human-owned columns (notes, status, owner, test, doc, gate) byte-for-byte, plus a --check mode that exits nonzero on any derivable-column drift so it can join a gate list. It must assert row anchors and refuse unknown or ambiguous rows rather than guessing. This removes the failure mode instead of repairing instances; stdlib-manifest-test remains the independent cross-check.

Dependency, stated as dot identifiers rather than commit identifiers (2026-07-27 correction): this tool calls the public-signatures scanner, so the scanner must first be a package with a stable named surface instead of a wall of PS- globals. Two dots claimed that packaging, and the overlap was ruled on 2026-07-27: habu-pkg-public-signatures-e25db8b1 owns it, because it is the one that actually delivered the work, as lane commit 97642e1c. It is recorded above as the blocker. The other claimant, habu-pkg-checker-tools-fe04934e, has been reduced to packaging tools/check-core.f into CHECK-CLI and no longer owns this file. Note that 97642e1c is in the vecmem lane and not yet on master, so this leaf waits on that lane's integration.

Acceptance: on a tree with a deliberately drifted effect column, --check exits nonzero naming the row, refresh restores it byte-identically to the scanner emission while leaving human columns untouched (proven by diff), stdlib-manifest-test green before and after, both diff lints.
