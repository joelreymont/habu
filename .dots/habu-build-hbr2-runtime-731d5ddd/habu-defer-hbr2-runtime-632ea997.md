---
title: Defer HBR2 RUNTIME and UI parts without a G0-G4 caller
status: open
priority: 3
issue-type: task
created-at: "2026-10-04T05:10:11.654720+03:00"
---

Problem: HBR2 specifies parts no viewer gate G0-G4 reaches (maki docs/viewer.md). Deferred, each with the caller that unblocks it: compaction (§3.1; publishes through the §3.3 rebase path over the changed-key metadata of commits since its base) when the 48 MiB page pool's tombstone share is measured; JOIN, RACE and BOUNDED-MAP (§5.2) when habu-build-hbr2-scene-972b5283's bundle streaming names one; the semantics record and UI:SEMANTICS (§7.2, §13.1, A.3) for the §13 accessibility gate, a release decision; Boolean and OptionIds binding wrappers (§9.1) for the first field of each kind; secondary indexes (§3.1) when a query's scan is measured too slow. Acceptance: each part lands as its own dot naming its caller. Files: none. Verify: none. Depends: none. Ownership: none. Lane: tim. Claim: unassigned.
