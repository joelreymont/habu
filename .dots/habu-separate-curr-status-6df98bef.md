---
title: Separate current status from design history
status: open
priority: 3
issue-type: task
created-at: "2026-07-21T22:04:23.228179+02:00"
---

Current capability pages, future plans, architecture decisions, experiment journals, and worker transcripts are interleaved in the same long documents. A search result therefore cannot tell whether a claim is proposed, landed, superseded, or historical. Define three document classes with validated frontmatter: immutable decision records, generated current capability/status views, and archived experiment journals. Every live plan/design page must declare status, effective date, owning dot or landed revision, and superseded-by identity; every live source path is validated. Move operational journals and worker scratch evidence to archives without rewriting history, split unrelated GPU material out of type-family design, and generate current critical-path and capability pages from code/dot registries. MODEL-CAD and inference plans remain architecture inputs, not mutable execution journals. Add missing/invalid status, stale owner/revision, broken path, contradictory current entries, and archived-as-current mutations. Do not solve this with more disclaimer prose. Files: document schema/lint/renderer and staged doc migrations. Verify generated identity, link/path lints, host/dot lints, and full native gate.

CODE-REVIEW 2026-07-21 required a generated status view; that clause is retired with STATUS.md itself (habu-retire-status-md-0ae9b261) and no status ledger is to be reintroduced. Any generated capability view this dot produces must derive from the exact suite/trust/gate registries and the verified tree, so that mutating a registry changes the view or fails the gate; a date banner or a manually copied count is never evidence. False boundary descriptions retire through habu-verify-trusted-boundary-b5de1099 and the owning safety fixes.
