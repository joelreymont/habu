---
title: Separate current status from design history
status: closed
priority: 3
issue-type: task
created-at: "2026-07-21T22:04:23.228179+02:00"
closed-at: "2026-08-02T15:53:49.850393+02:00"
close-reason: "Obsolete: STATUS.md is deleted and no generated status replacement is wanted."
---

Current capability pages, future plans, architecture decisions, experiment journals, and worker transcripts are interleaved in the same long documents. A search result therefore cannot tell whether a claim is proposed, landed, superseded, or historical. Define three document classes with validated frontmatter: immutable decision records, generated current capability/status views, and archived experiment journals. Every live plan/design page must declare status, effective date, owning dot or landed revision, and superseded-by identity; every live source path is validated. Move operational journals and worker scratch evidence to archives without rewriting history, split unrelated GPU material out of type-family design, and generate current critical-path and capability pages from code/dot registries. MODEL-CAD and inference plans remain architecture inputs, not mutable execution journals. Add missing/invalid status, stale owner/revision, broken path, contradictory current entries, and archived-as-current mutations. Do not solve this with more disclaimer prose. Files: document schema/lint/renderer and staged doc migrations. Verify generated identity, link/path lints, host/filemap/dot lints, and full native gate.

CODE-REVIEW 2026-07-21 requires STATUS.md facts such as suite counts, gate coverage, engine identity, trust census, and last verified revision to be generated from the exact suite/trust/gate registries and verified tree. A date banner or manually copied count is not evidence. Mutating a registry must change the generated status view or fail the gate; changing only STATUS.md must also fail. FILEMAP generation is separately owned by habu-generate-filemap-md-84e85083, and false boundary descriptions retire through habu-verify-trusted-boundary-b5de1099 and the owning safety fixes.
