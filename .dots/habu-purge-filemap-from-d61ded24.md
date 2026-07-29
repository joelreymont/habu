---
title: Purge FILEMAP from live dots
status: active
priority: 2
issue-type: task
created-at: "2026-07-28T10:47:08.487241+02:00"
---

Why: open and active dot contracts must not require a deleted file or gate. Owned result: in dots whose frontmatter status is open or active, remove FILEMAP.md from write sets and remove filemap-lint or filemap gate tokens from verification lists while preserving every other sentence, file, gate, dependency, and acceptance rule. Do not touch closed dot history. Close these obsolete open dots with an explicit user-directed removal reason: habu-deduplicate-filemap-existence-e1da224d, habu-enforce-one-filemap-3d42ccbe, habu-generate-filemap-md-84e85083, habu-pkg-filemap-lint-5d7baf5c, and habu-derive-census-walk-78867ab0. Remove blocker edges to those retired dots; add no replacement inventory or walk-completeness task. Forbidden: status changes to any other dot, prose redesign, dependency changes unrelated to the five retired dots, source/docs edits, or bulk changes to closed history. Checkpoint: record the exact open/active file set and references before mutation; the native dot gate is green. Acceptance: no open or active dot contains FILEMAP.md, filemap-lint, or a blocker to a retired filemap dot; the five named dots are closed with no active copy; all other statuses and blocker edges are byte-identical; native dot-dep-lint reports zero findings. Ownership: live task metadata migration only.

Claim: RELEASED 2026-07-29 by the stale-claim audit. Agent `codex-filemap-dots` and workspace `.jj-ws/habu-purge-filemap-from-d61ded24` are both gone: the directory does not exist and `jj workspace list` has no record of it. The work has not landed - the five dots it was to retire are still `status: open` and more than forty live dots still cite `FILEMAP.md` or `filemap-lint`. The dot stays active and is free to claim.
