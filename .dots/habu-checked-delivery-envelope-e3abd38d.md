---
title: Checked delivery-envelope generator tool
status: open
priority: 2
issue-type: task
created-at: "2026-07-22T15:30:44.197820+02:00"
---

Why: the cross-agent integration protocol on this repo now requires every lane delivery to carry a six-line envelope (commit / parent / write-set / focused-gates / reviewer-verdict / engine-sha); today it is hand-typed prose, which drifts and cannot be validated mechanically. Owned result: tools/delivery-envelope.f, a checked Habu tool (package DELIV) run by bin/hb from a workspace root that GENERATES the envelope (commit and parent from jj via the sanctioned process boundary, write-set from the commit diff, engine-sha by hashing bin/hb with the baked SHA256-FILE-HEX) with gate results supplied as arguments, and VALIDATES a pasted envelope against the workspace (commit exists, parent matches, write-set matches the diff exactly — extra or missing files fail). Acceptance: generation on a real lane workspace reproduces the actual commit facts byte-for-byte; validation rejects a wrong parent, a write-set omission, and a stale engine sha in fixtures; hostile fixtures prove reordered or duplicated lines cannot pass. Owning gate: new tools/delivery-envelope-test.f via bin/hb, hermetic. Depends: none. Files: tools/delivery-envelope.f, tools/delivery-envelope-test.f, FILEMAP.md rows.
