---
title: Convert split-str batch plain-33
status: open
priority: 2
issue-type: task
created-at: "2026-11-25T11:04:05.001140+01:00"
blocks:
  - habu-say-str-in-c336fe7d
---

Problem: the 11 LSP files of /tmp/jerry-p1/unmark/b-plain-33.txt still carry the split-str marker (Joel 2026-11-23). Their JSON:READ-STR calls move to vectors in the JSON reader lane first. Acceptance: no split-str line in them; each checks as on master; lsp-boundary passes. Depends: habu-say-str-in-c336fe7d. Claim: unassigned.
