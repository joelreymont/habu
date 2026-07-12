---
title: "checker: JSON family hint field is bare tail while expected/actual are qualified"
status: open
priority: 2
issue-type: task
created-at: "2026-07-13T00:17:44.392611+02:00"
---

Diag-renderer review observation (2026-07-13, pre-existing, render.f:672 not in that diff): the E-MISMATCH JSON 'family' hint renders the bare tail via TFAM-NAME$ ('family':'region') while expected/actual now render qualified folded ('cad-kind:region<>'). A repair agent keying on the bare field for a foreign-package family gets an unresolvable name. Fix: qualify the family hint with the same FAM-NAME-REND path (folded pkg:tail for foreign packages), pin in the JSON-contract suite (gate-diagnostics-lib), and check repair-agent consumers (maki eval harness repair loop reads which fields?). Small; after the diag-renderer commit lands (same file).
