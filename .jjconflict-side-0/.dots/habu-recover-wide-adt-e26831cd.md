---
title: Recover wide-ADT LAYOUT-BUFFER memory-lowering campaign
status: open
priority: 2
issue-type: task
created-at: "2026-07-19T20:50:02.984528+02:00"
---

Forensic sweep 2026-07-19: four sibling directories habu-minion-doc-esJW, habu-minion-seal-UllK, habu-minion-audit-rRsm, habu-minion-bootstrap-wide (2026-07-11) hold the wide-ADT memory-lowering + LAYOUT-BUFFER checker campaign. Genuinely stranded: lib/layout-buffer.f and src/habu/layout-buffer.f are absent from master, its governing dots (checker-capability-typed a480c423, pin-wide-adt 31f1639c, doc-wide-adt c821deea) are absent from master, and the work survived only as unbookmarked heads. Now durably bookmarked AND pushed to origin: recover-wide-adt-layout (d53a2c9f, LAYOUT-BUFFER checked family-bound ADT storage), recover-wide-adt-seal (675699d7, pin wide ADT protected stores), recover-wide-adt-doc (9f82e86c, document wide ADT memory lowering), recover-wide-adt-checker (9a9cb627, seal layout-buffer checker authority); chain also includes lowering, lint-recognition, transaction-hardening, and provenance-seal commits. Partial sibling bookmark maki-layout-buffer-lints (502a1810, divergent) also not on master. Recovery: reconcile these heads against the active LAYOUT-BUFFER work under goal dot 7939a157, decide re-derive versus superseded per head with evidence, then land in reviewed slices; retire the four minion directories only after harvest.
