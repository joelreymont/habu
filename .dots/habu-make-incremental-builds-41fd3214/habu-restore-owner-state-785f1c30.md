---
title: Restore owner state on whole-file import
status: open
priority: 1
issue-type: task
created-at: "2026-10-22T17:12:02.942985+03:00"
blocks:
  - habu-key-and-check-9cad1739
---

Problem: the first whole-file replay copied owner data raw (checker FV without its pools, counter high-water without handles, TASK OWNED-USER-NEXT omitted, scope trailers decoded against the wrong range, CWORK offsets stored). Acceptance: each owner exports at file end and restores at import through its own interface: NCOMP:REPLAY-DEFINITION deltas in file order, CHECKER-UNIT export/import, NSTR literal prepare, TASK:OWNED-USER-NEXT, require rows, exit scope; CWORK excluded; does> definers and created-word data import (DOES-BYTE@ refusal gone). Verify: native-file-forms-*, task-state rows, a does> fixture, native-file-auto e2e. Design: docs/package-build.md "Keys and validity"; working notes ~/.cache/tmp/carl-file/design.md. Depends: container dot. Claim: unassigned.
