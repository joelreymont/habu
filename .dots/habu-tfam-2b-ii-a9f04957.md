---
title: "TFAM 2b-ii: sealed system packages via CHECKER-PACKAGE guard"
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T02:07:47.905380+02:00"
---

Sealing slice 2. Guard point validated: CHECKER-PACKAGE (checker.f ~3461) already case-folds (CHECKER-PACKAGE-COPY/CHECKER-FOLD-C) and C-PACKAGE (habu2.f ~2951) calls it BEFORE C-PACKAGE-ENSURE allocates WIDs, so rejecting there is before-mutation. Reject user-source 'package TFAM/TYPE/MATCH' (case-insensitive) unless friend latch on; friend path fixtures prove engine load still creates/reopens them. Needs 2b-i latch. Depends: 2b-i.
