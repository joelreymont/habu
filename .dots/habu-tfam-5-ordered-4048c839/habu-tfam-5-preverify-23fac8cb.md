---
title: "TFAM 5: preverify/all-errors ordered-event redrive + support parity"
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T08:53:45.129143+02:00"
---

Replace path-set closure discovery with ordered-event replay in the consumers. verify-source.f preverify (RECORD-DEFINER? :373) and tools/check-core.f flatten (CHK-MATERIALIZE-FILE/LIST :494, CHK-SCAN-DEPS :426) must redrive each ORIGINAL source-list file per ordered event (stop flattening away original paths). all-errors (tools/check-all-errors-core.f CA-COLLECT-SUPPORT :427) is missing deftype/deflinear/value-record/immediate/EXPORT vs verify-source (asymmetry, census gap4); top-level TRUST replayed by all-errors but NOT verify-source RECORD-DEFINER? (gap5). Drive support replay from the shared ordered event log so package/family metadata loads before metadata-derived signatures and EXPORT rows. --all-errors --source-list replays all prior source-list entries before checking a later file. Do this via the event log, not ad-hoc line collection. Depends on event-log + discovery dots.
