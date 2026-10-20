---
title: Sweep declared capture cells once
status: active
priority: 2
issue-type: task
created-at: "2026-10-16T15:22:55.948216+03:00"
---

Measured same-host warm self-build: ACAP-NEXT-CELL spends3.196 thread-CPU seconds of DATAcopy3.308, visiting15339 declared rows separately for15328gaps (235116192rowvisits); real byte/cell scan takes0.106. Build one capture-local sorted numeric offset column from eligible tagged XTOFF rows below DATA length, then sweep once with the existing byte cursor, preserving minimum-selection semantics for duplicates/overlaps and all raw-pointer refusals. Reuse SORT and existing finally cleanup; do not mutate XTOFF row order/metadata or add ABI/format/generalinterval machinery. Extend actual named-cell capture/write/boot fixture with reverse-address declarations and nonempty gaps before implementation. Match old bitmap/values/XTOFF and scanned-cell count; measure collection/sort+sweep and total copy against3.308CPU (target below0.33, no permanent timing assertion). Independent Astra review, focused capture E2Es, final native/convergence required.
