---
title: Avoid an extra DATA payload page
status: active
priority: 2
issue-type: task
created-at: "2026-10-14T16:55:46.946322+03:00"
---

Otto’s macOS Dots image carries8,408 encoded DATA payload bytes in __TEXT; report suggests placement in an already-mapped writable region could avoid a16KiB file page. Inspect actual Mach-O segment protections, startup relocation/copy requirements and signing before selecting a layout. Preserve existing encoding; no new codec/compression, extra runtime dependency or unsafe executable/writable mapping. Acceptance: runnable real stripped app, standard Mach-O layout/signature validation and exact before-after file/page measure; focused writer E2E, required native/convergence gates and Astra review.
