---
title: Avoid an extra DATA payload page
status: closed
priority: 2
issue-type: task
created-at: "2026-10-14T16:55:46.946322+03:00"
closed-at: "2026-10-21T10:04:32.118889+03:00"
close-reason: "Existing DATA_CONST placement (370481f1/fa53fd66, integrated3bdffeea) independently reviewed clear through current writers, object relinking, startup fixups and readers. Fresh same-source Dots a0b1e8f9 on Habu9ff01561/a30 host: identical155620 code bytes and11080 payload bytes; TEXT180224 to163840, DATA_CONST16384 unchanged, signature1684 to1556, file198396 to181884 (16512 saved). Control omits only placement call; production restored exactly. Both strict codesign checks and equal help/version runs pass. Existing focused payload/multipage/relink/reader cases and qualified native/convergence evidence retained; fresh independent paired-artifact review clear. No new engine or codec."
---

Otto’s macOS Dots image carries8,408 encoded DATA payload bytes in __TEXT; report suggests placement in an already-mapped writable region could avoid a16KiB file page. Inspect actual Mach-O segment protections, startup relocation/copy requirements and signing before selecting a layout. Preserve existing encoding; no new codec/compression, extra runtime dependency or unsafe executable/writable mapping. Acceptance: runnable real stripped app, standard Mach-O layout/signature validation and exact before-after file/page measure; focused writer E2E, required native/convergence gates and Astra review.
