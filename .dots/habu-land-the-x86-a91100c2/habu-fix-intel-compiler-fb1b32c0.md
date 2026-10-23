---
title: Fix Intel compiler image construction
status: active
priority: 2
issue-type: task
created-at: "2026-10-23T11:35:27.342292+00:00"
---

Pulsar owner: images worker in .jj-ws.noindex/pulsar-intel-images, based on intel 184663fe. compiler-images-build throws CODE-RECLAIM:E-LIVE (7179) after compiled, leaving images missing and 45 exit-75 consumers blocked. Fix source, preserve master tests, verify produced images and dependent rows. Shared-code ARM checks remain open for the Mac. Independent review before intel landing.

First error reduced: bundle white capture raises E-LAYOUT-BOUNDS (7122) by running names before x86 placement; outer recovery masks it with E-LIVE. Ordinary native build passes. Reusing x86 held writer reveals a second real bundle issue: host-check declarations after target NCOMP installation publish through a fresh idle target NSHADOW, so declared code-cell capture finds no shipped shadow entry. Fix existing publication/observer seam; keep code-cell checks intact. Shared native-build-core/capture changes need Mac ARM check OPEN.
