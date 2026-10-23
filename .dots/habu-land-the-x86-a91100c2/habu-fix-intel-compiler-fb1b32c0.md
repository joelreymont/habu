---
title: Fix Intel compiler image construction
status: active
priority: 2
issue-type: task
created-at: "2026-10-23T11:35:27.342292+00:00"
---

Pulsar owner: images worker in .jj-ws.noindex/pulsar-intel-images, based on intel 184663fe. compiler-images-build throws CODE-RECLAIM:E-LIVE (7179) after compiled, leaving images missing and 45 exit-75 consumers blocked. Fix source, preserve master tests, verify produced images and dependent rows. Shared-code ARM checks remain open for the Mac. Independent review before intel landing.
