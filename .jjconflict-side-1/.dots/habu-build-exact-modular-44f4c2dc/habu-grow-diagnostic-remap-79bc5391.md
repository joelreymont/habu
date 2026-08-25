---
title: Grow diagnostic remap storage
status: closed
priority: 1
issue-type: task
created-at: "2026-07-15T23:50:06.103541+02:00"
closed-at: "2026-07-21T22:08:46.764660+02:00"
close-reason: Superseded because DIAG-REMAP is deleted with flat source composition. Any residual process-capture capacity belongs to the HB-BUILD maker/cache split and must use owned buffers there.
---

Full context: DIAG-REMAP rejects valid diagnostics above 64 KiB and decoded keys above 1 KiB, while HB-BUILD capture buffers impose the same truncation. Replace decode, key, diagnostic capture, patch, and output storage with owned growable buffers and checked vectors; no protocol-size ceiling except arithmetic or OS failure. Preserve unknown JSON bytes exactly. Acceptance: long keys/values, more than six patchable fields only where schema permits, diagnostics above 64 KiB, stdout plus stderr remap, and repeated reuse pass without mapping growth or truncation. Files: tools/diag-remap.f/test, HB-BUILD capture concern after package split, shared process capture only if measured ownership requires it.
