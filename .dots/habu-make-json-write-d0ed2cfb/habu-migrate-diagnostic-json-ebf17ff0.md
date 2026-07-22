---
title: Migrate diagnostic JSON writer
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T16:07:53.567835+02:00"
blocks:
  - habu-build-explicit-json-399f5929
---

Why: DIAG:RENDER-JSON uses the deleted singleton and returns a span valid only until the next render. Exact interface: RENDER-JSON consumes and returns a caller-supplied JSON-WRITE:writer; every private JSON helper threads that writer explicitly while the immutable diagnostic remains an input; no DIAG-owned JSON buffer or raw JSON span remains. Human RENDER is unchanged. Acceptance: canonical JSON stays byte-identical; two diagnostics render interleaved without state leakage; caller COPY refusal preserves the writer and destination; diagnostic-render owning suite passes. Smallest check: the existing diagnostic render suite through bin/hb. Depends: Build explicit JSON writer core. Ownership: maki/db/diagnostic-render.f, its owning tests, FILEMAP.md. Claim: unassigned.
