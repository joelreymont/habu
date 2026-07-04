---
title: standalone trust-lint file buffer smaller than GSI path
status: open
priority: 3
issue-type: task
created-at: "2026-07-05T00:05:00.000000+02:00"
---

Found 2026-07-05 while making the standalone lint-tools slice loadable
(habu-gate-stdlib-standalone-098d7f57). Once `bin/hb --load test/gate-stdlib.f
-- lint-tools` loads, its `TEST:SUITE trust-lint` (spawns bin/hb --load
tools/trust-lint.f) fails `lint: file exceeds buffer`: tools/trust-lint.f uses
TL-ARGV-FILE-CAP $30000 (192KB), but a scanned core source (src/core/checker.f)
now exceeds it. The resident GSI path is green because GSI-TRUST-LINT installs
larger buffers via TRUST-LINT-BUFFERS! (GSI-TL-STR-CAP $80000 / GSI-TL-FILE-CAP
$40000 in test/gate-stdlib-inline-lib.f). Fix: raise tools/trust-lint.f
TL-ARGV-FILE-CAP (and TL-ARGV-STR-CAP if needed) to match the GSI caps so the
standalone trust-lint suite tracks the same src/core growth watermark. Reads
stay fail-closed (READ-FILE dies on overflow), so this is a loud red, never
truncation. Owner overlap: same file family / dual-path as
habu-gate-case-lint-06257524.
