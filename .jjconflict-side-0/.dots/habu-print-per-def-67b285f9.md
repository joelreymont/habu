---
title: Print per-definition rows from the census CLI
status: open
priority: 2
issue-type: task
created-at: "2026-08-14T04:23:19.779883+02:00"
---

Found by the singletons diagnosis: tools/chain-census.f prints the histogram but no per-definition rows, so a bucket of one cannot be traced without writing a private driver over chain-census-core.f every time (three lanes have now done so). Add a rows section to the CLI output (code, name, file per refused definition) with a fixture in tools/chain-census-test-lib.f. Files: tools/chain-census.f, tools/chain-census-test-lib.f. Depends: none.
