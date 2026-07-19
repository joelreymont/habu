---
title: Package remaining 30 flat stdlib modules
status: open
priority: 2
issue-type: task
created-at: "2026-07-19T21:16:49.430874+02:00"
---

Follow-up to habu-pkg-stdlib-modules-3197dbbc (landed 98dba35e): 30 flat lib modules still lack package scope: argv, array, build, codesign, content-key, date, engine-id, errors, ffi, fmath, fmt, fs-mutate, fs, hashmap, json-write, map, prelude, process-argv, process-command, process-cwd, process-fork, property, render, report, sort, source, stats, table, test, time. Package them in dependency order in small reviewed batches; every migrated module needs all call sites updated, its manifest rows regenerated (lib/std.manifest via tools/public-signatures.f - note it only emits public-SECTION definitions, not EXPORT-from-private), its tests green, and the bare-load gate (test/stdlib-standalone-load.f) still green. Watch the two seams found during the first slice: tools/bundle-lib.f source-concatenation needs the provided-marks path for any newly-required module, and modules loaded by the docs/bootstrap.md seed prelude (errors, string, memory, fs, fs-mutate, process, process-argv, process-env, codesign) must be verified against that exact prelude before their batch lands - if packaging one would disturb the seed path, split it into its own seed-verified batch. prelude and errors are the riskiest; do them last as their own slices.
