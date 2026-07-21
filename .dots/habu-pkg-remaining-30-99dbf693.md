---
title: Package remaining 30 flat stdlib modules
status: open
priority: 2
issue-type: task
created-at: "\"2026-07-19T21:16:49.430874+02:00\""
---

Follow-up to habu-pkg-stdlib-modules-3197dbbc (landed 98dba35e): 30 flat lib modules still lack package scope: argv, array, build, codesign, content-key, date, engine-id, errors, ffi, fmath, fmt, fs-mutate, fs, hashmap, json-write, map, prelude, process-argv, process-command, process-cwd, process-fork, property, render, report, sort, source, stats, table, test, time. Package them in dependency order in small reviewed batches; every migrated module needs all call sites updated, its manifest rows regenerated (lib/std.manifest via tools/public-signatures.f - note it only emits public-SECTION definitions, not EXPORT-from-private), its tests green, and the bare-load gate (test/stdlib-standalone-load.f) still green. Watch the two seams found during the first slice: tools/bundle-lib.f source-concatenation needs the provided-marks path for any newly-required module, and modules loaded by the docs/bootstrap.md seed prelude (errors, string, memory, fs, fs-mutate, process, process-argv, process-env, codesign) must be verified against that exact prelude before their batch lands - if packaging one would disturb the seed path, split it into its own seed-verified batch. prelude and errors are the riskiest; do them last as their own slices.

Claim: RELEASED 2026-07-21 (agent=pkg-remaining finished its batch; landed 3ce3b677/f1e7d41b/9a8137c8 = TIME package, ARRAY package, prelude documented as the sanctioned core/prelude global surface). Verified remaining set after that batch: errors, process-argv, map, source (ffi and test need no action - ffi already package-owned with a documented compat shim, test is a pure aggregator; the shim retirement is separate work).

DECISION 2026-07-21 (orchestrator): lib/errors.f stays a documented GLOBAL vocabulary and is NOT packaged. Throw codes are a flat, globally-unique code space exactly like the blessed src/config.fs constants; qualifying 51+ consumer files as ERRORS:E-* adds churn without a collision to prevent, and errors.f sits first on the seed prelude where a packaging mistake bricks recovery. Treat errors.f alongside prelude.f as the second documented core-surface exception.

Remaining slices, each its own careful lane when scheduled (fill-in priority; do not take a type-chain slot): (1) process-argv - 100+ consumers, on the seed prelude, full two-stage installed-engine proof required; (2) map - declares ENUM slot-state + SUMTYPE map-loc, so packaging renames every SLOT--STATE:*/MAP--LOC:* constructor per the cad-num-types.f nested-constructor precedent; consider deferring until after the M1 ENUM migration wave to avoid renaming twice; (3) source - beneficial (removes global SOURCE-* collision with the baked core-prefix namespace) but consumer disambiguation against src/habu + bootstrap/cg/forth.fs prefix names is error-prone; scout first.
