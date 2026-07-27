---
title: Package checker tools
status: open
priority: 1
issue-type: task
created-at: "2026-07-19T21:30:10.947678+02:00"
---

SCOPE REDUCED TO CHECK-CLI (2026-07-27, orchestrator ruling on a duplicate
ownership found while correcting restored dot contracts). This dot originally
owned packaging TWO modules, tools/check-core.f and tools/public-signatures-core.f.
The public-signatures half is SUPERSEDED: the vecmem lane already did that work
under habu-pkg-public-signatures-e25db8b1, delivered as lane commit 97642e1c
"Package public-signatures core and its callers", which creates package
PUBLIC-SIGNATURES with a thirteen-word export surface (MAIN private, the
command-line entry reopens the package), gives the suite its own package, and
produces byte-identical outputs. Delivered work owns the file, so
habu-pkg-public-signatures-e25db8b1 is the sole owner of packaging
tools/public-signatures-core.f and anything depending on that packaging blocks
on it, not on this dot. Note that the delivered commit is in the vecmem lane and
not yet on master, so the file still reads as unpackaged there; that is an
integration state, not a reason to redo the work.

What remains owned here: tools/check-core.f only, into package CHECK-CLI. The
census below is left intact for the check-core half and is stale for the
public-signatures half - the lane measured 224 definitions there, not 222, and a
real cross-file surface of 13 words, not 15, because two of the originally
counted names turned out to be a comment and a fixture string.

tools/check-core.f:22-1266 exposes 280 CHK-* or CHECK-MAIN globals; tools/public-signatures-core.f:9-899 exposes 222 PS-* globals. They are loaded by the checker CLI, diagnostics gate, and diagnostic worker; tests currently reach internal state directly. Sixty-two names exceed the inline dictionary limit, and prefix spelling provides no privacy for configuration, buffers, parser/replay state, signature tables, or lifecycle helpers. Put the modules in packages CHECK-CLI and PUBLIC-SIGNATURES. Export MAIN plus only the deliberate in-process capture/materialize/direct-run contract; tests reopen owner packages for white-box cases; update CLI/gate/worker callers directly and delete all CHK-/PS-/CHECK-MAIN aliases. Coordinate grammar behavior work in habu-tools-check-unified-fb3b67f6 and habu-tools-reflect-all-80b1aa58 without absorbing it. Preserve CLI text/JSON, source-list replay, all-errors behavior, direct-run captures, diagnostics, public-signature manifests/order/hashes, and exit codes exactly. Add old-global/private rejection and public qualified positives. Measure long-name/dictionary/JIT/DATA/CODELEN, startup/capture/replay throughput before/after. Verify checker/public-signature/diagnostic/worker suites, package/host/filemap/dot lints, fixpoint, and full native gate. Ownership: module boundaries and caller renames only.

Read the paragraph above with the PUBLIC-SIGNATURES half struck out: this dot
packages CHECK-CLI, exports MAIN plus the deliberate capture, materialize and
direct-run contract, deletes the CHK- and CHECK-MAIN aliases, and migrates the
checker CLI, diagnostics gate and diagnostic worker callers. The
public-signature manifest, order and hash preservation stays in the acceptance
list as a cross-check that this work does not disturb the other package, not as
work this dot performs.
