---
title: Enforce the seal guards on Gforth
status: open
priority: 1
issue-type: task
created-at: "2026-10-09T20:26:37.151402+03:00"
blocks:
  - habu-compile-checked-definitions-6e291539
---

Problem: native traps every post-seal write into the ten bands of src/habu/data-bands.f:20-28 (GUARD-SPAN, src/habu/habu1.f:295-317: exit 83 ENGINE-ERROR:SEAL-VIOLATION, no message, no hook), refuses `ndict!` below the seal watermark SEAL-NDICT-CELL with exit 83 (habu1.f:1474-1478), and installs the preflight hook once (BSETPREFLIGHT, habu1.f:3740-3770: an identical reinstall is inert; a replacement or an xt outside the live code window is refused with a named rc-70 line). The Gforth codegen dot gave the host the friend band only (prims.fs SPAN-GUARD, its band constants copied into prims.fs). The host's ndict! (reader.fs HB-NDICT!) has no watermark check (`0 ndict!` exits 83 on native and goes on on the host), and its set-preflight has neither rule (~/.cache/tmp/heron-arm64/evidence/gfcodegen/report-4.md). data-bands.f is one table so that every engine's guard widens in the edit that adds a band; a host copy of its rows would not.
Acceptance: the host's span guard walks native's band table (src/habu/data-bands.f, and the layout.f and data-claims.f facts it is built from) and traps a post-seal write into any band at every host computed-address sink, as GUARD-SPAN does; no band offset is copied into a host file, and the friend constants leave prims.fs. If the host cannot read that table before the seal, the worker stops and reports why with file:line. ndict! below the watermark exits 83 as native. set-preflight follows BSETPREFLIGHT, with the host code range set-check uses. Cases in test/gforth/cases/ match native: a post-seal store into each band's first and last byte and the bytes just outside it; `0 ndict!`; set-preflight install, identical reinstall, replacement and an xt outside the range.
Files: src/host/gforth/prims.fs, src/host/gforth/reader.fs, src/host/gforth/layout.fs, src/host/gforth/boot.fs, test/gforth/cases/, test/gforth/host-test.f.
Verify: `HB_TMP=$PWD/build/tmp bin/hb --load test/gforth/host-test.f`.
Depends: the Gforth codegen dot. Ownership: the files above. Worker: worker-max. Claim: unassigned.
