---
title: Seal every captured package against reopening
status: open
priority: 2
issue-type: task
created-at: "2026-09-16T16:49:49.727440+03:00"
---

Problem: only seven system packages are sealed (src/core/checker.f CHECKER-SEALED-PKG?: tfam, type, match, checker-cert, lower-cert, lower-cert-hook, engine-error); any other package captured into the engine can be reopened by user source with `package NAME`, which reaches its private words and is the only reason their names and signatures must stay in the image. Acceptance: at capture (SEAL-CAPTURE) every package present in the engine is marked sealed, the mark is a checker fact the image carries (not a name list in checker.f); user source that reopens or qualifies into a sealed package is refused by name at the package token with the existing E-EXPORT-SEALED family; an application own packages, and packages loaded from source after boot, reopen as before; docs/forth.md states the rule beside the multi-file reopening paragraph; a fixture reopens an engine package and is refused, reopens a user package and succeeds; downstream (Radar, Tender, Loom, Maki) audited for engine-package reopens before landing, each found site converted to a public API or reported. Files: src/core/checker.f, src/core/internal-mark.f, src/habu/habu2.f (seal token), docs/forth.md, test/. Verify: the fixture; test/run.f; downstream suites. Depends: none. Ownership: checker seal. Claim: unassigned. Parent: the ship-only-the-surface epic.
