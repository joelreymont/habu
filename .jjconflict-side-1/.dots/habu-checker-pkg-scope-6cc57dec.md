---
title: Checker package-scope resync across scope-changing recovery
status: open
priority: 2
issue-type: task
created-at: "2026-07-17T05:27:13.283720+02:00"
---

Exotic residual from the pkg-scope recovery landing (73942d02): when a package is legitimately open ACROSS an evaluate/REPL boundary and the failing input ITSELF changed package scope before erroring (';package ... package Y ... <err>' shapes), the ENGINE restores the boundary scope correctly (PKGSNAP) but the CHECKER remains at the inner scope - the PKGRESYNC-CELL drain only handles the restored-to-global case via checker-end-package. Fix: re-derive the checker's package name/mode from the restored engine PKG-REC on every recovery resync (needs a checker entry that sets scope to a NAMED package, not just end-package - check what the sealed-packages surface exposes; COORDINATE with tfam, surgical). Pre-existing misbehavior (also wrong before the landing); requires deliberate nested-evaluate gymnastics to reach. Acceptance: a fixture with a boundary-open package + an inner scope change + error asserts BOTH engine and checker land at the boundary scope (checked defines resolve in the right package after recovery); the existing pkg-scope fixtures stay green; engine batteries + full run.f. Files: src/habu/habu2.f (resync drain), possibly src/core/checker.f (named-scope entry - tfam window), test/gate-engine-lib.f. Ownership: engine/checker recovery coherence.
