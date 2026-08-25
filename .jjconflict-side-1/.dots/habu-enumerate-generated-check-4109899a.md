---
title: Enumerate generated check-state transitions
status: closed
priority: 2
issue-type: task
closed-at: "2026-07-08T00:00:00+02:00"
close-reason: Both specced parts landed. (1) TRUSTED.md "Build-time-generated trust (explicit exemption)" now enumerates the check-CLI runner prelude (tools/check-core.f CHK-BUILD-PREFIX) alongside BFR-CHECK-OFF and check-hook.f's own install, naming the 0 set-check window, the CHECK-F-HOOK re-arm, and the 70-throw fail-closed body. (2) Source-shape regression check/prelude-hook-shape (tools/check-test-lib.f CKT-TEST-PRELUDE-HOOK): a line-split scanner asserts every set-check line in the GENERATED prelude is one of the two audited shapes with exactly one hook install, plus the fail-closed body text; doctored legs prove teeth (appended rogue "' EVIL-HOOK set-check" rejects; missing-install truncation rejects). Generated source stays lexer-invisible to checked-boundary-lint/trusted-inventory by design - this regression is the policing for the generated seam. check-test rc 0; trusted-inventory strict rc 0.
created-at: "2026-07-08T09:22:08.935302+02:00"
---

TRUSTED.md 'Build-time-generated trust (explicit exemption)' claims the generated set is EMPTY except the refresh prelude's BFR-CHECK-OFF and check-hook.f's own ' HOOK set-check - but tools/check-core.f CHK-BUILD-PREFIX (:869-876) emits '0 set-check' + ': CHECK-F-HOOK ...' + "' CHECK-F-HOOK set-check" into every check-CLI child runner prelude: a generated check-state transition missing from the enumeration. The lexer-based lint and trusted-inventory correctly skip string literals, so the generated install is invisible to hook-identity policing (habu-police-set-check-850bc543) by design; the policing story for generated installs is source-shape regressions (build-fixpoint-test pins stage2 shapes), but nothing pins the check-CLI prelude's hook name. Fix: (1) update the TRUSTED.md enumeration to name the CHK-BUILD-PREFIX prelude; (2) add a source-shape regression (tools/check-test.f territory) asserting the generated prelude installs exactly CHECK-F-HOOK and re-enables fail-closed (70 throw), so a rogue name in generated text cannot land unnoticed.
