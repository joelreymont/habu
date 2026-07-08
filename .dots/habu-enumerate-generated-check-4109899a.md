---
title: Enumerate generated check-state transitions
status: open
priority: 2
issue-type: task
created-at: "2026-07-08T09:22:08.935302+02:00"
---

TRUSTED.md 'Build-time-generated trust (explicit exemption)' claims the generated set is EMPTY except the refresh prelude's BFR-CHECK-OFF and check-hook.f's own ' HOOK set-check - but tools/check-core.f CHK-BUILD-PREFIX (:869-876) emits '0 set-check' + ': CHECK-F-HOOK ...' + "' CHECK-F-HOOK set-check" into every check-CLI child runner prelude: a generated check-state transition missing from the enumeration. The lexer-based lint and trusted-inventory correctly skip string literals, so the generated install is invisible to hook-identity policing (habu-police-set-check-850bc543) by design; the policing story for generated installs is source-shape regressions (build-fixpoint-test pins stage2 shapes), but nothing pins the check-CLI prelude's hook name. Fix: (1) update the TRUSTED.md enumeration to name the CHK-BUILD-PREFIX prelude; (2) add a source-shape regression (tools/check-test.f territory) asserting the generated prelude installs exactly CHECK-F-HOOK and re-enables fail-closed (70 throw), so a rogue name in generated text cannot land unnoticed.
