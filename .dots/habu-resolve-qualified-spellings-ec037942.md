---
title: Resolve qualified spellings in the name check
status: open
priority: 2
issue-type: task
created-at: "2026-08-03T22:45:51.601209+02:00"
---

Regression on proofs introduced by f7eb936d (the inline row name check): tools/codegen-compare-test.f assertion 238 red — SPEND-FOUR migrates FAN-CEILING-N whose body names its callees with package-qualified spellings (CODEGEN-CORPUS4:C-ADD1-N), the NINL row records the bare published name (C-ADD1-N), NELAB:CALLEE-COPY?'s NAMED? compares raw strings, mismatches, and throws E-NELAB-INLINE (-8559) on a legitimate program. Green at 23bb4070, red at proofs; verified by bisect and by the -8559 fingerprint. The check must compare resolved identity, not raw spelling: a package-qualified reference and the bare published name denote the same word. Fix in the elaborator's side (resolve the site's spelling through the same naming grammar the engine uses — the package-qualified form's final component — or better, compare against the resolved symbol's own name), NOT by widening NAMED? to substring matching. Regression tests: a cross-package qualified caller of a recorded callee (the compare-test shape) in test/compiler/native-inline.f, plus the existing mismatch case must still refuse. Gate: tools/codegen-compare-test.f green again; add it to the per-landing gate list.
