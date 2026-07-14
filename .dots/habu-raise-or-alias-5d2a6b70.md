---
title: Raise or alias TF-CTOR-NAME-LIMIT ctor packages
status: open
priority: 2
issue-type: task
created-at: "2026-07-14T17:39:35.451726+02:00"
---

Checker capability gap (found by tspolicy lane 2026-07-14): generated ctor-package names silently SHA-fall-back when the escaped PKG-FAMILY name exceeds TF-CTOR-NAME-LIMIT=16 (src/core/type-family.f:604) - e.g. POLICY-REQ--CLASS (17), POLICY-PROMOTE--POLICY (22), EVID-CERTIFY--SLOT (18) get opaque Thexhash-TAIL ctor packages: deterministic but unreadable/fragile, so unusable in committed source. This forced the R7 renames req-class->req and promote-policy->gate-set, and makes EVID's slot sums unconstructable cross-package (blocks readable bundle construction and the end-to-end POLICY:CHECK execution test). Fix in the checker/compiler: raise the limit (audit the 16-char constraint's real origin - dictionary name cap? AOT record field?) or add short ctor aliases (EXPORT-style alias for the ctor package). Acceptance: a >16-char escaped family in a package is constructable by readable name; negative regression for whatever real limit remains; EVID slot sums constructable cross-package. Files: src/core/type-family.f + checker tests. Verify: type-family suite, full run.f. Ownership: checker type-family lowering. NOTE: coordinate with sol's active type-system lanes (region-lower, numeric-roles) before claiming - same subsystem.
