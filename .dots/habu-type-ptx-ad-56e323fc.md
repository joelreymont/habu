---
title: Type PTX AD policy
status: open
priority: 2
issue-type: task
created-at: "2026-07-19T21:04:38.045698+02:00"
blocks:
  - habu-pkg-ptx-autodiff-d15a611e
---

lib/ptx/ad.f:222-237 models the closed save policy {auto, save, recompute} as three raw integer constants plus a generic variable. AD-POLICY! accepts n, spends two runtime comparisons rejecting out-of-range values, and AD-SAVE? rereads the raw cell twice with equality/default dispatch. tools/ptx/ad-entry-lib.f:434-441 and lib/ptx/ad-test.f:79-86 pass raw codes; the negative only proves the hand-written bounds check, not static domain separation. The shipped ENUM, TYPED-VARIABLE, and MATCH facilities make this state closed and stored without raw-cell laundering. After habu-pkg-ptx-autodiff-d15a611e gives the subsystem its package owner, declare a package-local/public policy ENUM with auto ordinal zero so the zero image preserves the current default; store it in TYPED-VARIABLE; make the setter and every caller take the enum; replace the equality/default chain with exhaustive MATCH. Delete raw policy constants and the runtime range-check branch; invalid n and foreign-enum calls must reject at check time. Preserve AD-SAVE? decisions and emitted PTX byte-for-byte for all three policies. Add checked negative fixtures for n/foreign-enum policy calls, exact decision-table tests, and before/after CODELEN plus loaded JIT/DATA measurements; require no code/data growth. Files: lib/ptx/ad.f, ad-test.f, tools/ptx/ad-entry-lib.f and its owning tests/docs. Ownership: save-policy typing only; DAG operation typing is separate.
