---
title: Guard provisional declaration ownership
status: open
priority: 1
issue-type: task
created-at: "2026-07-24T02:56:00.553945+02:00"
blocks:
  - habu-prove-verifier-event-f3454331
---

Why: coordinated checker rollback retires old event rows with their type rows, but two event frames inside the same live checker scope can still claim one provisional family. RESET can also erase live transaction evidence. These are same-generation ownership violations, separate from the cross-generation alias fixed by habu-retire-events-with-699e6475.

Owner and interfaces: TYPE-REGISTRY-ROLLBACK:PROVISIONAL? ( family -- bool ) is the sole saved-mark query and is true exactly when a registry rollback frame is live and family lies in its current provisional family range. DECL-EVENT captures it through package-local DEV-FAM-PROVISIONAL?. DECL-EVENT-ROLLBACK:ACTIVE? ( -- bool ) is the read-only outer event-scope query used only by DEV-RESET. Add no temporary package, global bridge, or second rollback mark.

Binding invariant: before mutating the top event frame, DEV-DECL validates the exact live top token and preserves current same-frame behavior: a second same-family DECL rejects E-DEV-STATE 7162 and a wrong-family rebind rejects E-DEV-FAMILY-SCOPE 7173. When the frame is unbound and the requested family is provisional, scan every live event row in [0, DEV-N) and reject 7173 if any DECL row names that family. Bind and emit only after every check succeeds. This complete live-row scan covers globally published rows, finalized nested rows, and active ancestor rows; rolled-back rows are outside DEV-N.

Reset invariant: DEV-RESET rejects E-DEV-TX 7161 before its first store when either the declaration-event frame depth is nonzero or DECL-EVENT-ROLLBACK:ACTIVE? is true.

Production failures on verified master: inside CHECKER-SCOPE-START, publish PRODUCT guard-red, open a second public declaration-event frame, and bind TFAM-N@ 1-; master returns zero instead of 7173. RESET likewise returns zero with an active event frame or with only the outer checker/event rollback frame live instead of rejecting 7161.

Public proof: add a package-owned section outside package DECL-EVENT in test/decl-event-suite.f. Reuse the existing checker-savepoint and family-declaration seams. Call only qualified public DECL-EVENT, TYPE-FIELD, and published family/reflection operations. Do not reopen DECL-EVENT, read DEV or DEVTX state, add a trusted bridge, or copy transaction state.

Test migration: in test/enum-decl-suite.f, TEST-PAYLOAD-VIEW binds its nested empty frame to the established epwide family in PFB, while its outer-token and empty-current-frame assertions stay exact. REG-RESTORE first calls public DECL-EVENT:RESET to retire references, then restores its eight raw target-registry marks; remove the scattered resets that followed individual restores. This white-box helper remains the sole test path that bypasses coordinated checker rollback.

Acceptance: same-family claims by published, finalized-nested, and active-ancestor rows reject 7173 before event, field, family, or depth mutation; the rejected frame remains usable for a different provisional family or rollback; different provisional families nest; an established family older than the saved mark remains legal; coordinated rollback permits later numeric-family reuse because it has already retired old rows. Active-frame and outer-scope RESET reject 7161 without mutation and RESET succeeds after both depths close. The enum token-scope and deterministic-identity assertions stay exact. Removing token validation, the provisional-range query, live-row kind/family matching, either RESET condition, or the atomic enum restore makes an owning regression fail.

Files: src/core/decl-event.f, test/decl-event-suite.f, and test/enum-decl-suite.f only. Forbidden: cross-generation alias detection, published-only scan, verifier cleanup, second registry or watermark, latch, persisted bit, restore hook, event-record field, temporary owner package, global bridge, new TRUSTED boundary, finalizer/frontend/legacy edit, STATUS ratchet, or unrelated caller migration. Smallest owning checks: bin/hb < test/decl-event-suite.f and bin/hb --load test/enum-decl-suite.f. Then run exact typed-local, package, generated-declaration, and candidate-validation slices. Depends: habu-prove-verifier-event-f3454331. Claim: agent=guard_impl workspace=.jj-ws/habu-guard-provisional-ownership (RELEASED 2026-08-21: workspace gone, no live lane - gc).
