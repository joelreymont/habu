---
title: Guard provisional declaration ownership
status: open
priority: 1
issue-type: task
created-at: "\"2026-07-24T02:56:00.553945+02:00\""
---

Why: while a family remains inside the current checker rollback frame, exactly one live declaration-event row may claim it. Today a published row, a finalized nested row, or an active ancestor row can be followed by another event frame binding the same provisional family. RESET can also erase live event or checker evidence. Both break immutable ENUM finalization.

Owner and interfaces: after the rollback-frame implementation in src/core/type-family.f, protected temporary package TYPE-FAMILY-OWNER publishes PROVISIONAL? ( family -- bool ), true exactly when a checker rollback frame is live and the family lies between that frame's saved family high-water and the current family high-water, and RESET-ALLOWED? ( -- bool ), true exactly when no checker rollback frame is live. DECL-EVENT captures them through inventoried internal bridges DEV-FAM-PROVISIONAL? and DEV-FAM-RESET-ALLOWED?. src/core/generated-declaration-protection.f retires both owner capabilities before user source.

Binding invariant: before mutating the top event frame, DEV-DECL validates the exact live top token and preserves current same-frame behavior: a second same-family DECL rejects E-DEV-STATE 7162 and a wrong-family rebind rejects E-DEV-FAMILY-SCOPE 7173. When the frame is unbound and the requested family is provisional, scan every live event row in [0, DEV-N) and reject 7173 if any DECL row names that family. Bind and emit only after every check succeeds. This complete live-row scan covers globally published rows, finalized nested rows, and active ancestor rows; rolled-back rows are outside DEV-N.

Reset invariant: DEV-RESET rejects E-DEV-TX 7161 before its first store when either event depth is nonzero or RESET-ALLOWED? is false.

Production failures on verified master: inside a real checker savepoint, all of the following return zero: publish a new family then bind it in a fresh event frame; finalize an inner frame then bind its family in the still-open outer frame; bind the same family in active outer and inner frames; RESET with an active event frame; RESET with only the checker savepoint live.

Public proof: add a package-owned section outside package DECL-EVENT in test/decl-event-suite.f. Reuse the existing checker-savepoint and family-declaration seams. Call only qualified public DECL-EVENT, TYPE-FIELD, and published family/reflection operations. Do not reopen DECL-EVENT, read DEV or DEVTX state, add a trusted bridge, or copy transaction state.

Acceptance: the three duplicate-claim shapes reject 7173 before public event count, identity, field count, family kind, or depth changes; active-ancestor duplication rejects at inner DECL before variant or field work; a rejected frame remains usable for a different provisional family or rollback; same-frame 7162/7173 behavior stays exact; different provisional families nest and publish normally; rollback permits later reuse; a family older than the top checker mark stays legal in a frame rolled back before publication; active-frame and checker-mark RESET reject 7161 and leave the original transaction/savepoint usable; RESET succeeds after both depths close. Both temporary owner names are absent after generated-declaration protection. Removing token validation, the provisional-range test, live-row kind/family matching, either RESET condition, or capability retirement makes a public production-path regression fail.

Files: src/core/type-family.f, src/core/decl-event.f, src/core/generated-declaration-protection.f, test/decl-event-suite.f, and TRUSTED.md. Forbidden: DEV-CUR-DECL?, DEV-OWNER-ACTIVE?, published-only scanning, an ancestor DEV-PREPARE guard, another registry, watermark, latch, persisted bit, restore hook, event-record field, public API, finalizer/frontend/legacy edit, new test trust, package reopen, private-state assertion, STATUS ratchet, or caller migration. Smallest owning check: bin/hb < test/decl-event-suite.f. Then run exact typed-local, package, trust/inventory, generated-declaration, and candidate-validation slices. Depends: none. Claim: unassigned.
