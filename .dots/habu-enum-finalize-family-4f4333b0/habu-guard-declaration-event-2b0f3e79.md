---
title: Guard declaration event reset
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-24T02:56:00.553945+02:00\""
---

Why: DECL-EVENT:DECL currently permits a second DECL in one frame and permits a freshly opened frame to bind the same provisional family after an earlier frame published under the same live checker rollback mark. DECL-EVENT:RESET also erases published evidence while an event frame or checker rollback mark is live. The ENUM finalizer cannot be immutable while either path exists. Owner and interface: after TFAM rollback-frame definitions in src/core/type-family.f, add protected temporary package TYPE-FAMILY-OWNER with PROVISIONAL? ( family -- bool ) and RESET-ALLOWED? ( -- bool ); PROVISIONAL? is true only with a live rollback frame and saved TFAM-N <= family < live TFAM-N. DECL-EVENT captures them through exactly two inventoried TRUSTED bridges, uses no raw frame access, and generated-declaration protection undefines both capabilities before user source. No finalizer or kind mutation in this leaf. Behavior: DEV-DECL requires the exact open top frame, rejects E-DEV-STATE 7162 if that frame already owns any DECL, and, only for a family in the current provisional range, rejects E-DEV-FAMILY-SCOPE 7173 if any published event already names it. Every valid event frame begins with DECL and all later rows carry that bound family, so kind and owner predicates would be redundant rather than additional protection. DEV-RESET checks before every store and rejects E-DEV-TX 7161 when DEV-TX-DEPTH is nonzero or TYPE-FAMILY-OWNER says a checker rollback frame is live. At depth zero RESET remains legal even with older live families because a later checker mark saves their high-water and PROVISIONAL? excludes them. Published event identity and lifecycle remain unchanged. Checkpoint: through public DECL-EVENT and the real CHECK-CANDIDATE start/done path, prove same-frame duplicate DECL, same-mark fresh-frame rebind, active-frame RESET, and checker-mark RESET currently succeed or erase evidence; no raw event or family store. Stop on any need for a second registry, persisted state, restore hook, event-record change, or caller migration. Acceptance: all four paths reject with the exact codes before DEV-N, DEV-PUB-N, identity, family kind, field state, or rollback depth changes; wrong-family same-frame rebind also rejects 7162; another provisional family remains legal; rollback of an unpublished first frame permits an identifier reused by a later checker mark; a family older than a new mark is not treated as provisional; RESET succeeds after all event and checker frames close. Remove either owner query, the current-frame scan, published-family scan, family comparison, range test, or pre-mutation reset guard and a production-path regression fails. Capability public/private and post-protection absence tests pass. Files: src/core/type-family.f, src/core/decl-event.f, src/core/generated-declaration-protection.f, test/decl-event-suite.f, TRUSTED.md, FILEMAP.md only if inventory requires. Smallest check: bin/hb < test/decl-event-suite.f. Run exact package/typed-local/trust/inventory and generated-declaration focused gates; no full native gate before root integration.

Ratchet closure: the verified integration base now certifies 4058 linux-arm64
definitions. The preserved guard implementation added four top-level colon
definitions in `src/core/decl-event.f` and two in `src/core/type-family.f`, so
4064 is expected only if its exact six-definition delta survives the rebase.
Remeasure the assembled stage2 source on the final rebased implementation and
update only the linux-arm64 Certified row in `STATUS.md` to that measured
value; do not copy the expected value without proof. Uncheckable and rejected
remain zero, the macOS row remains owed, and `Last verified` remains unchanged
until the combined full tree is green. Acceptance: the build-fixpoint census
and `STATUS.md` agree on the exact integration tree, status/stale-status checks
accept the row, and removing any retained guard definition makes the ratchet
fail. No source, date, other-target, timing, or code-size edit belongs to this
control correction.

Implementation delivered and accepted from agent=decl_event_guard
workspace=.jj-ws/habu-guard-declaration-event-2b0f3e79 commit=af0cef9106fa.
Claim: agent=nested_decl_fix
workspace=.jj-ws/habu-guard-declaration-event-2b0f3e79-r2.

Destruction correction on exact candidate `9e1d46f3`: the accepted
`DEV-PUB-FAMILY?` scans only `[0, DEV-PUB-N)`. A nested `PUBLISH` finalizes its
frame without advancing `DEV-PUB-N`, so its surviving declaration is invisible
to a later outer `DECL`. Simply excluding every active-frame owner is also
wrong: an outer and inner active frame can bind one provisional family, then
publish two declaration rows.

Exact design: keep event owners and the active transaction stack as the sole
authority. Add private `DEV-OWNER-ACTIVE?` to compare an event owner with every
currently active frame owner. Replace the old published-only scan with private
`DEV-SURVIVING-FAMILY?`: a matching declaration counts when it is below
`DEV-PUB-N` or its owner belongs to no active frame. Current and active-ancestor
rows remain provisional for this scan. Before `DEV-PREPARE` mutates field or
publication state, compare the current frame's bound family with every active
ancestor frame's bound family and reject `E-DEV-FAMILY-SCOPE` on a match. The
existing current-frame duplicate guard remains authoritative within one frame.
Do not advance `DEV-PUB-N` early and do not add a watermark, registry, state
flag, persisted bit, or rollback hook.

Checkpoint: through public `DECL-EVENT` inside one real `CHECK-CANDIDATE`
savepoint, publish an inner declaration, then bind its family in the still-open
outer frame. The pre-change path returns zero. Independently bind one family in
an outer frame and an inner frame; the pre-change inner `PUBLISH` succeeds.
Run the package gate on the first representative source diff before continuing.

Acceptance: both duplicate-family paths reject `E-DEV-FAMILY-SCOPE` before
field publication, published-event watermarks, family state, transaction depth,
or identity state changes. Cover previously global rows, rows finalized by a
nested frame, an active ancestor followed by inner rollback, an active ancestor
followed by rejected inner publish, frame-owner reuse after rollback, nested
publish followed by outer rollback, and successful outer finalization. A
different provisional family remains legal. Nested rollback permits later
reuse; outer rollback removes surviving nested rows; outer publication makes
the complete surviving stream globally published. Mutations that remove
`DEV-OWNER-ACTIVE?`, use only `DEV-PUB-N`, omit the `DEV-PREPARE` ancestor
guard, or retain stale frame ownership must each fail through the production
path.

Exact write set remains `src/core/decl-event.f` and
`test/decl-event-suite.f`. No ratchet, status, type-family, protection,
manifest, public-interface, new state, or caller migration belongs to this
correction.
