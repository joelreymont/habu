---
title: Police set-check hook identity in checked-boundary-lint
status: open
priority: 2
issue-type: task
created-at: "2026-07-02T11:01:04.432158+02:00"
---

tools/checked-boundary-lint-core.f:233 UB-SET-CHECK-ON? treats any non-0 set-check argument as a checking re-enable; nothing validates that the installed xt is one of the audited checker hooks, so ' EVIL-HOOK set-check passes the boundary lint while neutering verification. trusted-inventory now counts every ' NAME set-check / ['] NAME set-check as a ratcheted HOOK-INSTALL row (named by the hook), so rogue installs are visible and grow the baseline, but identity is still not statically policed. Capability: checked-boundary-lint validates the installed hook name at every set-check install site against the audited list (HOOK, USER-HOOK, SNAP-CHECK-HOOK, CHK-CHECK-HOOK, LINT-CHECK-HOOK, ES-VERDICT-HOOK plus named test fixtures) and rejects unknown installs; add a negative regression with a rogue hook install. Remove the TRUSTED.md caveat when it lands.

## Resolution (implemented)

checked-boundary-lint now polices hook identity. tools/checked-boundary-lint-core.f
tracks a second-back token (UB-PREV2), UB-SET-CHECK-INSTALL? recognises a
`' NAME set-check` / `['] NAME set-check` install (tick two-back, name one-back,
which also excludes the `' set-check` name reference), UB-HOOK-ALLOWED? checks the
name against the audited list (HOOK, USER-HOOK, SNAP-CHECK-HOOK, CHK-CHECK-HOOK,
LINT-CHECK-HOOK, ES-VERDICT-HOOK, PROP-CHECK-HOOK = the 7 distinct names of the 12
HOOK-INSTALL sites), and UB-REPORT-ROGUE-HOOK emits an E-UNAUDITED-HOOK finding
(text + JSON) for any other name. Negative regression: CBLT-TEST-ROGUE in
tools/checked-boundary-lint-test-lib.f asserts `' EVIL-HOOK set-check` exits 1 with
`UNAUDITED-HOOK`/`EVIL-HOOK` in the report. The TRUSTED.md caveat is updated.

Dot stays OPEN (not archived): the TRUSTED.md trusted-inventory-classes block owns
its 9 HOOK-INSTALL rows (src/core/check-hook.f:HOOK ... test/prop-test-core.f:PROP-CHECK-HOOK)
to this dot id, and `bin/hb --load tools/trusted-inventory.f -- strict` fails if an
owning dot is missing from .dots/ (DOT-EXISTS?). Archiving this dot would orphan
those rows and turn strict red, and habu-audit-trusted-inventory-3a950436 explicitly
disclaims the hook rows. It therefore remains the owner-of-record; do not `dot off`
until those rows get a permanent owner.

## Audit (2026-07-08, on 44efc694): implementation verified complete

Every capability line-item is landed and green:
- Hook-identity policing: tools/checked-boundary-lint-core.f tracks the
  second-back token (UB-PREV2, :205-:230), recognises `' NAME set-check` /
  `['] NAME set-check` installs (UB-TICK-PREV2? :261, install recognition
  :264), validates the name against the audited 7 (UB-HOOK-ALLOWED?: HOOK,
  USER-HOOK, SNAP-CHECK-HOOK, CHK-CHECK-HOOK, LINT-CHECK-HOOK,
  ES-VERDICT-HOOK, PROP-CHECK-HOOK), and reports any other name as
  E-UNAUDITED-HOOK.
- Site census (2026-07-08 tree): every token-level install site's hook name is
  in the audited list; the only other textual match, tools/check-core.f:876
  `' CHECK-F-HOOK set-check`, is a string literal inside CHK-BUILD-PREFIX's
  GENERATED child prelude - string/comment mentions are lexer-skipped by
  design and documented in TRUSTED.md ("comment and string mentions never
  count").
- Negative regression: CBLT-TEST-ROGUE (tools/checked-boundary-lint-test-lib.f
  :244) - `' EVIL-HOOK set-check` exits 1 naming EVIL-HOOK. Owning test green:
  `bin/hb --load tools/checked-boundary-lint-test.f` rc 0.
- TRUSTED.md caveat: the old "identity is still not statically policed" text
  is gone, replaced by the policed statement (UB-HANDLE-INSTALL paragraph).
- Ratchet: `bin/hb --load tools/trusted-inventory.f -- strict` rc 0.

NOT closed, per the owner-of-record constraint above (all ratchet row-owner
dots are open by convention; DOT-EXISTS? is file-presence, but archive removes
the file). Two follow-ups minted instead of editing shared files:
- habu-enumerate-generated-check-4109899a (TRUSTED.md's "Build-time-generated
  trust" enumeration omits CHK-BUILD-PREFIX's generated `0 set-check` +
  `' CHECK-F-HOOK set-check` prelude; the generated-install seam has no
  source-shape regression pinning the hook name).
- habu-permanent-owner-for-83401fcc (permanent-ownership mechanism so
  completed capability dots like this one can close without orphaning
  TRUSTED.md rows).
