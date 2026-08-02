---
title: Mirror namespace rows in recovery
status: active
priority: 1
issue-type: task
created-at: "2026-07-31T06:35:00.747855+02:00"
---

Source dependencies: the exact reviewed native E1 namespace-row source and the
HB package hard cut. Owner: the Gforth recovery emitter in
`bootstrap/cg/forth.fs` under its existing engine-emission package.

Mirror the native namespace kind constants, 48-byte namespace rows, exact
`LNSFIND`, absolute prefix creation and reopen, public/private package pair
allocation, type rows with a protected public WID and zero private role,
full-prefix inline/EXT storage, and strict snapshot and compact-AOT row
validation. Namespace roles are zero or dynamic WIDs below `WID:MAX`; pair and
single-WID allocation preflight the same `WID:MAX` bounds as native before any
publication. `C-STORE-DEF-NAME` is the shared publication choke and rejects
publication into a protected WID with native-equivalent friend handling and
register preservation.

One parametric compile-failure tail handles package, qualification,
name-capacity, dictionary-capacity, and WID-capacity failures so caught
evaluation restores CP and NDICT while WIDN remains monotonic. Snapshot
corruption, malformed namespace rows, compact AOT marker rows, and
protected-WID publication remain hard failures. Keep first-colon qualified
lookup here; last-colon behavior belongs to the separate resolution leaf.

Write set: `bootstrap/cg/forth.fs` and the existing behavioral rows in
`tools/bootstrap-codegen-test.f`. Do not add a package owner, parent link, side
table, schema, version, compatibility path, native source change, source-copy
parser, or lint. Acceptance runs the real no-binary recovery image and proves
namespace kinds, absolute prefix creation/reopen, `WID:MAX` boundaries,
protected publication, caught rollback, snapshot and compact-AOT validation,
and failure parity with native.

Claim: agent=recovery_e1_impl workspace=.jj-ws/habu-mirror-namespace-rows-ff1dc330.
