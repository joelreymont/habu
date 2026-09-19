---
title: Preserve the source when stream-copy paths alias
status: active
priority: 1
issue-type: task
created-at: "2026-09-18T19:52:45.285581+03:00"
---

Claim: alder, .jj-ws/alder-alias-reland, based on integrated 88e43ad5.

Measured re-landing on 88e43ad5 with a private product engine: the alias and
fs-mutate tests pass. Empty MAIN still carries 3 zero DATA bytes (7710 nonzero
bytes). They are the NUL terminators of `stat`, `__error`, and
`__errno_location`, at file offsets 8712/8720/8737 and window offsets
131548/131556/131573. These belong to NSTR's literal arena, not the FFI name
table. Loading the linker above BLOB-END still interns its literals into the
application pool; aot-window-latch.f describes this contamination explicitly.
The sparse format intentionally carries short zero gaps (RUN-GAP-MIN=2), so
removing the contamination is the responsible fix, not changing that threshold
or the HBT-SIZE-AOT expectation. Requested the narrow NSTR pool scope from
Hazel before editing her compiler-owned file.

The follow-up isolates linker loading with NSTR ACTIVE/WINDOW-OPEN/SWITCH,
restores the application pool on either load outcome, and refuses foreign or
compact imported owners. The same stripped fixture now carries 57 nonzero DATA
bytes and zero zero-bytes. The whole hb-build-fixtures row passes, including
HBT-SIZE-AOT and HBT-STRIPPED-LIB-STATE, without changing their assertions.
Also green: native-string, alias/fs-mutate/fs-identity, stripped-literal,
stripped-sparse-data, stripped-address, dynamic-buffer-capture and both tiers
of aot-named-cells. Three private native-build generations are byte-identical:
SHA256 b9da1368b8da6c3a43dc3ad618754c73e082b9c9755c457e8ad2ca87786c17e0,
4,391,104 bytes. Astra reviewed both commits and cleared the two follow-up
corrections. Hazel owns the final full gate and closure after duplication.

Problem: lib/fs-mutate.f:175-193 opens the destination with O_TRUNC before comparing its identity with the open source. COPY-FILE-STREAM from a file containing must-survive to a symlink to that file returned success and left the source at zero bytes on bin/hb. Identical paths and hard links share the same destructive path. Acceptance: reject or safely handle identical source/destination inodes before any truncation, using descriptor identity where needed to avoid a path-check race; regression tests cover identical paths, symlink and hard-link aliases, preserve source bytes on refusal, and retain ordinary streaming copies. Files: lib/fs-mutate.f and its focused tests, filesystem identity primitive/boundary only if needed. Verify: focused filesystem suites through bin/hb. Ownership: filesystem library. Claim: unassigned. Update 2026-09-19: the FFI-in-stripped-image defect is fixed on the line (Carry an FFI call into a stripped image: RBASE-CELL published by the stripped entry as a TEXT-BASE owned cell; BUILD-AOT-FFI fixture), so this commit can be re-landed once that engine is integrated. Re-landing acceptance: rebase 5cff25e1 onto the head, run the whole hb-build-fixtures row (tools/hb-build-test.f) on a private engine built from the tree, and hold BOTH HBT-STRIPPED-LIB-STATE and HBT-SIZE-AOT green. HBT-SIZE-AOT was the second red in chain BZ: with this commit every stripped image - even ': MAIN ( -- ) ;' - carried 3 zero data bytes (data 7607+0 -> 7644+3), i.e. the FFI name table's padding entered the image's data window although the program requires no FFI library; find why (the builder's own lib/fs-mutate.f -> fs-identity.f FUNCTION: declarations in hb-build's process, or the window's extent) and fix that layer before landing - the invariant 'not one zero byte of a stripped image travels' is pinned on purpose.

Decision (hazel, 2026-09-19 19:40): the three zero bytes are the NUL terminators of stat/__error/__errno_location, lib/fs-identity.f literals interned into the application pool while tools/aot-build-core.f loads the linker above BLOB-END (aot-window-latch.f admits the contamination as a cost). Fix shape, released to alder narrowly: NSTR:ACTIVE ( -- owner ) and NSTR:SWITCH ( owner -- ) (sets ACTIVE-P, rebuilds SLOT from the pool rows, refuses an owner off the FIRST-P chain); WINDOW-OPEN = NEW-POOL dup APPEND-OWNER SWITCH; the builder keeps the application owner, opens a window for the linker, switches back before LINK so REINTERN-OWNED imports only closure-reached linker literals. No pool close, no scope wrapper. string.f is baked: fixpoint chain. The no-zero assertion of the hb-build fixture stays.
