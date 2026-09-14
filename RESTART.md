# Cedar restart — 2026-09-14

Correctness first; Joel deferred performance. Mutual review before integration.
Do not call Habu release-qualified yet.

## Workspace and communication

- Integration: `/home/joel/Work/habu/.jj-ws/cedar-closure-identity`.
- jj bookmark `cedar/compiler-integration`, GitHub `joelreymont/habu`.
- Herdr only for agent coordination; no BB watcher. Read
  `/home/joel/.codex/skills/herdr/SKILL.md`, verify `HERDR_ENV=1`, then
  `herdr agent prompt hazel "[cedar] ..."` (similarly rowan, alder, tender).
- Our agent name is `cedar`; pane `w3:p1`, tab label `4`, workspace `habu`.
  Prefer the live name or `--current`; old external pane `w1:p4` is stale.
- Hazel is account-limited until 17:50 local time. Internal Astra agent
  integer_overflow owns M4 in .jj-ws/cedar-integer-overflow; checker_followup_review
  approved the boolean correction and is reviewing the stripped-linker design.

## Integrated and reviewed

- `54e9ae7c`: native finally preserves grouped outputs. Hazel caught missing/dead
  call-row handling. The first binary failed EXECUTABLE-BUILD:WITH; the corrected
  guard follows execute for absent call rows and ignores dead output grouping.
  `test/compiler/native-finally.f` covers products, saved values, callbacks,
  cleanup/body throws, dead quotations, borrow refusal and the actual build scope.
- `9b6dc198` + `2a246ed9`: decimal scaling. Cedar reviewed original agent patch
  79f6766e; Hazel reviewed Cedar's negative POW10+ bound and unused-variable
  cleanup. Float suites pass at both tiers. Conversion remains approximate.
- `71baed89`: quotation input variance (C2); `a7227a9d`: PSIG trailing tokens
  (M8); `cd82e4d9`: implicit return-row borrowing (C4). These are unchanged
  transplants of Hazel's 9d7e967ef8c8, 8fdae0acd0a6, e637ee616184 on 6ce2e21e.
  Cedar reviewed source, added independent return-input/nested-polarity probes,
  and asked for the existing SGRBASE seal instead of per-word peek guards.
  Final fixtures include the added probes and grouped/quotation borrow controls.
- Bookmark was pushed through cd82e4d9. Later working edits require a checkpoint.

## Current integration check

A native rebuild exposed RETURN-BORROWED?'s numeric 0 in a `( -- f )` helper.
Reviewed correction 6594f822 returns RES-FALSE; the rebuild succeeds as
`/tmp/cedar-checker-native`, SHA-256
`954c53fda7e5012e09b356f5a5cedfb0cda0d60cfffc0e8d4c0abc23eec829ab`.
Source is cd82e4d9 plus that one-line correction. Checker-soundness, product
layout, float and AOT native-finally focused tests pass. Build session 41563 is
collected. The entire checker fixture forced to AOT refuses TAKE-RU8 (-8304):
the same non-neutral return-stack helper also refuses on c37b51ff. This is an
existing native effect limitation, not a passing AOT test or checker regression.

Integer-overflow agent owns engine admission and parser fixtures; Cedar owns
checker integer admission and independent review. Hazel's broad source-engine
gate/control runs are supplemental evidence, not the native release gate.

## Frozen downstream candidates

**New Maki-accepted candidate:** `.jj-ws/cedar-finally-acceptance`, source54e9ae7c,
`bin/hb` SHA-256
`c37b51ff3be1daf520cc0ba682403c87021618b5ff8e404eb26e4dd2143cbcda`.
The same binary is `/tmp/cedar-finally-reviewed`. It rebuilt itself byte-identically
as `/tmp/cedar-finally-selfhost`; both build sessions finished successfully.
Rowan accepted Maki master066a09ec plus native-cell treede65f404: suite50/50,
native_image.py (isolated REPL, warm recapture, KiCad DRC), plain/items/all-Kiapi
images, and Kiapi/native-cell REPL probes. All three images build. Maki pin remains
10d66991 until our full native gate/checker qualification. Dotb343e371 is closed
on the compiler reproducer and application acceptance, independently of release.

**Previous candidate:** `.jj-ws/cedar-compiler-acceptance`, source9d0a44a3,
SHA24d8075c4ac640df811daf736cec2ffaba7176471753d9a5d27ff031f31148c9.
Tender reports native/source suites and standalone+REPL gate pass on this pair;
its slot-membership failure was Tender's seven-key/six-key mask, now corrected.
Tender's checked callback composition uses a row R, retaining a five-cell proof;
no checking was disabled. General scalar-variable/product instantiation is a
separate P2 capability dot38b01472, not a new soundness blocker.

Integration `bin/hb` still points to older checkpoint SHA46921f52. Do not treat it
as the current source binary. Frozen acceptance workspaces must remain unchanged.

## Confirmed stripped quotation crash — Cedar owns

Dot `habu-relocate-quotation-code-f372a04a` under compiler campaign. Audit C5 is
now reproduced on54e9ae7c/c37b51ff, independent of finally and --repl images.
Source `/tmp/cedar-audit-quotation-app.f`:
```
package QUOTAPP
public
: INC ( n -- n ) 1+ ;
: APPLY ( n [ n -- n ] -- n ) execute ;
: RUN ( -- ) 41 [: INC ;] APPLY 42 <> if -9031 throw then ;
;package
: MAIN ( -- ) QUOTAPP:RUN ;
```
`/tmp/cedar-finally-reviewed --load tools/hb-build.f -- <source> -o <output>`
succeeds (AOT, engine stripped), but output exits139. Core1521112 showed PC/x9
0x1865de0 unmapped, LR0x401310 just after BLR x9 at0x40130c. Artifact is mapped
at0x400000. Extracted core was deleted. Logs `/tmp/cedar-audit-quotation-{build,run}.log`.

Fix both closure traversal and relocation using the emitter's address bitmap:
- aot-closure.f SCAN-REC currently follows direct BL only, missing tick-only targets;
- aot-lib.f COPY-COMPACT-BLOB copies four-word address literals unchanged;
- SNAP-RELOC:CHAINV/SET-CHAIN/constants are in aot-decl.f, and the live addrmap
  formula is in aot-capture.f ACAP-CHAIN-BIT?. Map records address sites, not kinds;
  distinguish code/DATA by recorded live extents, never unmarked numeric literals.
- Preserve blob sizes and emit position-independent code references. Validate
  chain shape/extent, retain address-only reachable words, refuse missing mappings.
- Regression must build/run default stripped binaries, including named ticks,
  anonymous/nested quotations, saved values, DATA/scalar controls. No --repl workaround.
No C5 source patch has been made yet. Peer review required before landing.

## Remaining closure

Full native gate on earlier engine ran353/353,351 passed. build-fixpoint is now
focused green; WID restore still has mismatched aggregate deadlines. Its eleven
builds plus data-span child cannot be budgeted as one build. data-span makes
several240s children under a240s parent; gate gives whole WID360s. Preserve cases
and distinguish timeouts from semantic failures; no fresh full green gate exists.

Public recovery dot a2551190 remains: partial recovery engine
`/tmp/cedar-compiler-fixpoint/tmp/hb-stdin` cannot build the full compiler
(REG-INCOMING?/TFAM:REG-AOT-MERGE-INCOMING?, -8286). BF native snapshot route
passes; bootstrap finalrefresh/seed/ddc/chainbake routes still need assessment.
Do not restore retired ENGINE-SNAP-XT hooks or obsolete hb-host test machinery.

Warmed source-order VERIFY dot0c9fe3d7 remains reproduced, design only. See PLAN.md
and the old RESTART.md in jj history for its scoped source-visibility overlay.

Audit `/tmp/habu-audit-2026-09-14.md` used oldc3e1b024/Sept10engine. C2/C4/M8 now
implemented; C5 confirmed above. Tested C1 and listed C3 exploits reject, broader
C3 remains unvalidated. C6 dispatch and M10 early-gate-stop were already fixed.
M1/M2 stack bounds, M3 ndict bounds, M4 literal overflow, M9 IR successors,
M11 environment cap and M13 zero UNMAP still require current evidence/fixes.
Original probes are in `/tmp/claude-1001/-home-joel-Work-habu/3954d386-699e-4c6e-b454-0300ac86489f/scratchpad/`.
