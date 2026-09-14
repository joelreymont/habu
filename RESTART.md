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
- Hazel is available and reviewing retained NSTR pool ownership for stripped
  closures. Internal Astra agent integer_overflow completed dynamic object
  payload storage; checker_followup_review is repairing missing DATA-site bitmap
  publication at seed restore. Astra literal_ownership owns persistent NSTR
  ownership and the native-build source-owner handoff in cedar-literal-ownership.
  Prefix every message to another agent with your own live name,
  including one-liners; the shared Herdr skill now requires this.

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
- `2929bda4` + `9709aa17`: integer overflow admission and float-shaped overflow
  claiming. `220930e4`: matching checker integer admission. Hazel and Cedar
  reviewed; focused integer, float and checker tests pass on the combined engine.
- `180b7e63`: public ndict count bounds (M3), reviewed by Cedar; agent engine
  passes dictionary controls. It still needs the combined native rebuild/gate.
- `a5dfb5f8` + `37e78525`: stripped build and outer CLI execute in native build
  scope, with tier 1 selected before dependencies. `e9111ac2` removes obsolete
  maker entry/helpers. Cedar reviewed the Astra implementation. Focused CLI,
  tier, preseed, quotation, hook-registry and surface checks pass on their stated
  pre-C5 sources. Bookmark pushed through e9111ac2; C5 remains a working patch.
- `1fb297ec`: dynamic object codec/cache/link payload storage, reviewed by Cedar.
  Five optimized object suites pass; >512KiB text/data, aliased self-load/append,
  returned cache-key corruption and actual raw-object executable controls pass.
  Combined test/stripped-quotation.f now passes full hb-build and fresh execution.

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

Latest combined numeric engine: `/tmp/cedar-numeric-native`, SHA-256
`2a49a29c9804f00292652d45f0a32aa27e6f224034585501e890bf9d2acbc14c`,
source through220930e4. It includes integer engine/checker admission and the float
claiming fix. New integer, float, checker and native-feed focused suites pass.
It does not include M3. Agent M3 engine `/tmp/cedar-ndict-native`, SHAeaf37486,
does not include Cedar's checker integer admission; neither is a release pair.

Hazel's broad source-engine attribution reran23 suites against6ce2e21e and
e637ee61 with the same engine: no exit-code differences. Native-window-owner was
skipped (>5min). This is supplemental evidence, not a passing native release gate.

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
C5 working patch now follows marked code addresses and emits ADRP/ADDI/NOP/NOP
while preserving each16-byte site. It validates opcode/register/shift/extent,
retains address-only closure targets and allows a stable one-past DATA pointer.
Hazel reviewed these and the NSTR window lifecycle. NSTR:WINDOW-OPEN now follows
the BLOB-SRC latch so new application strings/trap messages live in restored DATA.
Direct native writer builds and runs QUOTAPP and test/stripped-quotation-subject.f;
the latter prints exactly `stripped-quotation: ok`. Logs/artifacts are under
`/tmp/cedar-c5-linker` and `/tmp/cedar-c5-subject`. New stripped-address test passes
seven malformed-chain/DATA/zero-register refusals and valid boundaries. Namespace
IDs and unaligned instruction interiors are now refused as code owners; both
were demonstrated failing before correction and now pass with valid-entry controls.

Object payload limits are fixed above (dot2e2a7bed). Hazel reduced a real retained
library failure: /tmp/hazel-c5/unmap-fail-subject.f calls MEM:UNMAP on an invalid
span. The engine prints `memory: unmap failed`; its stripped image builds0/exits71
but emits20 NUL bytes. Internal Astra confirmed the owner is seeded REGION code;
EM-AOT-RELOC-DATA fails to republish ADDRMAP bits (CODE/named passes do). Engine
text's201 records have only5 canonical scalar chains, so no separate engine-text
table is indicated. The seed fix/rebuild is active in cedar-native-stripped.
Dot31ba3e76 owns this plus persistent previous-pool row ownership. literal_ownership
must transfer the retained host's compiled literal rows into the new target NSTR
owner; target tables are otherwise empty even though its code uses host strings.
Do not relax DATA checks or claim a general stripped compiler until these close.

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
M1/M2 stack bounds remain source-confirmed (dot986147f9). M3/M4 are implemented
above but require combined qualification. Native dynamic +loop is missing
(dotdb5978a3, source reproducer throws E-NELAB-CTRL). M9 IR successors,
M11 environment cap and M13 zero UNMAP still require current evidence/fixes.
Original probes are in `/tmp/claude-1001/-home-joel-Work-habu/3954d386-699e-4c6e-b454-0300ac86489f/scratchpad/`.
