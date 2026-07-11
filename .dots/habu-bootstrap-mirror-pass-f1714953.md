---
title: Bootstrap mirror pass-2 + wide-marking parity
status: open
priority: 2
issue-type: task
created-at: "2026-07-09T13:28:23.589159+02:00"
---

Mirror the native width-aware pass-2 lowering (habu2.f EM-COMPILE-P2WIDE / EM-P2-CARVE / EM-P2-LOCREF / EM-P2-TRIGGER family) and the interpret wide-marking publish-tail call (EM-REC-WIDE-PUBLISH) into bootstrap/cg/forth.fs + jit.fs. TFAM 12 item (4) verdict 2026-07-09: parity is VACUOUS today, proven by corpus scan — no SUMTYPE/PRODUCT/TYPEFAMILY/ENUM declaration exists in src/, lib/, tools/, or maki/ non-test source (rg '^SUMTYPE |^PRODUCT |^TYPEFAMILY |^ENUM ' = 0 rows; only the implementation words in src/core/sumtype.f and the dispatch mirrors), so no definition compiled by a Gforth-recovered engine can carry a wide width fact before the immediate native fixpoint refresh replaces it (docs/bootstrap.md recovery contract). The boundary rots the day a wide layout family lands in the recovery corpus: implement this parity BEFORE (or with) the first non-test SUMTYPE/PRODUCT declaration. No current gate exercises the bootstrap emitter (tools/bootstrap.sh recovery is the only exposure).

## RE-VERIFICATION + TRIPWIRE (trigger condition fired: production ADTs landed)

The switchover campaign landed ELEVEN production declarations outside test
files: lib/adt/option.f:26 (SUMTYPE option 1), lib/adt/result.f:35 (SUMTYPE
result 2), lib/map.f:8 (ENUM slot-state) + :30 (SUMTYPE map-loc 0), lib/ffi.f:19
(ENUM kind), lib/process.f:12 (SUMTYPE outcome 0), lib/ptx/ir.f:23 (PRODUCT
ptxir-node 0), maki/fusion-plan.f:55 + maki/report.f:52/67/86 (ENUMs). The
original "no declaration anywhere in non-test source" proof is DEAD.

RECOVERY IS STILL SAFE — the narrower, sufficient invariant holds:
1. tools/bootstrap.sh SRC_COMMON (lines 65-92) compiles ONLY src/core +
   src/arch + src/os + src/habu with the Gforth stage-0 emitter; nothing from
   lib/, tools/, or maki/ enters that stage.
2. src/** non-test contains ZERO ADT declarations (line-start scan and lexed
   live-token scan both empty; the only keyword tokens are the grammar's own
   definition names in src/core/sumtype.f:1064-1131 and strings/comments).
3. The recovered engine immediately runs the native fixpoint refresh
   (bootstrap.sh lines 270-272: lib/* + tools/build-fixpoint.f install
   --force), so every lib/maki wide family is compiled by the REAL native
   pass-2 emitter, never by the gforth stage.

TRIPWIRE LANDED: tools/bootstrap-mirror-lint.f walks src/** (.f/.fs,
non-test), lexes each file (comments/strings skipped), and FAILS on any live
SUMTYPE/ENUM/PRODUCT/TYPEFAMILY token (definition-name and '/[']/postpone
escapes excluded, so sumtype.f's own definers stay silent), naming this dot in
the finding. Wired into the run.f lint-tools pool
(lint-tools/bootstrap-mirror, test/gate-stdlib-lint-tools.f) via
tools/bootstrap-mirror-lint-test.f (clean src walk = 0 findings over 67 files
+ planted-overlay red fixture + exclusion coverage). Red-first proven live: a
synthetic SUMTYPE planted in src/ -> 1 finding, exit 1; removed -> 0, exit 0.

REMAINDER (unchanged, now enforced): implement the stage-0 pass-2 +
wide-marking mirror in bootstrap/cg/forth.fs + jit.fs before (or with) the
first src/ ADT declaration — the tripwire makes that ordering a red gate
instead of a convention.
