---
title: Bootstrap mirror pass-2 + wide-marking parity
status: open
priority: 2
issue-type: task
created-at: "2026-07-09T13:28:23.589159+02:00"
---

Mirror the native width-aware pass-2 lowering (habu2.f EM-COMPILE-P2WIDE / EM-P2-CARVE / EM-P2-LOCREF / EM-P2-TRIGGER family) and the interpret wide-marking publish-tail call (EM-REC-WIDE-PUBLISH) into bootstrap/cg/forth.fs + jit.fs. TFAM 12 item (4) verdict 2026-07-09: parity is VACUOUS today, proven by corpus scan — no SUMTYPE/PRODUCT/TYPEFAMILY/ENUM declaration exists in src/, lib/, tools/, or maki/ non-test source (rg '^SUMTYPE |^PRODUCT |^TYPEFAMILY |^ENUM ' = 0 rows; only the implementation words in src/core/sumtype.f and the dispatch mirrors), so no definition compiled by a Gforth-recovered engine can carry a wide width fact before the immediate native fixpoint refresh replaces it (docs/bootstrap.md recovery contract). The boundary rots the day a wide layout family lands in the recovery corpus: implement this parity BEFORE (or with) the first non-test SUMTYPE/PRODUCT declaration. No current gate exercises the bootstrap emitter (tools/bootstrap.sh recovery is the only exposure).

## RE-VERIFICATION + TRIPWIRE (trigger condition fired: production ADTs landed)

The switchover campaign landed TEN production declarations outside test
files: lib/adt/option.f:26 (SUMTYPE option 1), lib/adt/result.f:35 (SUMTYPE
result 2), lib/map.f:8 (ENUM slot-state) + :30 (SUMTYPE map-loc 0),
lib/process.f:12 (SUMTYPE outcome 0), lib/ptx/ir.f:23 (PRODUCT
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

Claim: agent=adt-fix workspace=.jj-ws/habu-bootstrap-mirror-pass-f1714953 (RELEASED 2026-08-21: workspace gone, no live lane - gc)

FINDINGS RETIRED AS VACUOUS, MIRROR STILL NOT BUILT (2026-08-06, adt-fix).
All 56 findings named files under src/compiler, and src/compiler is NOT in the
recovery corpus. The lint was walking all of `src`, while its own header already
said the corpus is "SRC_COMMON: src/core + src/arch + src/os + src/habu". The
domain was wrong, not the rule.

THE INVARIANT, RE-DERIVED FROM THE TREE:
1. tools/bootstrap.sh has exactly ONE file-list array, SRC_COMMON (:69-102). It
   is a closed, explicitly ordered list. The only directories any path in the
   whole script names are src/arch/arm64, src/core, src/habu, src/os,
   src/os/linux, src/os/macos (plus lib/ and tools/ for the POST-recovery
   fixpoint refresh). `src/compiler` appears nowhere in the script.
2. The list cannot be widened implicitly by requires: the stage-0 sources carry
   ZERO `require` lines (checked roles.f, layout.f, habu1.f, habu2.f, xref.f,
   enums.f, combinators.f), and nothing under src/core, src/arch, src/os or
   src/habu requires anything from src/compiler. The shell script is the whole
   dependency graph.
3. So no src/compiler declaration can reach the unmirrored gforth pass-2, and
   the 56 findings described a hazard that does not exist.
4. Independently, the stage is ARMED rather than silent about wide layouts:
   bootstrap.sh bootstrap_wide_gate runs test/bootstrap-wide-memory.fs plus two
   fixtures that must exit rc 70 with marker BOOTSTRAP-WIDE-ARMED. A wide fact
   reaching the stage-0 emitter is refused, not miscompiled.

THE REPAIR IS A DOMAIN NARROWING PLUS A NEW DRIFT GUARD, NOT A LOOSENING. The
ADT rule is untouched. RUN now walks the four corpus roots (a strict superset of
SRC_COMMON, so it can over-report but never under-report), and CORPUS-DRIFT-CK
reads SRC_COMMON back out of tools/bootstrap.sh and reds on any entry outside
those roots - so adding a src/compiler file to the stage-0 list turns this lint
red at once instead of silently dropping that file from the scan. bootstrap.sh
stays the single authority on what the corpus is.

FALSIFIED, NOT ASSUMED: planting `src/compiler/ir/id.f` into SRC_COMMON makes
the guard report "SRC_COMMON entry `src/compiler/ir/id.f` lies outside the roots
this lint scans", 1 finding; removing it returns 0. The pre-existing
planted-overlay red fixture still fires its 4 findings, so the ADT rule is still
proven live.

EVIDENCE FROM THE REAL THING: HABU_ALLOW_BOOTSTRAP=1 tools/bootstrap.sh ran
green end to end on this tree WITH all 56 declarations present - exit 0,
"bin/hb refresh OK: compiler fixpoint", "bootstrap OK: bin/hb", self-check
census 0 uncheckable / 0 rejected / 4297 certified - and the engine reconverged
BYTE-IDENTICALLY (79cb84e8450352cc before and after). That is the invariant
holding in production, not a reading of it.

REMAINDER UNCHANGED: the stage-0 pass-2 + wide-marking mirror in
bootstrap/cg/forth.fs + jit.fs is still NOT implemented. It is still required
before the first ADT declaration in a file the recovery corpus actually
compiles, and the tripwire now guards exactly that set.
