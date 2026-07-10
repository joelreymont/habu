---
title: "Switchover wave A: option<scalar> + option<idx> over sentinels"
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T22:18:57.000713+02:00"
---

docs/census-switchover.md section 5 wave A. After items 8+9+12: migrate the 65 single-value+flag parser/lookup words (STR>NUMBER? string.f:230 r16, STR-PARSE-POS/NEG, DATE-N, PARSE-YMD, MAP-GET map.f:206 r7, FL-*/STR>FLOAT, FIND-EXECUTABLE*, PTXIR-FIND, tools imgdump/imagedisasm/date/json/trusted-inventory parsers) and ~15 -1-index finders (FIND-SUB string.f:81 r27, INDEX-OF :92 r21, A-FIND-INDEX(I), FIND-TAG, HM-PROBE, MAP-INDEX/PROBE, ACAP-POOL-FIND, FS-TRY-*STAT-MODE) to option<T>. Callers rewritten to MATCH. Full site list + radii in the census. DEPENDS: items 8, 9, 12.

## FIRST SLICE — LANDED (campaign PHASE 2 pivot)

Shared family: `lib/adt/option.f` — `SUMTYPE option 1 none | some a`. PLACEMENT
decision: lib/adt/ SUBDIR, not flat lib/. Rationale: the published-word stdlib
manifest (lib/std.manifest) models WORDS with hand-written sigs, not TYPE families
(a type + generated OPTION:SOME/NONE constructors); flat lib/ would demand a
module row for a word-set that does not exist. lib/adt/ (2 slashes) is exempt from
the manifest coverage walk exactly like lib/ptx/ and lib/layout/, WITHOUT making
option private — the constructors are public dictionary words, public-sig-rendered,
resolvable from any consumer. Listing ADT type modules in the manifest would need a
schema extension (dot if wanted). LOAD-ORDER convention (documented in the file
header): a consumer `require lib/adt/option.f` FIRST; option is ONE shared arity-1
public family declared once per session (well under the WID cap).

Migrated finder: `FL-FIND-E` (lib/float.f) `( ptr u8 n -- n )` returning -1 →
`( ptr u8 n -- option<idx> )` (SOME index of e/E, else NONE). ONE caller,
`FL-PARSE-EXP`, rewritten from a `epos 0 <` sentinel test to
`MATCH option none OF ... ENDOF some OF IDX>N ... ENDOF ;MATCH`. No `-1` left in the
migrated path. Picked FL-FIND-E over FIND-SUB/INDEX-OF (widest radius, later) and
A-FIND-INDEX (zero real callers): self-contained, one caller, clean sentinel test,
on-demand lib. Its manifest row (FL-FIND-E is a published word) updated to the new
sig — which confirmed the item lane's public-sig renders `option<idx>` returns
(the readiness-audit "soft dep" is working).

Tests (lib/float-test.f): FL-FIND-E found→some(idx)/absent→none; FL-PARSE-EXP both
branches (with/without exponent); STR>FLOAT exponent cases still green (behavior
preserved). Gate class: LIGHTER — float.f/option.f are on-demand libs, not in the
boot prefix, so no engine change and NO byte-fixpoint. NO new trust rows.
Registered: FILEMAP (option.f) + manifest (FL-FIND-E row) + the existing
float-parse gate suite. Remaining ~79 wave-A sites are later slices.
