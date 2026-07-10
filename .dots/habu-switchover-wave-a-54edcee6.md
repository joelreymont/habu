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

## SLICE 2 — LANDED (lib/date.f DATE-N)

`DATE-N` (lib/date.f) `( ptr u8 n n -- n bool )` → `( ptr u8 n n -- option<n> )`:
SOME parsed fixed-width decimal field, NONE on a non-digit (wraps the internal
digit-loop sentinel at the boundary). Its THREE in-file callers in `PARSE-YMD`
(the Y/M/D field parses) rewritten from `DATE-N 0= IF drop ... exit THEN X !` to
the guard-and-continue MATCH shape `MATCH option none OF 0 0 0= 0= exit ENDOF some
OF X ! ENDOF ;MATCH` (verified an `exit` inside a MATCH arm works — the common
early-return migration shape). DATE-N test (tools/stdlib-date-test.f DATE-N-OK/BAD)
rewritten to MATCH both branches; DATE-N manifest row updated to the option<n> sig
(public-sig renders it). Reused the shared `lib/adt/option.f` — NO new public
family, no WID pressure. Gate class LIGHTER (date.f on-demand, no byte-fixpoint).
NO new trust rows. PARSE-YMD (its own return; WIDE external radius — trust-lint,
stale-status-lint) deferred to dot habu-switchover-parse-ymd-b3d81c47.

## SLICE 3 — LANDED (lib/map.f MAP-GET)

`MAP-GET` (lib/map.f) `( ptr a count ptr u8 len -- n bool )` → `( ... -- option<n> )`:
SOME value if the key is present, else NONE (wraps the MAP-LOCATE found/not-found
sentinel at the boundary). Callers rewritten to MATCH: `MAP-HAS?` (in-file:
`MAP-GET nip` → MATCH → bool), the test wrapper `MT-MAP-GET` + the assertion
helpers `MT-ASSERT-HIT`/`MT-ASSERT-MISS` (so the existing hit/miss cases now
exercise found→some / absent→none AND MAP-HAS? both branches), and
`examples/file-map.f` (FM-INC / FM-COUNT did `MAP-GET if` → MATCH). MAP-GET
manifest row updated to option<n>. Reused shared option — NO new public family.
Typed the touched locals (MAP-GET `cap:count`/`len:len` — the count/len ROLES,
not `n`, so MAP-LOCATE's input still matches). Gate class LIGHTER (map.f on-demand,
no byte-fixpoint). NO new trust rows.

CAVEAT for future slices: caller radius must include `examples/` — `examples/
file-map.f` is a real MAP-GET caller that lib/tools/maki/src greps miss. A jj
op-log divergence (concurrent fable moves) also reset the working copy mid-slice
and reverted the edits twice; re-applied from the recorded diffs.

## SLICE 4 — LANDED (lib/fs.f FS-TRY-*STAT-MODE pair)

`FS-TRY-STAT-MODE` / `FS-TRY-LSTAT-MODE` (lib/fs.f) `( ptr u8 n -- n )` returning
`-1` → `( ptr u8 n -- option<n> )`: SOME stat/lstat mode when statable, else NONE
(wraps the internal `FS-TRY-STAT if FS-STAT-MODE@ else -1` sentinel at the
boundary). Migrated the mirror PAIR together to keep fs.f internally consistent.
All FOUR callers are in-file: `STAT-MODE` (`dup 0 < if throw` → MATCH with a
`none OF E-FS-STAT throw ENDOF` / empty `some OF ENDOF` unwrap — proves `throw`
is bottom in a MATCH arm, like `exit`), and `FILE?` / `DIR?` / `SYMLINK?` (each
`dup 0 < if drop FS-FALSE else <bit-test> then` → `none OF FS-FALSE ENDOF` /
`some OF S-IFMT and S-IF* = ENDOF`). No sentinel comparison left in fs.f. Both
manifest rows updated to `(ptr u8 n -- option<n>)`. Direct found→some / absent→none
tests for both finders added to `FS-TEST-INTERNALS`; caller both-branch coverage
already exists (FS-TEST-FILE-DIR / FS-TEST-PATHS for STAT-MODE/FILE?/DIR?,
fs-mutate-test SYMLINK? cases). Reused shared `lib/adt/option.f` — NO new public
family. Picked the FS pair over STR>NUMBER? (r16, chained through STR-PARSE-*),
FIND-EXECUTABLE* (7 hits) and FIND-TAG (throws, not a sentinel finder).

CLOSURE NOTE (the extra edit beyond lib/): `lib/fs.f` is a `TR-GATE-HARNESS-FILES`
member (test/run-files.f), so the result-cache CLOSURE-LINT
(test/run-result-cache-test.f) requires every file fs.f `require`s to be a
declared member of that set. Adding `require lib/adt/option.f` to fs.f made the
lint fire `result-cache closure: lib/fs.f -> missing lib/adt/option.f` (twice:
DEBUG + AOT-NEG sets). Fixed soundly by adding `lib/adt/option.f` to
`TR-GATE-HARNESS-FILES` — it now content-keys the harness cache too, so an
option.f edit correctly invalidates fs.f-dependent phases. This is why slices 1–3
(map/date/float, not harness members) needed no run-files.f edit; any FUTURE
switchover of a harness-closure lib (process*, content-key, test/*) must likewise
declare its new option.f edge in TR-GATE-HARNESS-FILES. Gate class LIGHTER (fs.f
on-demand, no byte-fixpoint). NO new trust rows.

## SLICE 5 — LANDED (lib/float.f FL-SIG, first option<r>)

`FL-SIG` (lib/float.f) `( ptr u8 n -- r bool )` → `( ptr u8 n -- option<r> )`:
SOME significand (unsigned mantissa with optional fraction), NONE if there are no
digits / a bad half (wraps the two internal `FL-DIGITS>F` bool sentinels at the
boundary — `iok`/`fok` still bool inside FL-SIG; only FL-SIG's OWN return
migrated, FL-DIGITS>F stays value+flag for a later slice). FIRST **option<r>**
instantiation (payload is a float role `r`, not `n`/`idx`) — the checker accepts
it exactly like option<n>/option<idx>; no layout issue (1 cell). ONE in-file
caller `STR>FLOAT`, rewritten from `FL-SIG {: sok :} … sok 0= or … if drop …` to
`MATCH option none OF 0.0 0 0= 0= exit ENDOF some OF ENDOF ;MATCH` — the empty
`some` arm leaves the significand `r` on the stack, then the remaining
`u 0= FL-VALID @ 0= or` guard and the exponent multiply continue unchanged. No
sentinel test left in the migrated path.

Picked FL-SIG over: STR>FLOAT itself (its callers include `maki/golden-artifact.f`
— forbidden lane) and FL-DIGITS>F (two callers, both combine into FL-SIG needing
nested double-MATCH — messier). FL-SIG has the single clean caller. float.f is in
NO TR-*-FILES set, so NO run-files.f closure edge (unlike fs.f) and it already
`require`s lib/adt/option.f from slice 1 — NO new require, NO new public family.

Manifest FL-SIG row → `(ptr u8 n -- option<r>)`. Direct FL-SIG test added
(lib/float-test.f FL-RUN-SIG: T-FS some(r)~want for 3.14/100/.5/5./0, T-FS-BAD
none for ""/"."/"abc"/"1.2.3"); the STR>FLOAT FL-RUN cases still green
(behavior preserved). typed-local-diff-lint forced typing the pre-existing bare
`u` on the FL-SIG def line I touched → `u:n` (byte-string length role; matches
slice-1's FL-FIND-E / FL-PARSE-EXP `{: a:ptr u:n :}` in the same file). Test local
`{: want:r :}` typed to the float role (precedent lib/json-read-test.f). Gate class
LIGHTER (float.f on-demand, no byte-fixpoint). NO new trust rows.

CAVEAT for future float slices: STR>FLOAT can't migrate to option<r> here — its
maki-lane caller (golden-artifact.f) is off-limits to the tfam lane; that
migration needs cross-lane coordination or a dot. FL-DIGITS>F is the remaining
in-float leaf finder.

## SLICE 6 — LANDED (lib/float.f FL-DIGITS>F; float finders complete)

`FL-DIGITS>F` (lib/float.f) `( ptr u8 n -- r bool )` → `( ptr u8 n -- option<r> )`:
SOME digit-run value (empty string is valid → SOME 0.0), NONE on a non-digit
(wraps its own bool sentinel). ONE caller `FL-SIG` (in-file), rewritten from two
`{: iok :}`/`{: fok :}` bool binds + a combined guard to a guard-then-two-MATCHes
shape: `ilen flen + 0= if OPTION:NONE exit then` (the "no digits at all" case that
rejects "" and ".") followed by `a ilen FL-DIGITS>F MATCH … none→NONE some→(ival
on stack)` then `fa flen FL-DIGITS>F MATCH … none→drop-ival-NONE some→(ival fval
on stack)` then `flen POW10 f/ f+ OPTION:SOME`. No bool sentinel from FL-DIGITS>F
left inside FL-SIG — float's internal finder chain is now fully option<r>.
Manifest row → option<r>. Direct FL-DIGITS>F test added (FL-RUN-DIGITS: some for
123/0/empty, none for abc/1.2/12x). Typed the touched `u:n` on the def line.
LIGHTER (float.f on-demand). NO new trust rows. (Migration authored here; a
process-spawn SIGKILL flake under gate contention stalled the final test/run.f, so
the orchestrator gated + committed it — b43dd57f.)

WORKSPACE-STALE LESSON: a concurrent tfam move (parallel src/core lane) can turn
the workspace stale MID-SLICE — `jj st`/`jj diff` then show EMPTY while edits sit
unsnapshotted on disk (0-byte patch is the tell). Recovery: back up the edited
files to HB_TMP FIRST, run `jj workspace update-stale` (it RESETS working-copy
files to the new head, reverting the slice), then `jj new <newhead>` and restore
the backups (verified the concurrent op didn't touch the same files via
`jj file show -r <newhead>` diff before restoring). Then re-run the whole gate on
the new base. Abandon the divergent empty leftover.

## SLICE 7 — LANDED (lib/array.f A-FIND-INDEX / A-FIND-INDEXI pair)

`A-FIND-INDEX` / `A-FIND-INDEXI` (lib/array.f) `( ptr a len [ q ] -- n )` returning
`-1` → `( … -- option<idx> )`: SOME first matching index (`i >IDX OPTION:SOME
unloop exit` inside the `?do`), else `OPTION:NONE`. Migrated the mirror pair
together. Callers: ONE external (`examples/array.f` `AE-FIRST-GT-SIX`, rewritten to
`MATCH option none OF -1 ENDOF some OF IDX>N ENDOF` — the slice-3 examples/ sweep
rule caught it) plus the array-test wrappers `AT-A-FIND-INDEX`/`INDEXI` (kept `-- n`
with an internal MATCH re-wrap so the def lines stay OUT of the diff — minimal
churn, all existing `-1 T=`/`0 T=`/`2 T=` assertions and the two E-A-BOUNDS
neg-length throw tests unchanged). Added DIRECT option assertions
(AT-A-FIND-SOME / AT-A-FIND-NONE: found→some(idx), absent→none). Both manifest rows
→ option<idx>. array.f is in NO TR-*-FILES set → NO run-files.f closure edge;
added `require lib/adt/option.f` to array.f (its first require — it followed the
comment-dep convention). NO master overlap (array finders identical to master).
Typed the touched `len:len` on the two def lines; the `q` predicate-quotation
local kept bare under a `\ typed-local-lint: allow-bare-local` directive (the
lint's group-exempt mechanism; matches GSI-RUN/gate-common-lib precedent). LIGHTER
(array.f on-demand). NO new trust rows.

WAVE-A STATUS: the genuinely clean low-radius leaf finders are now largely
exhausted (float ×3, date, map, fs pair, array pair done). Remaining wave-A
finders are all entangled or off-lane: STR-PARSE-POS/NEG only feed STR>NUMBER?
(r16) so migrating them just pushes the sentinel up to STR>NUMBER?'s 16 external
callers (defer with STR>NUMBER?); MAP-INDEX/PROBE are heavily map-internal
(12/7 hits); HM-PROBE is NOT a missing-sentinel finder (always returns a valid
slot; the -1 is a loop flag); FIND-EXECUTABLE* is a process-env harness-member
cascade (FIND-EXECUTABLE→…-IN-PATH→PROC-TRY-PATH-SEG, ~7 cross-file);
ACAP-POOL-FIND is src/habu (byte-fixpoint / Wave-E bootstrap-sensitive);
FIND-TAG throws (not a sentinel); STR>FLOAT/PARSE-YMD/FIND-SUB/INDEX-OF already
deferred. RECOMMENDATION: the next slice should either take a deferred WIDE
finder WITH a caller-rewrite plan (INDEX-OF r21 is the most mechanical: callers
do `0 >= `/`0 < ` tests) or PIVOT to wave-B scoping (result<T,E> shared family).
