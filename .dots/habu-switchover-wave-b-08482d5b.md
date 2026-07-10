---
title: "Switchover wave B: option<tuple> + result<T,errno> process family"
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T22:18:57.005375+02:00"
---

docs/census-switchover.md section 5 wave B. The 25 multi-value+flag words (SPLIT-NEXT string.f:180, NEXT-LINE object.f:274 r8, RX-FIND* regex.f:454/464, LOAD object-index.f:116, JSONLF-*/JSONL-PARSE-ROW, PROC-ENV-DEFAULT0, FL-STRIP-SIGN, tool row parsers) to option<tuple>; the 34-site process rc family (PROC-WAIT-RC r17, PROC-RUN-RC r11, RUN-CAPTURE/PROC-CAPTURE-RC@ len-len-rc + process-env/cwd/argv mirrors) to result<T,errno> over E-PROC-*. Raw habu1.f emitters (BRUNRC/BPIPE/BPOLL/...) stay rc-sentinel at the trusted boundary; only checked wrappers migrate. DEPENDS: wave A patterns proven, item 12 multi-cell.

## FOUNDATION — LANDED (lib/adt/result.f + checker-proven)

`lib/adt/result.f` built: `SUMTYPE result 2 / VARIANT ok a ;VARIANT / VARIANT err
b ;VARIANT / ;SUMTYPE`, mirroring option.f's lib/adt/ subdir-exempt-but-public
placement (RESULT:OK / RESULT:ERR are public dictionary words; not manifest-listed;
require before consumers). Focused proof lib/adt/result-test.f gated via a new
`adt-result` TEST:SUITE in test/gate-stdlib-cases.f. Both files registered in
FILEMAP. NO new trust rows (result-test reuses the existing test/checker-assert.f
TRUSTED: helper). LIGHTER gate class (lib/adt on-demand, no byte-fixpoint).

CHECKER SUPPORTS 2-PARAM FAMILIES — proven, NOT blocked:
- Construct: `RESULT:OK ( a -- result<a,b> )`, `RESULT:ERR ( b -- result<a,b> )`.
- Eliminate: `MATCH result ok OF … ENDOF err OF … ENDOF ;MATCH`, each arm binds
  its own payload (ok→a, err→b).
- Negatives reject (CHECK-QUIET-CANDIDATE! code 0): OK given the ERR type, ERR
  given the OK type, and a MATCH whose err arm leaks the err payload where the ok
  type is declared. Positives accept (-1). result-test asserts all of these.

MULTI-PARAM SUMTYPE SYNTAX GOTCHAS (record for wave B/C/D authors):
1. PARAM ORDER: payload params bind in first-use declaration order — param a (0)
   MUST be introduced before param b (1). Declaring `err b` before `ok a` throws
   E-SUMTYPE-DECL (7107) — that was the orchestrator's failed probe. Declare the
   ok/a arm first. (Documented in result.f header.)
2. INTERPRET-MODE LAYOUT VALUE: a constructed result (like any layout/sum value)
   CANNOT sit on the interpret-mode top-level stack — `5 RESULT:OK` at the top
   level throws `interpret-mode layout value`. All construct+MATCH must run INSIDE
   a compiled word (result-test wraps its runtime checks in RT-RUN). This is the
   same rule option consumers already follow.
3. TYPE-PARAM WIDTH: a single-type param up to a counted pointer works —
   `result<n,n>` and `result<ptr u8, n>` both check. But a MULTI-CELL TUPLE param
   (two independent values as one param, e.g. `result<ptr u8 n, n>` for a
   (buf,len) success payload) does NOT check yet — it throws at the extra token.
   => the len-len-rc CAPTURE words (RUN-CAPTURE, PROC-CAPTURE-RC@, process-env/
   cwd/argv mirrors → result<(outlen,errlen),errno>) stay BLOCKED on item-12 /
   a product-typed payload param. SCALAR-rc words are unblocked TODAY.

## OPTION vs RESULT BOUNDARY (the wave-A/B split)

Rule: the flag decides the family. If the flag only says PRESENT vs ABSENT (one
failure mode, no reason) → `option<T>` (wave A, done). If the flag/rc distinguishes
SUCCESS-with-a-value from FAILURE-with-a-REASON (an errno, an error kind, distinct
E- codes) → `result<T,E>` (wave B). Most census §1b value+bool words were pure
absence and are already option<T>. The genuine result<T,E> population is narrow and
dominated by ONE cluster:

GENUINE WAVE-B result CANDIDATES:
- The process rc/errno family (result<exit,errno> over E-PROC-*, roles.f:65 >RC,
  rc≥0 = exit code = ok, rc<0 = -errno = err): PROC-OUTCOME>RC (kind code -- rc,
  r10), PROC-STATUS>RC (n -- rc, r4), PROC-WAIT-RC (pid -- rc, r23),
  PROC-RUN-RC (r11), PROC-RUN-IO-RC (r4). All return a SINGLE-cell rc →
  result<n,n> works TODAY (no item-12).
- The value+rc CAPTURE words: RUN-CAPTURE (r48), PROC-CAPTURE-RC@ (r4), and the
  process-env/cwd/argv mirrors — these carry a (len,len) success tuple →
  result<(outlen,errlen),errno>, BLOCKED on item-12 multi-cell payload params
  (see gotcha 3). Defer until item-12 / product payload lands.
- Raw habu1.f emitters (BRUNRC/BPIPE/BDUP2/BFCNTL/BPOLL/BKILL/BSETPGID/BWAITRC)
  stay rc-sentinel at the trusted boundary — NOT migrated (only checked wrappers).
- NOT result (stay option / already wave A): every §1b value+bool whose flag is
  mere absence — SPLIT-NEXT, NEXT-LINE, RX-FIND*, LOAD, the JSON row parsers,
  PROC-ENV-DEFAULT$?, FL-STRIP-SIGN — become option<tuple> once item-12 lands, not
  result. (Wave B's option<tuple> half also waits on item-12 for the tuple payload.)

## FIRST WAVE-B TARGET (recommended): PROC-RUN-IO-RC

`PROC-RUN-IO-RC ( ptr u8 len fd fd fd -- rc )` → `result<n, n>` (ok = process exit
code, err = errno). WHY FIRST: lowest-radius genuine result word that is UNBLOCKED
(single-cell rc, no item-12) and does NOT cascade — it sits at the TOP of the rc
chain (`PROC-SPAWN-IO PROC-WAIT-RC`), so migrating it wraps PROC-WAIT-RC's rc
sentinel at its own boundary and leaves the wide PROC-WAIT-RC (r23) untouched.
Exactly TWO callers: lib/build.f:205 (real) and lib/process-test.f:322 (test),
both currently `PROC-RUN-IO-RC RC>N …` — rewrite to `MATCH result ok OF … ENDOF
err OF … ENDOF ;MATCH`. PLAN: (a) body = `PROC-SPAWN-IO PROC-WAIT-RC RC>N dup 0 <
if negate >? RESULT:ERR else RESULT:OK then` wrapping the rc at the boundary (pick
the exact ok/err split from the rc encoding — ok = exit code, err = -errno); (b)
require lib/adt/result.f in lib/process.f (process.f IS a TR-GATE-HARNESS-FILES
member — ALSO declare lib/adt/result.f in test/run-files.f TR-GATE-HARNESS-FILES
before lib/process.f, the same closure-lint fix slice 4 did for option+fs.f);
(c) manifest row for PROC-RUN-IO-RC → result<n,n>; (d) T{ }T both arms; (e) after
it proves out, climb the chain PROC-WAIT-RC → PROC-RUN-RC → PROC-STATUS>RC →
PROC-OUTCOME>RC in dependency order, each wrapping the still-sentinel lower word.
NEXT-NEXT: the outcome sum (exited/signaled/timeout) is Wave C, not B — B stops at
result<exit,errno>.

## SLICE 1 — LANDED (lib/process.f PROC-RUN-IO-RC, first result<n,n> word)

`PROC-RUN-IO-RC ( ptr u8 len fd fd fd -- rc )` → `( … -- result<n,n> )`.

ERRNO BOUNDARY FINDING (answers the scope's open question): the errno is NOT
available at this boundary. OS-level spawn/wait failures do not RETURN a negative
rc — they THROW E-PROC-SPAWN inside PROC-SPAWN-IO. The rc that PROC-RUN-IO-RC
returns is always ≥0 (PROC-OUTCOME>RC: exit→code, signal→128+sig). So the genuine
distinction the return carries is success vs the process's own failure code, NOT
success vs errno. MAPPING CHOSEN: **ok = clean exit (rc 0); err = the nonzero
completion rc** (a nonzero exit code, or 128+signal). This matches how both callers
already interpret rc (rc==0 = success), gives clean caller code (the err arm
handles ALL failures — build.f dropped its extra `rc 0 <> if throw`), and err
carries a real distinguishing reason. (result<exit,signal> via PROC-WAIT-OUTCOME
was the alternative — rejected: it forces callers to re-check the exit code and
buries success/failure, and its signal arm is hard to trigger portably. The
exit-vs-signal split belongs to the Wave-C outcome sum.)

Body wraps the sentinel at the boundary:
`PROC-SPAWN-IO PROC-WAIT-RC RC>N {: rc:n :} rc 0 = if rc RESULT:OK else rc
RESULT:ERR then`. Callers (exactly 2, swept lib/tools/test/examples/maki/src):
- lib/build.f BUILD-RUN — `PROC-RUN-IO-RC RC>N … rc 0 <> if E-BUILD-STATUS throw`
  → `PROC-RUN-IO-RC` then `nullfd FD>N close` then `MATCH result ok OF … ENDOF err
  OF drop E-BUILD-STATUS throw ENDOF ;MATCH {: rc:n :}`. BUILD-RUN keeps its
  `( … -- n )` sig (it has several callers) — only its body changed.
- lib/process-test.f TEST-RUN-IO-CAT — `/bin/cat` (exit 0) → MATCH ok(0). ADDED
  TEST-RUN-IO-FALSE — `/usr/bin/false` (exit 1) → MATCH err(1) — a DIRECT both-arm
  test (BUILD-RUN's ok/err arms are already covered by build-test BT-CMD-OK /
  BT-CMD-FAIL).

CHECKER LESSONS for result callers (learned building this slice):
- A `result<…>` value CANNOT be bound to a typed local — `{: r:result<n,n> :}`
  throws "unknown type ':}'" (the local-type grammar doesn't parse the parametric
  `<…>`). Keep the result on the data stack and MATCH it directly.
- A result value DOES survive an intervening stack op below it (build.f runs
  `nullfd FD>N close` with the result sitting on the stack, then MATCHes it — OK).
- An `if … RESULT:OK else … RESULT:ERR then` unifies both branches to result<n,n>
  cleanly (that is the constructor body shape).
- Same interpret-mode rule as option: construct/MATCH only inside a compiled word.

Manifest row → result<n,n>. process.f is a TR-GATE-HARNESS-FILES member, so
lib/adt/result.f declared in test/run-files.f TR-GATE-HARNESS-FILES (before
lib/process.f) — the closure-lint edge, exactly like slice 4's option+fs.f.
process.f uses the `s" path" required` dep style, so the require is
`s" lib/adt/result.f" required`. NO byte-fixpoint (process.f is on-demand, not in
the boot prefix; test/run.f fixpoint phase ran at normal 20.9s). NO new trust rows,
reused the shared result family. NEXT: climb to PROC-WAIT-RC (r23) / PROC-RUN-RC.
