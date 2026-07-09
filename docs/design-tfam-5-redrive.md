# TFAM 5 — Ordered-event redrive: candidate decision

Design decision for the blocked half of PLAN.md item 5 (D3 + D4), recorded in
`.dots/habu-tfam-5-ordered-4048c839/habu-tfam-5-event-d7618516.md`.

Static invariant: the checker's pre-run dependency closure for a file **must
never be a strict subset of the set of files the runtime actually loads for that
file**. If it is, the checker verifies against fewer support definitions than
runtime supplies, and either rejects good code (fail-closed, breaks green gates)
or — for content-addressed cache keys — hands back a stale artifact (a real
soundness bug). The enforcing boundary is the *event/closure producer* consumed
by `tools/check-core.f` preverify, `tools/check-all-errors-core.f` redrive,
`tools/event-closure-lib.f` (`EC:BUILD`), `tools/hb-build-lib.f` keys, and
`tools/public-signatures-core.f` pre-scan.

The blocker is that the intended producer — `DISCOVER:RUN`
(`tools/source-discovery.f`) — is a *body-skipping* lexer, so it under-approximates
the closure for the dominant real idiom and is strictly less sound than the
whole-file token scan it was meant to replace.

---

## 0. Reproduced facts (probe evidence)

All probes run against the committed `bin/hb` (`115831` bytes, built Jul 4).

### 0.1 DISCOVER is blind to colon-body loaders; the static scan is not

Fixture `e1.f`: a colon-wrapped guarded `required`, bare-called — the exact shape
of `tools/check-all-errors-core.f:8` `CA-MAYBE-VERIFY-SOURCE`:

```
: MAYBE ( -- ) s" src/habu/verify-source.f" required ;
MAYBE
```

`DISCOVER:RUN` on `e1.f`:

```
idiom1 colon-wrapped required: EVENT-COUNT=0     <- dep INVISIBLE
idiom2 bare top-level required: EVENT-COUNT=1     <- dep seen
```

`tools/check-core.f` `CHK-EXPAND-PATH` (whole-file `CHK-SCAN-DEPS`, lines 468-473)
on the same colon-wrapped file (pointing at a real file so `FILE?` succeeds):

```
idiom1 colon-wrapped required via CHK-SCAN-DEPS:
  CHK-DEP-ORDER-N=2
  CHK-DEP-N=2
    dep: .../p1.f
    dep: lib/errors.f            <- dep CAUGHT
```

Mechanism: `CHK-SCAN-DEPS` iterates *every* lexed token `0..L#` and matches
`require`/`required` regardless of nesting (`tools/check-core.f:468-473`, feeding
`CHK-SCAN-REQUIRE`/`CHK-SCAN-REQUIRED` at 460-466). `DISCOVER` deliberately skips
colon bodies: `SD-STEP` dispatches `:` to `SD-COLON`
(`tools/source-discovery.f:198`), which calls `SD-SKIP-BODY` (168-173) to consume
everything up to `;`. So the guarded loader inside `MAYBE` is never walked.

This is the whole blocker, proven: swapping `CHK-SCAN-DEPS` → `DISCOVER` drops a
dep the runtime loads.

### 0.2 Real load-and-record *does* observe colon-body loaders — because it runs them

`EVENT-ON` (not discovery), `included` a valid colon-wrapped entry under `catch`:

```
idiom1 real load-and-record (colon-wrapped valid): catch rc=0
  EVENT-COUNT=2
    ev kind=0 path=.../lr1.f          (the entry's own included event)
    ev kind=1 path=lib/vector.f       (the colon-wrapped required, recorded)
```

The loader words record before loading: `required`
(`src/core/include.f:287-294`) calls `EVENT-RECORD` then `INCLUDE-LOAD`. During a
real load the body of `MAYBE` actually executes, so the guarded `required` fires
and is recorded. This is the only way to see a colon-body loader without static
dataflow — and it is exactly candidate (a)/(b).

### 0.3 Real load of an *invalid* file is not catchable — it hard-faults

The whole reason `check` exists is files that do not compile. An undefined word
reached through `included`/`evaluate` does **not** raise a catchable throw; it
trips the in-binary signal handler (`bootstrap/cg/forth.fs:42`, "register dump on
signal"):

- `bin/hb --load broken-entry.f` where `broken-entry.f` is
  `s" lib/vector.f" required` then `: BAD totally-undefined-word-xyz ;`, wrapped
  in a child driver that turns `EVENT-ON`, `included` under `catch`, then prints
  `CHILD-REACHED-EMIT` → the child prints a **register dump and never reaches the
  emit line**.
- The same undefined word via a *top-level* `--load` reports cleanly
  (`E-UNDEFINED: totally-undefined-word-xyz`), but through `included`+`catch` the
  `catch` does not intercept it.

So "harvest events even on failure" by emit-at-end is impossible in-process and
in a naive child: the process dies (or faults) before the harvest.

### 0.4 D4 `--all-errors --source-list` is a proven no-op

`CHK-MATERIALIZE-LIST` (`tools/check-core.f:532-545`) builds the run source by
appending only `CHK-APPEND-REQUIRED` lines (513-517), i.e. `"path" required`\n per
entry. `CHK-RUN-ALL-CURRENT` (903-906) runs all-errors on `CHK-SOURCE`, which for
source-list mode is that materialized temp. `CA-COLLECT-DEFS`
(`tools/check-all-errors-core.f:475-480`) finds zero `:` definitions in a file of
pure `required` lines, so all-errors checks nothing. Confirmed by code path.

### 0.5 All event-log consumers inherit the blindness

`tools/event-closure-lib.f:102` builds its closure with `DISCOVER:RUN`;
`tools/hb-build-lib.f:724` and `tools/public-signatures-core.f:612` both consume
`EC:BUILD`. A colon-wrapped dep is therefore missing from hb-build cache keys
(under-approx key → risk of stale hit) and from the public-signatures package
pre-scan (a conditionally-opened package is invisible).

### 0.6 Real idiom exposure in the check closure

- `tools/check-all-errors-core.f:8` — guarded colon-wrapped `required` (monotone,
  load-if-absent).
- `test/run-worker.f:34` — `path pathu included` (genuinely dynamic; path is a
  runtime value chosen by a `case` dispatch, `run-worker.f:36-73`).
- `src/habu/driver-io.f:11-14` — loader retirement via
  `s" include" UNDEFINE-IF-DEFINED` (helper form).

### 0.7 Sizing

`tools/check.f` require-closure = 26 files, ≤42 loader tokens (upper bound,
including non-loader string literals). `tools/srclist.f` emits ~32 file paths for
the compiler order. `EVENT-MAX=$100` (256), `EVENT-POOL-CAP=$8000` (32 KiB),
`EVENT-FIELDS=6` (`src/core/include.f:192-194`). Per-file event counts are tiny;
the pool is `EVENTS-RESET` per `DISCOVER:RUN`, so 256 is ample **per file**. The
whole tree is 597 `.f`/`.fs` files, so a *global* accumulation across a full-tree
gate would exceed 256 — relevant only if the design accumulates one event log
across an entire source list rather than per file (see §4).

---

## 1. Candidate (a): two-pass load-and-record

**Mechanism.** Run the target (and each dep) in a sandboxed **child** `bin/hb`
with `EVENT-ON`, let the loader words record every source-composition act as it
fires, harvest the ordered event artifact, then use it to drive the real checked
preverify/redrive in the parent.

**Soundness argument.** *If* the child completes the load, the artifact is exact:
it is literally what the runtime loaded, in order, with real `fresh`/`known`
state (§0.2). Closure = runtime closure by construction; never smaller. This is
the only candidate that yields *exact* (not over-approximated) state, which is
what the plan's "recorded AFTER EVALUATION" language wants.

**Circularity break — and why it fails.** Preverify needs deps *before* checking,
but recording needs the file to *evaluate*, and the file may not compile — that
is what check verifies. The dot's suggested escape is "harvest events even on
failure." Probe §0.3 refutes it: an undefined word (the commonest check failure)
reached through `included`/`evaluate` hard-faults the child (register dump) before
any emit-at-end runs, and is not catchable. Emit-at-end therefore loses the
artifact exactly for the files check exists to reject.

The *only* rescue is **incremental durable emission**: append each event to a
side fd/file inside `EVENT-RECORD` (`src/core/include.f:250-262`), flushed, so a
post-fault parent can read the partial artifact off disk. That is real engine
work on the hot loader path plus a child-process/fd protocol plus partial-artifact
semantics (how much of a crashed file's closure do you trust?). And even
incremental emission cannot record loaders that live *after* the fault point in
source order — so on a broken file the closure is again a subset (the very
regression we are trying to avoid), now nondeterministically truncated at the
crash site.

**Idiom outcomes.** `CA-MAYBE-VERIFY-SOURCE`: recorded (body runs) on a *valid*
load; truncated/lost on a broken load. `run-worker.f` `path pathu included`:
recorded exactly (real path value is on the stack at run time) — (a) is the only
candidate that resolves dynamic paths. `driver-io.f` loader retirement: the child
would actually undefine the loaders and subsequent recording would stop — must be
rejected/guarded.

**Cost.** Highest. Engine change to the loader record path, new child driver +
capture protocol in tools, partial-artifact trust policy, and it still
under-approximates on failure. Circular by nature.

## 2. Candidate (b): DISCOVER executes-and-records colon-wrapped loaders

**Mechanism.** Teach the `DISCOVER` walker to *run* colon definitions that wrap
loaders instead of skipping their bodies.

**The fatal question — "how does it know which to execute without full
evaluation?"** It cannot. To fire the guarded `required` in `CA-MAYBE-VERIFY-SOURCE`
the walker must evaluate the guard `s" VERIFY:SOURCE-BUF" XREF-FIND 0= if …`,
which depends on live dictionary state; to fire `run-worker.f`'s
`path pathu included` it must evaluate the `case` that computes `path`. Any walker
that executes bodies is running arbitrary Forth = candidate (a) with all of its
crash exposure (§0.3) and none of its child-process isolation. Any walker that
*guesses* which body statements to run (e.g. "execute only tokens that look like
loaders, skip the guard") drops the guard and is unsound — it would record a
`required` the runtime skips, over-firing in the wrong direction and possibly
*executing* a real `required`/`included` (side effects, more loads, more crash
surface) during what is supposed to be a read-only discovery pass.

There is no sound middle for (b): it degenerates into (a) or into a guess.

**Idiom outcomes.** Same as (a) if it executes; unsound if it guesses.

**Cost.** Equivalent to (a) plus loss of isolation. Reject.

## 3. Candidate (c): fail-closed manifest when a colon-wrapped/dynamic loader is present

**Mechanism.** When discovery sees a loader it cannot resolve statically (a bare
`included`/`required` with a non-literal path, or a loader guarded inside a colon
body), it **rejects fail-closed** and requires an explicit, checked manifest that
declares the file's dependency closure. `DISCOVER` already rejects the dynamic
case (`E-DISC-DYNAMIC`, `tools/source-discovery.f:159`) and loader shadow/undefine
(`E-DISC-SHADOW`, 178); (c) extends the same discipline to the colon-body case and
adds a manifest side-input as the sound over-approximation.

**Soundness argument.** A declared manifest that is checked to be a **superset**
of the runtime closure is sound for every consumer (never smaller). Over-approx is
safe for keys (§5) and acceptable for preverify under the load-if-absent
discipline (§5). The manifest is data, never executed → no crash surface, no
circularity.

**Who maintains it / what it looks like.** A sidecar list (e.g. a checked
`MANIFEST: file.f … ;MANIFEST` table in `tools/srclist.f` style, or a
`\ deps: …` header the producer parses) enumerating the transitive files. Cost:
**every** file using the load-if-absent idiom (`CA-MAYBE-VERIFY-SOURCE` and its
peers — dozens of support files register support this way) would need a manifest,
kept in sync by hand, with drift risk. That is a large, permanent maintenance tax
for an idiom that is *statically visible in the token stream* (§0.1 shows the
whole-file scan already reads it). Applying (c) everywhere throws away information
the lexer already has.

**Idiom outcomes.** `CA-MAYBE-VERIFY-SOURCE`: rejected unless manifested.
`run-worker.f`: rejected unless manifested (correct — it is genuinely dynamic).
`driver-io.f`: rejected (correct — it retires loaders).

**Cost.** Low engine cost, high recurring human cost if applied to the whole
load-if-absent population.

## 4. Winning hybrid: static whole-file ordered-event producer + fail-closed dynamic manifest

None of (a)/(b)/(c) is right alone. (a)/(b) chase *exact* state by execution and
inherit circularity + crashes; (c) is sound but over-taxes a statically-visible
idiom. The synthesis: **make the event *producer* a whole-file lexical pass (like
`CHK-SCAN-DEPS`, which already sees colon bodies), not a body-skipping executor,
and reserve (c)'s fail-closed manifest for the genuinely dynamic tail.**

**Mechanism.** Replace `DISCOVER`'s `SD-SKIP-BODY` behaviour: lex the entire token
stream (colon bodies included) and emit one ordered event per literal loader token
(`require`/`required`/`include`/`included`/`provided`, plus `S\"` escaped forms),
each carrying kind, exact path string, byte span, and a **statically modeled**
`fresh`/`known` state. The state model mirrors runtime `REQUIRE-KNOWN?`
(`src/core/include.f:96-100`), which is *pure exact-string comparison*: the first
lexical occurrence of a `required`/`provided` path is `fresh`, later exact-string
matches are `known`; `included` is always replay-every. Because the runtime dedup
is itself a pure string test, the static model is exact for it — no execution
needed.

Fail-closed guards stay and grow: non-literal loader path
(`path pathu included`), loader-word shadow (`: required …`), loader-word
*retirement* including the helper form (`s" require" UNDEFINE-IF-DEFINED`,
`driver-io.f:11-14`, which the current `DISCOVER` misses because it is in a colon
body and reserved-name-lint misses because it is not a definition — see §6). Any
of these → reject; the file must supply a checked manifest (option (c)) to
participate in event-closure consumers.

**Soundness argument.** The whole-file lexer captures a guarded loader
*unconditionally*, i.e. it **over-approximates**: it records a `required` even in a
branch the runtime skips. Direction of error is therefore always "closure ≥
runtime closure," never smaller — the invariant in the header holds by
construction. It never executes source → no circularity, no crash (§0.3 cannot
happen). It is the identical information source the master-green `CHK-SCAN-DEPS`
already uses (§0.1), so it cannot regress the flows that pass today
(`GE-ENGINE-STDLIB-CHECK`, `CKT-TEST-REQUIRE-FACADE`). The genuinely dynamic tail
is the only thing it cannot see, and that is exactly where (c) applies.

**Over-approximation: acceptable for keys, acceptable for preverify here.**
- **Keys (hb-build, `EC:BUILD`).** A superset key only over-invalidates (spurious
  cache miss); a *subset* key produces a stale hit = unsound. So keys *require* a
  superset; `DISCOVER`'s current subset is the actual bug, and the whole-file
  superset is the fix. Over-approx is unconditionally safe here.
- **Preverify.** Both directions are lossy in principle: a subset makes the
  checker reject good code (fail-closed, no false accept); a superset could
  register support the runtime will not load and thus *accept* code the runtime
  then fails on (false accept, unsound) **only for a non-monotone guard** ("load X
  only if Y is *present*"). The codebase's idiom is uniformly *monotone*
  load-if-absent (`XREF-FIND 0= if … required`): in the checked context the guard
  condition (word absent) is the same one that makes the runtime load it, so the
  over-approximation *coincides* with the runtime closure for these guards. That
  is precisely why the whole-file scan is green on master today. The design keeps
  that superset and additionally rejects any *dynamic/non-monotone loader
  dataflow* fail-closed, so the residual unsound case (a non-monotone guard) is
  structurally excluded rather than assumed absent.

**Idiom outcomes.** `CA-MAYBE-VERIFY-SOURCE`: captured statically (superset),
matches runtime under monotone load-if-absent — no manifest needed.
`run-worker.f` `path pathu included`: reject fail-closed → manifest.
`driver-io.f` retirement: reject fail-closed (loader retirement) → manifest or
documented boundary + dot.

**Cost.** Engine: none on the runtime loader path (the record path in
`include.f:250-262` is untouched; only `DISCOVER`'s producer changes). Tools:
rewrite `SD-WALK`/`SD-STEP`/`SD-COLON` in `tools/source-discovery.f` to scan bodies
and emit the static state model; one shared producer then fixes `EC:BUILD`,
hb-build keys, and public-signatures at once (single closure source, as the
cross-consumer note demands). `EVENT-MAX=256`/32 KiB pool is adequate per file
(§0.7); a global cross-file accumulation for D4 should scan per original file
(reset between files) to stay inside the cap, or raise `EVENT-MAX` if a single
file ever exceeds it (none does today). Migration for `EC:BUILD` consumers is
transparent — they keep calling `EC:BUILD`/`EC:COUNT`; only the underlying walker
changes.

---

## 5. Verdict

**Winner: §4 — static whole-file ordered-event producer, plus fail-closed
manifest for the genuinely dynamic tail.**

Why the others lose:
- **(a)** is circular (must run the file to know what to check), crashes on the
  files check exists to reject (§0.3, register dump, uncatchable), and even with
  incremental durable emission still truncates the closure at the crash site =
  the same subset regression. Highest cost, lowest safety.
- **(b)** has no sound way to run only the loader-bearing body statements without
  the guard's runtime state; it collapses into (a) (with worse isolation) or
  guesses (unsound, and it *executes* real loads during a "read-only" pass).
- **(c)** alone is sound but imposes a hand-maintained manifest on every
  load-if-absent support file — dozens of files, permanent drift risk — for an
  idiom the lexer already reads for free. Correct only for the dynamic tail.

The hybrid keeps the proven-green static superset, upgrades the *producer* from a
body-skipper to a whole-file lexer so the dominant colon-wrapped idiom is captured
soundly for **all** consumers at once, and applies (c)'s fail-closed manifest only
where dataflow is truly dynamic.

### Slice plan (each independently green)

1. **Non-regressing scan widening.** Extend `CHK-SCAN-DEPS`
   (`tools/check-core.f:468-473`) to also capture `include`/`included`/`provided`
   literal forms (today `require`/`required` only). Pure superset growth; lands
   now, green against the existing check test-suite. (This is the dot's stated
   partial improvement and de-risks slice 3.)
2. **Whole-file event producer.** Rewrite `tools/source-discovery.f` so
   `SD-WALK` scans colon bodies (drop `SD-SKIP-BODY`'s skip; keep `;`/comment
   handling for token boundaries) and emits ordered events with the static
   `fresh`/`known` model and byte spans. Keep/extend fail-closed guards: dynamic
   path (`E-DISC-DYNAMIC`), loader shadow (`E-DISC-SHADOW`), **new** loader
   retirement via `UNDEFINE-IF-DEFINED`/`UNDEFINE` string form. Gate:
   `tools/source-discovery-test.f` unchanged plus new colon-wrapped-required and
   helper-retirement fixtures. This single change fixes `EC:BUILD`, hb-build keys
   (`tools/hb-build-lib.f:724`), and public-signatures pre-scan
   (`tools/public-signatures-core.f:612`) together.
3. **Migrate preverify onto the shared closure.** Point `tools/check-core.f`
   preverify/closure at the event producer (retire `CHK-SCAN-DEPS` once slice 2
   proves parity), asserting identical dep sets on `GE-ENGINE-STDLIB-CHECK`
   (`test/gate-engine-lib.f:106`) and `CKT-TEST-REQUIRE-FACADE`
   (`tools/check-test-lib.f:423`).
4. **D4 per-original-file redrive.** Replace the `required`-lines materialization
   (`CHK-MATERIALIZE-LIST`, 532-545) for the all-errors path: iterate
   `CHK-DEP-ORDER` and run all-errors on each **original** file, replaying prior
   source-list entries as support first (new cross-file support entry in
   `tools/check-all-errors-core.f`, alongside `CA-SUPPORT-BEFORE`). Reset the event
   log per original file to stay inside `EVENT-MAX`.
5. **Dynamic manifest.** Fail-closed rejection + checked manifest sidecar for the
   dynamic tail (`test/run-worker.f:34`, `src/habu/driver-io.f:11-14`); track any
   file kept out of event-closure consumers as a documented boundary + dot.

### Acceptance fixtures

- **Colon-wrapped parity:** a file shaped like `CA-MAYBE-VERIFY-SOURCE` yields the
  *same* dep from the event producer as from the whole-file scan
  (event-count and path equal; the §0.1 gap closes).
- **Consumer propagation:** `EC:BUILD` closure over a file whose *only* dep is a
  colon-wrapped `required` includes that dep; an hb-build key changes when that
  colon-body dep's content changes (proves no stale-hit); public-signatures
  pre-scan sees a package opened only inside a colon-wrapped `required`.
- **Fail-closed dynamic:** `path pathu included` and
  `s" require" UNDEFINE-IF-DEFINED` each reject with a discovery error unless a
  manifest is supplied; with a valid superset manifest, closure = manifest.
- **D4 flip:** `--all-errors --source-list a.f b.f` where `b.f` has a broken
  `:` definition reports that definition's diagnostic against **`b.f`** (not the
  materialized temp), and prior-file `a.f` support (types/packages) is in scope —
  turning the proven no-op (§0.4) into a real per-original-file redrive.
- **Monotone-guard soundness:** a load-if-absent guarded support file that
  runtime loads in the checked context is preverified; a synthetic *non-monotone*
  guard (`… if present … required`) reaching a dynamic path rejects fail-closed
  rather than producing a false accept.

---

## 6. Dot-claim vs code cross-check

The dot's RCA is accurate; no contradictions found. Refinements/additions:

- **Confirmed exact:** `CHK-SCAN-DEPS` at `tools/check-core.f:468-473` catches the
  colon-wrapped idiom (§0.1); `CHK-MATERIALIZE-LIST` 532-545 + `CHK-RUN-ALL-CURRENT`
  903-906 make source-list all-errors a no-op (§0.4); `run-worker.f:34` dynamic
  `included`; `driver-io.f:11-14` loader retirement; `EC:BUILD` via `DISCOVER:RUN`
  (`event-closure-lib.f:102`) feeding hb-build (`:724`) and public-signatures
  (`:612`).
- **Refinement of "sees FEWER deps = soundness regression":** precise direction
  differs per consumer. For hb-build/`EC:BUILD` **keys** a subset closure is a
  genuine *soundness* bug (stale-hit). For **preverify** a subset is a
  *completeness* regression (fail-closed reject of good code, breaking
  `GE-ENGINE-STDLIB-CHECK`/`CKT-TEST-REQUIRE-FACADE`); it does not produce a false
  accept. The unsound-accept risk is the *opposite* error (over-approx of a
  non-monotone guard), which the design excludes fail-closed (§4/§5).
- **New gap not in the dot:** the loader-retirement idiom
  `s" require" UNDEFINE-IF-DEFINED` (`driver-io.f:11-14`) is invisible to **both**
  the current `DISCOVER` (it is inside a colon body) **and** the reserved-name-lint
  loader guard (`tools/reserved-name-lint-core.f:163-168`), which only flags a
  *definition* named after a loader (`: required …`), not a helper-form retirement.
  The whole-file producer must add a fail-closed guard for this form; the two
  guards are complementary, not redundant.
