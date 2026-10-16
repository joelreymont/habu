---
title: Throw test wait deadlines by name
status: open
priority: 2
issue-type: task
created-at: "2026-10-16T16:28:19.553568+03:00"
---

Problem: docs/gate.md (the bullet "A test's own capture deadline is no assertion result") makes a test's own deadline no assertion result: a bounded wait whose clock ends must throw E-PROC-TIMEOUT after printing one stdout line naming the wait, so the row exits 67 with `hb: uncaught throw code -2502` last on stderr and the pool labels it TIMEOUT-UNDER-LOAD, unless the case expects that deadline and asserts it. A task asserts nothing: a worker records its deadline and main throws it at the join. Deadlines are never raised. Joel ruled every such defect dotted and fixed. Earlier members of this family landed as b7d3d95c, d3c72266, fe581822 and 8dcd9878.

## Scope and contract

This family covers the census’s 74 asserted-deadline uses, its additional wait shapes, and the propagation defects found while tracing their consumers. Census: the 19 wait words with 74 asserted-deadline uses (curl-test 17, tcp4-test 12, aio-test 21, task-test 3, process-task-test 1, ws-test 19, keyed-image-reap-test 1), the same defect in other shapes in about a dozen more files, and the wrong-code throws. Line numbers are on master ae381482 (6b24c466 does not touch these files); word names are the stable anchors.

An unexpected wait deadline must produce a named stdout diagnostic and escape the row as `E-PROC-TIMEOUT`. The row must exit **67**, with this exact last stderr line:

```text
hb: uncaught throw code -2502
```

The existing pool then reports `TIMEOUT-UNDER-LOAD`. Children convey the same outcome through `PROC-TIMEOUT-RC`, **124**, and their parents throw again. These boundaries already exist in `docs/gate.md:415–453`, `src/lib/process.f:226–255`, and `test/gate-pool.f:1474–1492`.

The requested **stdout** rule supersedes the census’s suggestions to write diagnostics to fd 2. Existing capture-reporting helpers remain appropriate for captures; they print the program and captured streams before throwing (`src/lib/test/subject.f:100–119`).

Excluded from implementation:

- `src/lib/net/http.f`, `http-request.f`, `http-router.f`, and `curl.f`.
- `src/lib/net/udp4-test.f`, `src/lib/signal-test.f`, `src/lib/serial-test.f`.
- `test/baked-owner.f`, `test/diag-position-test.f`, `test/repl-open-definition.f`, `test/replay-binding.f`, `test/process-image.f`, and `test/process-image-subject.f`.

No deadline, retry count, sleep interval, or grace period is increased.

## Shared interface

Add `src/lib/test/wait.f`, in package `TEST-WAIT`, with two public words:

```forth
TEST-WAIT:EXPIRED ( ptr u8 n -- )
TEST-WAIT:UNTIL   ( R [ R -- R bool ] n ptr u8 n -- R )
```

### `EXPIRED`

The string names the wait. It prints exactly:

```text
<name>: a deadline expired
```

using stdout `type` and `cr`, then throws `E-PROC-TIMEOUT`. It does not read or change assertion counters or the current test label.

For example:

```forth
s" aio-test REACH" TEST-WAIT:EXPIRED
```

prints:

```text
aio-test REACH: a deadline expired
```

This follows the existing small diagnostic-and-throw shape in `src/lib/net/udp4-test.f:158–170`.

### `UNTIL`

Arguments, after the preserved stack row `R`, are:

1. A predicate preserving `R` and adding a boolean.
2. An absolute `mono-ns` deadline.
3. The wait’s name.

It checks the predicate first. Success returns `R`. Otherwise, expiry calls `EXPIRED`; an unfinished iteration calls `TASK:PAUSE`.

It does not catch predicate errors, retry particular exceptions, reset deadlines, join tasks, or perform cleanup. Callers retain their explicit predicates and compute their existing deadlines. Use `>=` for expiry; this does not extend any existing window.

The preserved row permits explicit cell/target or task-pointer arguments without closures or scratch globals. Habu quotations cannot capture locals; existing row-polymorphic callbacks provide the relevant language precedent (`docs/forth-card.md`, section 2; `src/core/combinators.f:15–21`; `src/lib/vector.f:216–219`). The signature has not been compiled yet: the first child validates it through its real consumers, and if the checker refuses this effect it reports the exact refusal instead of inventing another shape.

Require this module explicitly from its users. Do not add a task dependency to every user of `src/lib/test.f`, whose current imports are at `src/lib/test.f:3–7`.

### Why share the polling loop

Nine straightforward `TASK:PAUSE` loops repeat across four files:

| File | Repeated loops |
|---|---|
| `src/lib/aio-test.f:172–197` | count reached, task ended, loop stopped |
| `src/lib/net/curl-test.f:1355–1381` | count reached, task ended, loop stopped |
| `src/lib/net/tcp4-test.f:282–288,394–400` | count reached, writer ended |
| `src/lib/task-test.f:1183–1189` | cell equals target |

`ws-test.f:1593–1599` supplies another simple polling caller.

Sharing only the diagnostic would leave those loops duplicated. `UNTIL` removes their repeated clock and pause control while keeping each condition visible in a small local wrapper. Pure waits lose their boolean result and `?` suffix; their callers lose deadline-only `TTRUE`s.

Keep local loops for sleeps, blocking polls, progress-reset clocks, mixed failure results, and cleanup-sensitive waits. Examples include `ws-test.f:276–284,1642–1681`, `tcp4-test.f:485–493`, and `capture-tree-test.f:103–122`. Do not add configurable scheduling or error policies to accommodate them.

The two-word module is sufficient; no new waiter objects, outcome hierarchy, registry, or general task-join framework is needed.

### Already-fixed joins

`serial-test.f:111–119` and `signal-test.f:329–337` should eventually use this shared polling facility: their condition, pause, and timeout behavior are the same.

That migration is excluded here. It belongs in its own child, “Share the fixed task join waits”. Preserve their effective cutoff: `PROC-LEFT-MS` floors milliseconds, so replacing it with a nanosecond comparison must not add the final fractional millisecond (`src/lib/process.f:302–314`).

## Classification rulings

### Completed-duration assertions remain assertions

Keep all section 4 elapsed-time upper bounds:

- `tcp4-test.f:857`.
- `task-test.f:1091`.
- `ws-test.f:1736,1821,1852,2147`.
- `pty-harness-test.f:327`.

The same ruling applies to the additional direct comparisons read at `tcp4-test.f:907` and `ws-test.f:1988–1992`.

These measure behavior that has completed, or timestamps already recorded. They do not turn an unfinished observation into a false answer when a polling clock expires. This follows the user’s earlier latency ruling; the referenced HTTP comparison is now at `http-test.f:1665–1667`.

Scheduler starvation can affect these measurements. That does not make them wait-deadline results under the established ruling. Preserve their bounds and assertions.

### Subject-owned bounds still distinguish expiry from evidence

A library supplying the duration does not make a missed observation affirmative evidence.

- In `ws-test.f:1568–1574`, `HEARD-CLOSE?` must throw when the observation window expires. Keep the subsequent close code and recorded-duration assertions at `1735–1740`.
- In `PONG-WAITER?`, both clock-expiry paths must throw: the initial check at `1670` and the post-wake check at `1675`. A changed claim remains a genuine false result; an unexpected futex result remains its existing error (`1671–1679`).
- Positive TCP readiness expectations must translate `idle` into a named timeout. Preserve deliberate idle assertions at `tcp4-test.f:707,730`, and immediate probes at `663,668`. Positive timed consumers are at `456,691,719,735`; the worker consumer is at `303`, with its result read at `560–562`.

Do not globally change `TCP4:ready-result` or the production library.

### Wrong-code throws

Change only the clock-expiry meaning:

| Location | Ruling |
|---|---|
| `ws-test.f:664–672`, `AWAIT` | Its `idle` arm becomes a named `E-PROC-TIMEOUT`. Preserve other socket errors. |
| `ws-test.f:2502–2509`, `AIO-STOP` | Retry only `E-AIO-BUSY`; rethrow other errors immediately. Expired draining becomes a named timeout. The control-flow reference is `http-test.f:2188–2199`; use stdout for the new diagnostic. |
| `fs-rename-noreplace-test.f:91–112` | Record `E-PROC-TIMEOUT` in the task; name and throw it when main reads the joined result. |
| `fs-mutate-test.f:1007–1021` | Replace worker `E-FS-IO` on expiry with `E-PROC-TIMEOUT`; intercept that code before `JOIN-OK` asserts it. |
| `pg-test.f:152–184,848` | Replace expiry’s `E-EXEC`; retain `E-EXEC` for actual invalid PG results. Carry the timeout through the case-process boundary and harness cleanup. |

### Worker tasks record; main reports

Use task results or an existing local handoff. Workers do not assert deadlines or print competing timeout diagnostics.

For ordinary tasks, throw `E-PROC-TIMEOUT` inside the task and let `TASK:JOIN` record it. Main calls `EXPIRED` before any assertion on that result. The existing distinction is visible in `process-task-test.f:87–97,187–191`.

Two fixtures need explicit treatment:

- **Curl server:** `STREAM-WAIT-ACK` and `RETRY-NEXT` currently turn expiry into `SERVER-BAD`; the server publishes completion but has no join-result rethrow (`curl-test.f:457–478,562–601,694–737`). Record a small atomic deadline reason, close the active connection, and stop normally. Main observes that reason, joins/stops the server, and reports it before checking client results or server counters.
- **WebSocket handler:** HTTP catches handler throws, and stopping HTTP does not expose them as task results (`http.f:285–295,944–953`). Record the handler’s deadline in the existing `REST-SEEN` handoff and consume it in `REST-CASE` before assertions (`ws-test.f:548–569,1366–1408`). Do not rely on a second main-task deadline eventually expiring.

### Child deadlines require every process boundary

**Keyed image reap**

The chain is:

```text
SAVE child
  → KEYED-IMAGE:BUILD-RUN in BUILD child
  → keyed-image-reap-test parent
  → gate pool
```

`SAVE` currently exits 75 (`test/keyed-image-reap-child.f:67–77`). The keyed-image builder forwards 75, but already turns 124 into `E-PROC-TIMEOUT` (`tools/keyed-image.f:111–131`). Therefore changing only `SAVE` would make the intermediate BUILD child exit 67, losing the status distinction again.

Required behavior:

1. SAVE’s deadline exits 124.
2. BUILD catches that timeout and exits 124.
3. Parent `EXITED` recognizes 124 before any exit-code assertion (`keyed-image-reap-test.f:276–280,347,370,381`).

**Inferred consequence:** the live builder can expire before `FINISH-LIVE`, making earlier live-directory checks fail first. Record those observations at their existing points and assert them only after the live build’s completion has been classified (`keyed-image-reap-test.f:343–353,368–385`). Do not merely fix the final `EXITED 0 T=`.

**Gate image orchestration**

The controlled builder exits 71 today (`gate-images-orch-builder.f:18–24`). It is copied over `compiler-images-build.f` by the outer test (`gate-images-test.f:52–65`). `COMPILER-IMAGES:BUILD` already throws on child exit 124 (`test/compiler-images.f:35–44`), giving the inner pool the correct timeout.

There is another boundary: the nested gate normally exits 1 for any red set (`test/gate-stdlib-lib.f:225–233`), and the outer test asserts its exit code (`gate-images-test.f:80–107`). Preserve timeout through this fixture boundary too; otherwise the outer gate still receives an assertion failure.

Use the nested fixture’s existing teardown hook and structured compiler-red record, not transcript substring matching or changes to general gate exit policy (`src/lib/test/suite.f:358–373`; `test/gate-images.f:506–510`; `test/gate-pool.f:1070–1074`).

### PostgreSQL is mandatory

The default `pg` row runs:

```sh
bin/hb --load test/db/pg-cluster.f test/db/pg-kill-test.f </dev/null
```

It is registered at `test/gate-stdlib-cases.f:1628–1638`. The harness defaults to `src/lib/pg-test.f` and `src/lib/db/rows-test.f` (`test/db/pg-cluster.f:501–510`). `initdb` and `postgres` are gate requirements, not optional dependencies (`docs/gate.md:472–481`).

Changing `CHECK-PROGRESS` alone is insufficient: the case engine would exit 67, while `EXITED-0?` currently treats all nonzero exits as ordinary failures (`pg-cluster.f:220–232`). The case must exit 124 on its timeout; the harness must mark that outcome late and throw after stopping the server (`pg-cluster.f:434–444,525–546`).

### Count bounds

Keep these as assertions:

- `tcp4-test.f:830–843,854`: a bounded amount of sending seeks a full buffer.
- `curl-test.f:2021–2035`: eight attempts seek a race outcome.

They bound performed work or attempts at a behavioral outcome, rather than elapsed observation time.

**Policy inference:** the readiness-only retries in `tcp4-test.f:436–447` and reset-observation retries at `946–955` are bounded waits despite expressing their budget as turns. Their exhaustion should report timeout. Keep the existing 64 attempts and sleeps; do not invent or enlarge a wall-clock limit. The same reporting treatment applies to curl’s already-throwing `WAIT-READY` and `WAIT-DONE` (`curl-test.f:703–716`).

## Verification convention

The following notation is used in every dot:

**`P(name)`**, used in every child, means the forced-deadline copy must:

1. Exit **67**.
2. Print this stdout line for the expired wait:

   ```text
   name: a deadline expired
   ```

3. End stderr with exactly:

   ```text
   hb: uncaught throw code -2502
   ```

The pool's labelling of that exit as `TIMEOUT-UNDER-LOAD` is already covered by `test/gate-pool-test.f:981–1001`; a forced copy need not run through the pool.

A child boundary may exit **124** internally; its enclosing row must satisfy `P`.

Run normal focused rows with:

```sh
bin/hb --load <the row's files> </dev/null
```

There is no `--only` selector on `test/run.f` (`test/gate-stdlib-lib.f:29–34`). Forced variants use disposable copies under external scratch. Preserve the matching source-root relationships; overlaying one file can otherwise double-load providers (`docs/forth-card.md`, section 7).

For a pure loop, the deterministic forced copy replaces its success predicate with false and gives only that copied invocation an already-expired deadline. This exercises the real deadline branch and caller without relying on host load. Worker and process proofs below additionally exercise their actual handoff.

Keep the copied source and stdout/stderr/exit result as the repeatable proof artifact. Do not commit artificial delay switches or a new testing framework.

## Landing

Each child is one commit that also runs its `dot off`. Two worker lanes at most; the same-file curl and WebSocket chains stay sequential. Each child gets an independent read-only review (the requirement and the code) before landing, a focused follow-up review of its fixes, then its focused rows at the rebased tip. No compiler, runtime ABI, codegen or capture change is planned, and each row loads the shared helper it uses, so the focused rows bound the change; the full native suite is not required. Linux-only branches (ws-test PONG-WAITER futex arm, proc-watch-smoke zombie arm) are reported as untested on macOS, not counted as passed. This parent closes after every child.

Files: the children's. Verify: the children's. Depends: none. Ownership: jerry (lead). Claim: unassigned.
