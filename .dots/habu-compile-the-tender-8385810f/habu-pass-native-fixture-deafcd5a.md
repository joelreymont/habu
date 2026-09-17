---
title: Pass native fixture paths as complete arguments
status: closed
priority: 2
issue-type: task
created-at: "\\\"2026-09-13T14:51:13.081324+03:00\\\""
closed-at: "2026-09-16T14:34:49.466534+03:00"
close-reason: "superseded by habu-campaign-c1-finish-1f129a00: Native fixture paths are still passed as unquoted shell text; keep the complete-argument fix with the recovery fixtures."
---

Plan: [PLAN.md](../../PLAN.md). Design reconciled 2026-09-13; replaces stale diagnosis/claim. Claim: unassigned.

Own test/nf.fs HB_TMP paths/process invocation. Quote/escape complete shell arguments including redirections and size encoded combined command, or reuse structured argv if available. Space path fails today;97-byte root fits individual128 but exceeds command256. Preserve per-run isolation. Verify spaces/metacharacters/quotes, long valid root, clear overflow refusal and concurrent real gforth/native fixtures. No new runner framework.

Verification: focused real-load cases above; rebuild and run `bin/hb --load test/run.f` for compiler/runtime integration. Speed acceptance uses the all-AOT campaign pair; functional/count evidence can be developed in parallel.

Claim: Sol native-fixture-argv lane; Cedar independent review/integration. Scope: named fixture argv/path handling and buffer sizing, preserving real build behavior.

Stage0 prerequisite repair (Cedar, 2026-09-14, base `c0335a0d`): the real
Gforth build failed before testing paths because the mirror still had 128
primitive slots. Host registration now grows five-cell rows and a name buffer
through the existing `BUF-FIT`; offsets preserve names across relocation.
Both requested byte counts and geometric rounding are checked against the
host's signed size limit. Logical reset reuses storage. The emitted dictionary
still has 48-byte records and a 16-byte inline-name limit; separate zero padding
also handles a fresh one-byte pool without reading beyond it.

After that repair, the actual emitted program refused `atomic-cas` in the cold
prefix's `DYNAMIC-STORAGE:LOCK`. The mirror now supplies the two required native
operations, `atomic-cas` (the existing fixed CASAL instruction) and `atomic!`
(STLR), using its own sealed-span guard. No other recovery-chain work is part
of this leaf. The real primitive set is 131 rows / 680 name bytes.

Focused acceptance, linux-aarch64 / Gforth `0.7.9_20260610`:

- `test/bootstrap-primitive-registry.fs`: 257 rows / 4112 name bytes, all labels,
  lengths, names and WIDs retained across growth; invalid lengths and size
  overflow refuse without logical publication; reset/reuse and 48-byte emitted
  records pass. It runs inside the real BWM fixture, including the named
  overlong-name refusal.
- `env HABU_TARGET=linux-aarch64 HB_TMP=<private> gforth test/bootstrap-wide-memory.fs`:
  rc0, exact `ok\n`. Existing wide-memory, namespace, defer and catch assertions
  remain intact. Added successful and unsuccessful CAS, release store, and
  separate emitted-process seal refusals for store and both CAS outcomes.
- Actual I (`hb-integrated-I`, SHA256
  `13969ea2601f7e76eae2729e9d86d1fe10b297f361a7e178fc467db57cdaedae`)
  `--load test/nf-path-test.f`: rc0 / `test: ok`, including concurrent real
  Gforth builds at the 97-byte root and the spaces/quotes/metacharacters root,
  long combined REPL command, path overflow diagnostic and forged-output
  nonzero-exit control. Logs: `/tmp/cedar-stage0-final-nf-path.log` and
  `/tmp/cedar-stage0-accepted-qthf1hj1/gforth.log`.
- I `--load test/bootstrap-wide-memory-src.f`: rc0 / `ok`; log
  `/tmp/cedar-stage0-final-wide-memory-I.log`.

Awaiting independent root review and combined native gate. This is actual
stage0 fixture execution, not acceptance of the whole bootstrap recovery chain.
