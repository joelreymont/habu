---
title: Unfreeze checker property fuzzer
status: open
priority: 2
issue-type: task
created-at: "2026-07-01T22:33:20.202757+02:00"
---

test/prop-test-core.f:23-26: DEFAULT-SEED 1, 250 iterations, integer-only op alphabet - every run explores the identical space, so the product's core soundness claim gets a frozen 250-case regression, not fuzzing. Also reads engine state via hard-coded image offset $37D8 PROP-EVALERR-CELL inside TRUSTED: (prop-test-core.f:18-20; 18 TRUSTED: sites in the file) - layout-fragile. Fix: per-run random seed printed for repro + --seed flag, seed-sweep sharded across pool slots, extend generator alphabet (strings, locals, quotations with row vars, return-stack ops, leave/exit), replace the magic-offset peek with a checked engine primitive.

## Progress (2026-07-03)
DONE: (1) default seed is now per-run random (FRESH-SEED via mono-ns,
clamped to 31 bits, 0 mapped to 1) so every gate run explores fresh space;
RUN-SEED still printed by POS. and `bin/hb <seed> <count>` reproduces any
run (verified: seed 1 replays byte-stable). (2) the hardcoded $37D8 peek
is gone - ERR@ reads the NAMED layout constant EVALERR-CELL from
src/habu/layout.f, so a layout change cannot silently point the peek at
the wrong cell. Gate green with random seeds (250 iterations fits the
phase budget; raising to 1000 blew it - iteration scale-up belongs to the
sharding item below, not the per-run count).
REMAINING: seed-sweep sharded across pool slots (N slots x 250 iters,
distinct seeds, one red fails the phase); extend the generator alphabet
(strings, locals, quotations with row vars, return-stack ops, leave/exit);
replace the TRUSTED ERR@ peek with a checked engine primitive exposing
the eval-error flag.

## Progress (2026-07-03, cont.)

DONE:
- Seed-sweep sharded: the default run now forks PROP-SHARD-N (8) slots, each a
  distinct seed (golden-ratio 0x9E3779B1 spread off a per-run FRESH-SEED base)
  running DEFAULT-COUNT (250) iterations, so one gate phase covers 8 x 250 =
  2000 distinct-seed programs in parallel (~1s wall). Self-tests + baits run
  once in the parent; each shard runs silently and dies 1 on a false-cert; any
  nonzero shard exit fails the phase (SWEEP/SHARD-FORK/SHARD-JOIN, SWEEP-RED).
  Fault propagation verified (a shard `1 die` -> parent dies). The
  `bin/hb <seed> <count>` argv path still runs one seed serially for repro.
- Gate-capture gotcha handled: each shard checks hundreds of intentionally
  rejected fuzz programs, whose per-reject checker diagnostics on stderr x8
  overflow the gate's 32KB GT-ERR-CAP and trip E-PROC-TRUNCATED. Each shard now
  redirects fd 2 to /dev/null (SHARD-MUTE-STDERR); false-certs report on stdout
  + the shard exit, so the signal is preserved. Full gate green; prop/debug
  phase 1036ms; stdout clean, stderr 235B.
- Extended generator alphabet: added net-0 ops covering valid leave/exit
  (`3 0 ?do leave loop`), strings (`s" x" 2drop`), row-var quotations
  (`[: ;] execute`), return-stack (`>r r@ drop r>`), and mid-body locals
  (`{: zN :} zN`, fresh per-step name). Validated: ops appear in generated
  bodies and certify; 0 FALSE-CERT and 0 false-reject across seeds/counts (a
  wrong-net op can only cause rejections, never a false-cert). Note: the pre-
  existing metamorphic round-trip inconsistencies (REND-SIG under-renders input
  arity for `{: a b :}` local-binding bodies) are unchanged by this - present in
  both original and extended alphabets; not this dot's scope.

STILL OPEN (other worker's territory, do not touch here): replace the TRUSTED
ERR@ magic-offset/EVALERR-CELL peek with a checked engine primitive exposing the
eval-error flag - needs an engine (src/habu/*) change.
