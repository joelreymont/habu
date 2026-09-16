---
title: Measure and finish pass-scoped IR reader reuse
status: closed
priority: 2
issue-type: task
created-at: "\"2026-09-11T16:38:06.256061+03:00\""
closed-at: "2026-09-16T14:34:49.390899+03:00"
close-reason: "superseded by habu-campaign-c1-finish-1f129a00: Pass-scoped IR reader reuse is a measured compiler optimisation on the current plan; keep the measurement, not the standing lane."
blocks:
  - habu-build-the-compiler-c348eab0
  - habu-attr-and-remove-2b13e978
  - habu-make-spill-rewrite-ca192310
---

Plan: [PLAN.md](../../PLAN.md). Claim: Cedar reader lane; root owns independent
review and integrated full gate. Existing per-field checked readers remain intact.

Measured on actual append-B (`1d7b2b71`, SHA
`7e715bcb19486a0fd4c21631fe9d6b95ea82b1fe38e9bcbffde3de588edb79d7`): one
warmed trivial definition performs 2,895 frozen OPENs, 2,965 OPEN-LIVEs and
24,924 RD@ reads. CHECK-SCAN/CHECK!/EFFECT-QUERY/NDICT:CALL-TARGET counts are
1/1/3/1, so duplicate checker scans or token lookup are not this cost. Startup
and benchmark teardown are outside the counted interval; GDB counts are not
timings. Logs: `/tmp/cedar-append-query-count.log` and
`/tmp/cedar-append-open-callers.log`.

The repeated frozen opens belong to IR-OP/FUN readers behind NFROZEN:
FOPS 336, FATTRS 254, FOPCODE@ 215, FATTR-KEY@ 438 (two opens per call),
FOP@ 310 and FBLOCK@ 206. Live IR-SYM:LEN@ contributes 761 separate live
opens and is outside this leaf. A disposable native copy of the existing driver
with ten stage clocks measured 1,000 definitions: trivial/three-op selection
326/263 us, combine 252/20 us, emission 145/126 us and elaboration 78/77 us.
Its total 1,002/687 us agreed with the stock sampler's 1,006/691 us within the
concurrent-load variation. These are attribution measurements, not quiet speed
acceptance. Existing probes and logs are `/tmp/cedar-append-pass-{0,1}.log` and
`/tmp/cedar-append-profile-{0,1}.log`; no production profiling framework added.

Implemented: NFROZEN:VIEWS! opens its fourteen frozen views once per binding.
Its row accessors use typed reader-taking IR-OP/FUN/TYPE/ATTR/VERIFY APIs;
the public view wrappers and every existing pass/module rebind remain. Every
cell read still validates generation, state and bounds. Frozen API admission
rejects live readers; the eight-field operation tiling memo keys both complete
reader tokens and the operation owner. It retains no mapping pointer, size or
new ABI field. Public wrapper admission order is preserved, including a block
before its companion view and a predecessor pool before its row view.

Actual product `/tmp/cedar-family-stage-abi/hb-native-readers`, source
`38ed1d24`, SHA `5879a0ccae9c60e5e07285d0d0682b096e854d698affe8bf991dab1a1689bf14`,
built from append-B in 131.62 s, rc 0. Build log:
`/tmp/cedar-family-stage-abi/native-build-readers.log`. A test-only frozen-module
getter correction follows that source; runtime source is unchanged.

Tier-1 product suites passed: ir-arena, ir-op, ir-build, ir-fun, native-select,
native-combine, native-regalloc and native-loop. New controls cover live-token
refusal, generation/state error ordering, retired and reused slots, context
teardown, exact memo identity for alternate tables of one owner, bounds and
failed-miss recovery through both view and retained-reader APIs, and NFROZEN
switching between differently sized frozen modules. Logs:
`/tmp/cedar-readers-{ir-arena,ir-op,ir-build,ir-fun,native-select,native-combine,native-regalloc,native-loop}.log`.

The same warmed actual definition now makes 328 frozen OPENs (88.7% fewer).
OPEN-LIVE 2,965, RD@ 24,924, FIND-B 2,654 and checker/query counts are unchanged.
`/tmp/cedar-readers-count.log` uses the same interval and original sampler/count
tools. Integrated source is in product H (`35e16634`, SHA
`bbc477819d7d7bef0de5940611939ea4b622a08e4e1d7ec5c5af7f9aae915f7d`).

Three interleaved append-B/H pairs measured trivial AOT 994/953, 994/953,
996/961 us; three-operation AOT 705/645, 707/640, 707/641 us; JIT 29–30 us
unchanged. Every sample counts exactly 200 NCOMP calls. Own build lanes were
drained; an external Maki lint used about 34% CPU. This is composed-product
measurement, not isolated reader attribution or quiet acceptance. Logs and
results: `/tmp/cedar-H-reader-pairs/`. The integrated full suite remains root's
gate. This leaf does not claim the 500 us target or optimize live symbol reads.
