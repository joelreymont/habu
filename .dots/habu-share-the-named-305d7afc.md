---
title: Share the named-row store between the two codegen harnesses
status: active
priority: 2
issue-type: task
created-at: "2026-08-04T09:55:43.245282+02:00"
---

tools/codegen-compare-core.f and tools/codegen-workload-time.f still hold two copies of one named-row store, which is what is left after the timing discipline was factored into tools/codegen-time.f (dot habu-share-the-timing-2eda3703). Both define, identically or near-identically: `SLOT ( ptr a n -- ptr a )`, `ROW-OK`, a `ROW-MAX NAME-MAX * BUFFER: NAME-BYTES` plus `NAME-LENS` string table with `NAME-AT`/`NAME!`/`NAME$`, `variable ROW-N` with `ROWS` and `RESET`, a name-length cap check, and the same `0 begin dup ROW-N @ < while dup NAME$ a u STR= if exit then 1+ repeat drop -1` search (spelled ROW-OF in one and FIND-ROW in the other, the second filtered by path).

Roughly 60 lines are stated twice. What genuinely differs is only parameters and error codes: ROW-MAX 32 vs 48, NAME-MAX 64 vs 32, E-CODEGEN-COMPARE-CAP/ROW vs E-WLTIME-CAP/ROW. A shared store would take the capacities as its own constants (or as one pair large enough for both, decided by measurement, not by taste) and the cap/row code the way tools/codegen-time.f:SPREAD-OF already takes the dead-clock code - from the caller - so each harness keeps reporting its failures under its own names.

Why it is worth doing and why it was not done with the timing dot: the timing dot was scoped to the measurement discipline only and its constraints kept each harness's row plumbing where it was, so it came out net POSITIVE in lines (a new checked package costs about 30 lines of header and boilerplate before its first word). This one is where the net-negative actually is. Same constraints apply: both entry points' printed output must stay byte-identical except the clock-derived digits, and neither harness's error codes may move.

Gates: tools/codegen-compare.f (0 findings), tools/codegen-workload.f rc=0, tools/codegen-compare-test.f, tools/codegen-workload-test.f, tools/codegen-time-test.f, maki/test.f, the two diff linters, error-code-lint, dot lint.

Claim: agent=pub-unify workspace=.jj-ws/habu-unify-the-publication-1b2dc04a

MEASURED 2026-08-04 (agent=pub-unify). MEASURED-REJECTED, on the same rule and
the same precedent as habu-share-the-timing-2eda3703. No code change.

FIRST, TWO FACTS THE DOT IS WRITTEN AGAINST HAVE MOVED. tools/codegen-time.f
does not exist: the timing dot was itself closed as measured-rejected ("Close
the timing-share dot as measured-rejected", "Keep the timing lane's findings,
not its code"), so SPREAD-OF taking its dead-clock code from the caller is not a
shipped pattern this dot can follow - it is a design that was measured and
turned down. The search words are also the other way round from the description:
FIND-ROW is compare-core's and is the path-filtered one, ROW-OF is
workload-time's and is not.

THE DECISIVE FACT IS THAT THERE ARE NOT TWO COPIES, THERE ARE THREE LIVE
INSTANCES, AND ONE PROCESS HOLDS ALL THREE. Transitive closure of
tools/codegen-compare.f loads codegen-compare-core.f, codegen-compare-baseline.f
AND codegen-compare-gap.f together, and each declares its own count and its own
buffers:

  compare-core.f:133-144   NAME-BYTES / NAME-LENS / variable ROW-N
  compare-baseline.f:71-79 NAME-BYTES / NAME-LENS / variable ROW-N
  compare-gap.f:101-106    GAP-NAMES  / GAP-LENS  / variable GAP-N

A shared checked package whose store is `create`d tables plus a `variable ROW-N`
is a SINGLETON. It cannot serve three live instances in one process - they would
share one count and overwrite each other's rows. The dot's proposal ("a shared
store would take the capacities as its own constants and the cap/row code from
the caller") describes exactly that singleton, so it is not admissible for the
consumer set that actually exists. Serving them needs an INSTANTIABLE store with
caller-owned buffers passed on every call, which inflates ~50 SLOT sites and ~21
ROW-OK sites with extra base arguments and cannot be net-negative.

THE PART THAT CAN BE SHARED ALREADY IS SHARED, and deliberately. compare-core
publishes ROW-MAX / NAME-MAX / OUTPUT-MAX as PUBLIC constants and both
baseline.f and gap.f consume them, with the reason written at
compare-core.f:84-86: "These are public because the reader of a written table
has to allocate the same shape to read it back, and two files guessing the same
numbers separately is how a wider row silently loses its tail." The shape has
one owner. What is left duplicated is the per-instance buffer touch, which
docs/forth.md, "ptr locals and cell access", sanctions in as many words:
"Factor the arithmetic, duplicate the buffer touch."

THE ARITHMETIC ANYWAY, MEASURED, GRANTING THE DOT ITS OWN PREMISE (pretend only
the two harnesses exist and never coexist):

  - A shared ROWSTORE package was written and certified: SLOT, ROWS, ROW-OK,
    NAME-AT, NAME!, NAME$, ROW-OF, BUMP, RESET, with the cap and row codes taken
    from the caller as the dot asks. 42 code lines, 65 raw.
  - compare-core absorbs ~21 code lines (caps, NAME-BYTES/NAME-LENS, ROW-N,
    SLOT, ROW-OK, NAME-AT, NAME!, ROWS, NAME$, the count reset) and must add
    back ~7 (require, an error-code-binding ROW-OK wrapper, a NAME$ wrapper, and
    the public shape re-exports baseline.f and gap.f read). Net -14. FIND-ROW is
    not absorbed: it filters on PATH@, which is compare-core's own column.
  - workload-time absorbs ~25 and adds back ~5. Net -20. Its STR-AT, STR-CP and
    CAP-CK are not absorbed either: they are generic over its SECOND string
    table (FAM) and have to stay for it.
  - Net: -14 -20 +42 = +8 production lines.

So it loses on the arithmetic even before the instance problem, and the reason
is the one LESSONS.md already records: most of the "60 lines stated twice" are
words whose bodies embed a per-harness error code, and the shared version needs
a per-harness wrapper of the same size to bind it. The duplication is real and
the sharing is not cheaper.

Gates on the unchanged tree: codegen-compare 0 findings, codegen-compare-test
green, codegen-workload-test green, maki 200/200, all 16 native suites green,
both diff lints exit 0, error-code-lint 0 findings, dot-dep-lint 0 findings,
gate-stdlib --pool-slots 3: 185 PASS, 0 FAIL, rc 0.

Is this the best long-term answer or a patch? A refusal, and the correct one.
The shape constant - the thing whose divergence would actually corrupt a written
table - already has exactly one owner and is consumed by name. What remains is
buffer touch that the language standard tells us to duplicate, across three
instances a singleton cannot serve.
