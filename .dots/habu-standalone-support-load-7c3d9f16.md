---
title: gate-runner-support standalone --load fails (missing requires)
status: open
priority: 3
issue-type: task
created-at: "2026-07-05T00:06:00.000000+02:00"
---

Found 2026-07-05 while building an isolated phase-17 harness. The resident
runner support files do not load standalone via `bin/hb --load`, because they
rely on the full ordered resident load rather than requiring their own deps:
`bin/hb --load tools/public-signatures-core.f` fails E-UNDEFINED: COPY-UPPER,
and loading test/gate-runner-support.f's require list standalone dies rc 77
(E-LINT-TOKEN-CAP) from tools/lint/token.f. These files work only because the
full test/run.f resident image loads everything in the right order and forks
phases copy-on-write. Fix: make each support/tool entry require the exact deps
its own top-level path uses (per the "entries require their own dependencies"
lesson), so `bin/hb --load <support-file>` is loadable in isolation and a
per-phase harness (e.g. run-worker-<family>.f driven directly) works without
the whole DAG. This is the same theme as habu-gate-stdlib-standalone-098d7f57
but for the gate-runner support family, not the gate-stdlib entry.

## RCA + fix (2026-07-06, seal-hardening worker)

Failure 1 (E-UNDEFINED: COPY-UPPER) was missing requires, fixed:
tools/public-signatures-core.f now requires tools/lint/text.f +
tools/lint/intern.f (its own top-level deps), and tools/lint/intern.f now
requires lib/errors.f, lib/memory.f, lib/vector.f, tools/lint/text.f instead of
a "Load after" comment. Standalone `bin/hb --load tools/public-signatures-core.f`
is rc 0. Regression proof: tools/standalone-load-test.f (wired into the
stdlib-process-fixtures suite) spawns a fresh `bin/hb --load` child per entry
and asserts exit 0 for text.f / intern.f / public-signatures-core.f.

Failure 2 (gate-runner-support.f standalone rc 77) is NOT E-LINT-TOKEN-CAP and
NOT a missing require. Evidence: (a) the rc-77 exit is NOT catchable (a `catch`
wrapper still dies), so it is the ENGINE dict-room exit `$4D`
(habu2.f C-TRUSTED/C-COLON room check), which writes the pending token
("PS-TOK-BYTE") and exit-groups -- token.f's E-LINT-TOKEN-CAP is a catchable
`throw` that merely shares the number 77; (b) after gate-runner-support.f's
first 42 requires NDICT = 8144 of DICT-CAP 8192, and defining 120 plain dummy
variables dies at #49 with the same rc-77 signature; require #43
(public-signatures-core.f, ~150 defs) simply crosses the ceiling. The
aggregate's closure exceeds the 8192-entry dictionary by construction, so no
require fix can make the WHOLE-DAG image load; the resident runner avoids this
by forking family slices off a lean shared base (test/run-shared-stdlib.f,
which also notes aot-closure.f/clobber-lint.f cannot co-reside). The dot's
stated goal -- a per-phase harness without the whole DAG -- is served by
per-entry standalone loadability (fixed + proven above): a phase harness
requires its own entry, not gate-runner-support.f. Remaining proposals (out of
this change's scope): either raise DICT-CAP (engine layout change, own dot +
owner) or split gate-runner-support.f into per-family support entries and
retire the aggregate usage line in test/gate-runner-lib.f.
