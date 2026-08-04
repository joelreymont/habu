---
title: Retire the calendar check in the stale-status lint
status: closed
priority: 1
issue-type: task
created-at: "2026-07-29T09:49:04.044633+02:00"
closed-at: "2026-08-02T16:55:10.961737+02:00"
close-reason: "Obsolete in exact ancestry: 3b6186ae69bdf689923015c81c77cafee8ad011b deleted STATUS.md and all stale-status machinery."
---

Why: `tools/stale-status-lint-core.f` bundles two unrelated invariants, and only
one of them is worth keeping.

The first is a date check. `CHECK-STATUS` compares the `Last verified` line in
`STATUS.md` against the host's current date and reports a finding when they
differ. That is a pure function of the calendar. Measured on 2026-07-29 at 07:44
with no source change since the previous evening, when the tree was reported
clean: `STALE-STATUS STATUS.md: Last verified is 2026-07-28, expected
2026-07-29` followed by `stale-status-lint: 1 finding(s)`. A check that fails
because a day passed carries no information about the repository, and it has no
stable green state: the only way to keep it green is for a person to edit a date
so the build passes, which is exactly the manual bookkeeping this repository is
removing. Orchestrator decision recorded 2026-07-29 (codex): remove the check,
and do not bump `STATUS.md` merely to satisfy it.

The second is a count-shaped-string check, and it must survive. `SCAN-MD` walks
every tracked Markdown file outside a small allowlist, skips fenced code blocks,
and reports any line that quotes a number which looks like a self-check count,
with the diagnostic at `tools/stale-status-lint-core.f:494` telling the author to
point at `STATUS.md` instead of quoting a number. `STATUS.md:209-213` documents
`STATUS.md` as the single source of truth for those counts and names this lint as
what enforces it. Unlike the date check, this one has a stable green state: it
stays green forever unless somebody writes a count into a document, and when it
fails the failure is precise and actionable. It is roughly 490 of the file's 548
lines and owns five tests.

Recorded contract error, so it is not repeated: this dot originally specified
deleting all three `tools/stale-status-lint*.f` files. A worker checkpoint on
2026-07-29 proved that scope forces a large caller cascade. The gate phase
`GSI-TOOL-DOC-STATUS` at `test/gate-stdlib-inline-lib.f:253-258` contains only
these two files, so deleting them empties a first-class phase; that would in turn
retire `ID-TOOL-DOC-STATUS` and its four sites in `test/gate-runner-lib.f`, three
rows in `FILEMAP.md`, the schedule entry in `tools/suite-coverage-lint-core.f`,
the two paths at `tools/checked-boundary-lint-test-lib.f:242-243`, and a counted
span in `test/gate-stats.f`. That cascade is an artifact of over-broad scope, not
an inherent cost of the fix. It disappears completely when only the calendar
invariant is removed and the files survive. Two supporting claims in that
checkpoint were also wrong and are corrected here: the phase group has no other
doc tests to fall back on, and the phase-id list in `test/gate-runner-lib.f` is
not dense, since it already skips 35 between `ID-TAIL-BUILD` and
`ID-LINT-LIBS-CORE`.

Exact result: delete the calendar invariant and nothing else. Remove
`CHECK-STATUS` and the date machinery that exists only to serve it, which is
`PARSE-TODAY`, `TODAY!`, the date field and accessor set with `DATE$`, and the
`epoch-seconds DATE:SECONDS-DAY /` default. In `CONFIG`, drop the `[TODAY]`
positional from the usage string and reduce the expected positional count
accordingly. In `tools/stale-status-lint-test.f`, remove the two date cases and
rework the `EXPECT-BAD-TODAY` helper so the surviving count-shaped tests no
longer pass a date. All three files remain in the tree, and every count-shaped
test remains and stays green. `STATUS.md` content stays untouched, including both
its date line and its `:209-213` prose, which describes the surviving check and
therefore stays accurate. No registration, phase, phase id, `FILEMAP.md` row,
suite-coverage entry, or gate-stats span may change; if one of them still needs
to change, too much has been removed.

Owner: the three `tools/stale-status-lint*.f` files only. Dependencies: none.

Acceptance: `bin/hb --load tools/stale-status-lint.f` exits 0 and prints
`stale-status-lint: 0 finding(s)` with no date line. `bin/hb --load test/run.f`
goes from red to green, since the worker established that the single red phase on
the unmodified base is `lint-tools/status` and that this date finding is its only
cause. No reachable path in these files reads the host date, proven by reporting
every surviving reference to `epoch-seconds` and the date package. All five
count-shaped tests pass and are named individually. The typed-local and package
exact-diff lints exit 0 on the change, each proven live on that artifact by
injecting one violation and observing the rejection.

Metadata correction: `habu-fold-stale-lint-9d386e95` is NOT superseded by this
work. Its plan to migrate this lint's private output buffers onto
`tools/lint/text.f` stays valid because the files survive. The earlier note in
this dot saying it closes as superseded is withdrawn.

Follow-up to raise as its own leaf, not to be done here: the file is now named
for an invariant it no longer implements. Renaming it to match the surviving
count-check concern touches exactly the cascade surfaces listed above, which is
why it is separate work rather than part of this change.

Claim: agent=claude workspace=.jj-ws/delete-stale-lint
