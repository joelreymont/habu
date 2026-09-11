# Worker briefing

Standing conventions for implementation lanes. Dispatch prompts reference this
file; it states each rule once. Current ratchet values are never copied here —
the gate files are the source of truth.

## Workspace discipline

- Work ONLY in your assigned workspace `/home/joel/Work/Habu/.jj-ws/<lane>/`.
  Every Read AND Edit path carries that prefix; never touch
  `/home/joel/Work/Habu/<file>` directly.
- After your FIRST edit, self-check: `jj -R /home/joel/Work/Habu st` must show
  the default workspace UNCHANGED and `jj -R <your workspace> st` must show
  your file. Mismatch = STOP and report.
- Do NOT modify `.dots/`, do NOT move bookmarks, do NOT push. When done,
  `jj -R <workspace> describe -m "..."` and report. The orchestrator merges.

## Trusted boundaries

- Every `TRUST` or `TRUSTED:` site needs source-local rationale, a retirement
  owner, and a focused production-path test.

## Gates and evidence

- Run gates as `... > /tmp/<name>.log 2>&1; echo $?` then
  `rg 'RED:|red phases:' /tmp/<name>.log`. Never `| tail; echo $?` — that
  echoes tail's status and red gates print pass-looking perf lines mid-stream.
- Evidence hazard: the `rg` wrapper in agent shells can silently REWRITE
  matched-line text in its output (line numbers stay correct). Use rg only to
  locate lines; use Read/grep/sed for any text you quote or assert on.
- Both-direction proof: new tests must fail on the unfixed base (run there,
  record it) and pass on your change.

## Test registration

- Register new tests in `test/gate-stdlib-cases.f`. Read the findings lines,
  not exit codes (same for `tools/dot-dep-lint.f`).

## Forth policy

- All Forth runs through `bin/hb`. gforth exists only for no-binary seed
  recovery. If Habu lacks a capability you need, ADD it properly — no
  shell-script workaround for core logic. (Process-spawn idiom:
  Loom's `maki/cross-seq-contraction-test.f`.)

## Diff hygiene

- No diff churn: every hunk directly necessary for the stated change; no
  drive-by refactors — a genuinely better refactor is its own future change.
- One lane does one thing. Anything discovered along the way goes in your
  report as a proposed dot, not in your diff.
