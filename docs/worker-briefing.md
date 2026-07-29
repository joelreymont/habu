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

## Size ratchets (engine-touching lanes)

- Any change under `src/` (boot-loaded engine source) obligates a SAME-COMMIT
  update of every affected row: `test/gate-size-attribution-test.f`
  (LINUX-CODE-TEXT, LINUX-FLOOR-DIST, LINUX-TOTAL, and the per-region
  LINUX-REGION-BUDGETS rows — regions must sum exactly to CODE-TEXT),
  `test/gate-build-size.f` (GB-SIZE-BASELINE-LINUX), and the STATUS.md census
  row "Certified (linux-arm64): N".
- MEASURE then transcribe, never predict: rebuild with
  `HABU_ENGINE_SIZE_MAP=1 ... install --force`, run `tools/size-report.f`,
  copy the measured numbers. Arithmetic predictions have been wrong twice.
- macOS rows are OWED, never guessed. Update linux-arm64 rows only.

## TRUSTED.md

- Any row you add cites a STANDING long-term dot as owner — never your own
  lane's dot, which closes at merge and reds the strict lint on the next gate.

## Gates and evidence

- Run gates as `... > /tmp/<name>.log 2>&1; echo $?` then
  `rg 'RED:|red phases:' /tmp/<name>.log`. Never `| tail; echo $?` — that
  echoes tail's status and red gates print pass-looking perf lines mid-stream.
- ONE timing lane machine-wide. Unless the orchestrator granted you the timing
  lane, do not run perf/timing passes; a perf verdict that fails under
  contention is environment-deferred — land on correctness-green + stable sha.
- Evidence hazard: the `rg` wrapper in agent shells can silently REWRITE
  matched-line text in its output (line numbers stay correct). Use rg only to
  locate lines; use Read/grep/sed for any text you quote or assert on.
- Both-direction proof: new tests must fail on the unfixed base (run there,
  record it) and pass on your change.

## Test registration

- Register new tests in `test/gate-stdlib-cases.f` and
  `test/gate-stdlib-inline-lib.f`. Confirm with the suite-coverage lint, reading
  the findings lines, not exit codes (same for `tools/dot-dep-lint.f`).

## Forth policy

- All Forth runs through `bin/hb`. gforth exists only for no-binary seed
  recovery. If Habu lacks a capability you need, ADD it properly — no
  shell-script workaround for core logic. (Process-spawn idiom:
  `maki/eval/device-fault-test.f`.)

## New-op discipline (op-adding lanes)

Follow the dropout/SwiGLU template end to end: `maki/op-kind.f` enum +
OPKIND>N, `maki/op-registry.f`, `maki/adjoint.f`, `maki/executor.f`,
`maki/cad.f` token + shape rule, `maki/backward.f`, `maki/move-facts.f`,
device kernel under `tools/ptx/`, `perf-watch.f` registration, and a
`perf-rows.tsv` WAIVER row proven load-bearing.

## Diff hygiene

- No diff churn: every hunk directly necessary for the stated change; no
  drive-by refactors — a genuinely better refactor is its own future change.
- One lane does one thing. Anything discovered along the way goes in your
  report as a proposed dot, not in your diff.
