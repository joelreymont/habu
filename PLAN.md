# habu plan

Habu is a checked Forth whose public binary is `bin/hb`. The self-hosted native
engine, bootstrap recovery, build fixpoint, stdlib, and property soundness smoke
are implemented. This plan tracks only active work.

## Implemented

- `bin/hb` self-hosts and rebuilds through the native fixpoint installer.
- No-binary recovery is `HABU_ALLOW_BOOTSTRAP=1 tools/bootstrap.sh`; Gforth is
  recovery-only and installs exactly `bin/hb`.
- The default gate is Habu-native:
  `bin/hb --load lib/errors.f lib/string.f lib/fs.f lib/fs-mutate.f lib/process.f lib/process-argv.f lib/process-env.f lib/test-runner.f test/run.f`.
- The self-hosted lint migration is complete; its temporary root task file is
  retired.
- Property-based checker soundness is implemented in `test/prop-test.f`.
- Reusable property helpers are implemented in `lib/property.f`, tested in
  `lib/property-test.f`, documented in `docs/stdlib.md`, and exposed to the LLM
  benchmark as property tasks.
- Array hard-tail helpers are implemented in `lib/array.f`: scalar kernels,
  reverse, prefix sum, running max, map/fold/scan/find combinators, and bounds
  checks.
- JSONL benchmark merge is checked Habu in `tools/jsonl-merge.f`.
- Codex live benchmark capture uses final-message output, not event-stream text.

## Current Status

`STATUS.md` is the single source of truth for self-check counts and last verified
date. Other docs must point there instead of copying counts.

There is no valid current cross-language claim. The last Codex array matrix was
invalidated by model-output truncation and candidate-capture failure. The harness
is fixed, but the matrix has not been rerun.

Habu is usable for LLM-generated checked code, but not proven best. The current
known gaps are measured benchmark evidence, quotation/combinator hard tails,
diagnostic ablation, and cost proof.

## Active Work

1. Rerun the cross-language LLM matrix with fixed Codex final-message capture.
   Store JSONL/reports outside git or under ignored benchmark output paths.

2. Close remaining LLM hard tails:
   - quotation/combinator tasks;
   - `ARGMAX`, `REVERSE`, `PREFIXSUM`, `RUNMAX`, `COUNT-EVEN` if current reruns
     still show failures or token spikes.

3. Prove diagnostic value with an ablation:
   - structured repair packets;
   - raw checker text;
   - blind generic feedback.

4. Prove or reject cost competitiveness:
   - `tokens_used`;
   - `wall_ms`;
   - `runtime_ms`;
   - `final_chars`;
   - hard-tail token ratios.

5. Keep docs evidence-current:
   - no stale benchmark claims;
   - no generated artifacts committed;
   - `README.md`, `STATUS.md`, `LLM.md`, and `docs/stdlib.md` agree.

## Verification

Focused property soundness:

```sh
bin/hb < test/prop-test.f
bin/hb 2 10 < test/prop-test.f
```

Stdlib property helpers:

```sh
bin/hb --load lib/errors.f lib/test.f lib/property.f lib/property-test.f
```

LLM reference gate:

```sh
bin/hb --load lib/errors.f lib/string.f lib/fs.f lib/fs-mutate.f \
  lib/process.f lib/process-argv.f lib/process-env.f lib/test-runner.f \
  test/gate-common.f bench/llm/run-lib.f bench/llm/run.f
```

Default gate:

```sh
bin/hb --load lib/errors.f lib/string.f lib/fs.f lib/fs-mutate.f \
  lib/process.f lib/process-argv.f lib/process-env.f lib/test-runner.f test/run.f
```

## Root Docs Policy

Root Markdown is for active repo contracts only:

- `AGENTS.md` — repo rules for agents.
- `README.md` — user entry point.
- `PLAN.md` — active work only.
- `STATUS.md` — current verification status.
- `LESSONS.md` — running project memory.
- `TRUSTED.md` — audited trust manifest required by the gate.
- `LLM.md` — concise benchmark/repair protocol.
- `FILEMAP.md` — linted navigation index.

Completed feature plans do not stay in the root.
