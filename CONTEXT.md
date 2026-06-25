# Handoff Context

Date: 2026-06-25

This file is the pickup point for the next agent. The conversation history is not
required; use this file plus the committed docs and current tree as authority.

## Current State

- Repository: `/home/user/Work/habu`
- VCS: `jj`
- Remote branch: `origin/master`
- Current pushed tip: `3194ef7666ae6da4340d2be39a385f763d0b873c`
- Current commit subject: `Clean engine/imgdump stack effects`
- Expected local state before continuing: clean working copy on top of `master`
- Factorization parent dot: `habu-review-whole-repo-5e087327`
- Only remaining factorization child: `habu-factor-darwin-spawn-5a82930c`

`docs/factorization-review.md` is the durable audit record. The local `.dots/`
store is ignored, so do not rely on it being present in a fresh checkout.

## What Is Done

All all-repo factorization review rows except F04 are implemented and
Linux/aarch64 validated. In particular:

- F01, F02, F03, F05, F06, F07, F08, F09, F10, F11, F12, F13, F14, F15, F16,
  F17, F18, F19, F20, F21, F22, F23, and F24 are closed in the committed review
  record.
- F24 was the last Linux-actionable row. It added definition-local stack effects
  in `test/engine-suite.f`, fixed `tools/imgdump.f` comments-before-locals,
  corrected `E-NAME`'s documented `ptr u8 n` output, and renamed `h.` to `H.`.
- Linux validation after F24 passed:
  - `bin/hb test/engine-suite.f`
  - focused Linux `tools/imgdump.f`/`tools/imgdump-test.f` load
  - `test/gate-stdlib.f`
  - full native gate from `docs/bootstrap.md`

F04 source factoring is also already implemented in `src/habu/habu1.f`. The
Darwin `BSPAWNIO` variants now share frame enter/leave, action reset, stdio
dup2 append, descriptor zero/fill, nullable descriptor, argv/envp register
setup, and `posix_spawn` finish helpers. `tools/spawn-emitter-test.f` guards the
source shape and removed duplicated literal sequences.

Linux/aarch64 evidence for F04 already passed:

- `tools/spawn-emitter-test.f`
- process/process-argv/process-env/process-cwd focused fixtures
- `trust-lint`
- `filemap-lint`
- `stale-status-lint`
- full native gate

F04 remains open only because this Linux host cannot execute the required macOS
runtime validation of Darwin `posix_spawn`.

## Remaining Work

Finish F04 on a macOS ARM64 host.

Required validation:

1. Confirm the tree is at or after `3194ef7666ae6da4340d2be39a385f763d0b873c`.
2. Run the Darwin spawn source-shape regression.
3. Run macOS process runtime fixtures.
4. Run the PTY slice.
5. Run the full native port gate.
6. If all pass, update `docs/factorization-review.md` with exact macOS evidence,
   close F04, close the parent factorization dot, commit, move `master`, and push.
7. If any fail, keep F04 open, root-cause with the native debugger/stepper tools
   in `docs/debugging.md`, add the smallest correct fix/regression, validate, and
   commit that focused batch.

Do not run LLM benchmark gates for this. They are benchmark-host work, not
factorization or platform-port validation.

Do not validate macOS by injecting a Linux target prelude or by running Linux
tests from macOS. `bin/hb` should select its own host target sources.

## macOS Commands

Start by syncing:

```sh
jj git fetch
jj st
git ls-remote origin refs/heads/master
```

If `bin/hb` is missing, recover it exactly as `docs/bootstrap.md` says. Gforth is
only for no-binary recovery. Once `bin/hb` exists, use only native Habu.

Refresh native `bin/hb` on the Mac:

```sh
bin/hb --load lib/errors.f lib/string.f lib/fs.f lib/fs-mutate.f lib/process.f \
  lib/process-argv.f lib/process-env.f lib/codesign.f tools/build-fixpoint.f \
  tools/build-fixpoint-main.f -- install
```

Run the F04 source-shape regression:

```sh
bin/hb --load lib/errors.f lib/string.f lib/test.f lib/fs.f \
  tools/spawn-emitter-test.f
```

Run the focused process fixtures:

```sh
bin/hb --load lib/errors.f lib/string.f lib/test.f lib/fs.f lib/fs-mutate.f \
  lib/process.f lib/process-argv.f lib/process-test.f

bin/hb --load lib/errors.f lib/test.f lib/process.f lib/process-argv.f \
  lib/process-argv-test.f

bin/hb --load lib/errors.f lib/string.f lib/test.f lib/fs.f lib/process.f \
  lib/process-argv.f lib/process-env.f lib/process-env-test.f

bin/hb --load lib/errors.f lib/string.f lib/test.f lib/fs.f lib/fs-mutate.f \
  lib/process.f lib/process-argv.f lib/process-env.f lib/process-cwd.f \
  lib/process-cwd-test.f
```

Run the PTY slice from `docs/process-pty.md`:

```sh
bin/hb --load lib/errors.f lib/string.f lib/fs.f lib/process.f \
  lib/process-argv.f lib/process-env.f lib/test.f test/proc-pty.f
```

Run the full native gate:

```sh
bin/hb --load lib/errors.f lib/string.f lib/fs.f lib/fs-mutate.f lib/process.f \
  lib/process-argv.f lib/process-env.f lib/test-runner.f test/run.f
```

## Success Closeout

If all macOS checks pass:

1. Update `docs/factorization-review.md`:
   - add a F04 macOS validation bullet with exact command names and pass output;
   - change the continuation section so no factorization rows remain open;
   - update verification status to say the parent factorization review is closed.
2. Close local dots if present:

```sh
dot off habu-factor-darwin-spawn-5a82930c -r "completed: Darwin spawn factoring validated on macOS with process, argv, env, cwd, PTY, and full native gate."
dot off habu-review-whole-repo-5e087327 -r "completed: all factorization findings implemented and validated; F04 closed on macOS."
```

3. Run a final docs/status lint if docs changed:

```sh
bin/hb --load tools/date.f lib/errors.f lib/string.f lib/fs.f \
  tools/lint/text.f tools/lint/token.f tools/lint/lib.f tools/argv.f \
  tools/stale-status-lint.f
```

4. Commit and push with `jj`:

```sh
jj st
jj diff
jj commit -m "Close factorization review"
jj bookmark move master --to @-
jj git push --bookmark master
git ls-remote origin refs/heads/master
```

## Failure Path

If a macOS process or PTY check fails:

- Keep `habu-factor-darwin-spawn-5a82930c` open.
- Do not add guards or fallbacks to paper over the failure.
- Use existing native tools first: `docs/debugging.md`, REPL `step`, breakpoints,
  watch cells, `tools/jitdump.f`, and `tools/imgdump.f`.
- Root-cause whether the bug is in register mapping, frame layout, file-action
  descriptor construction, argv/envp setup, cwd action setup, or the shared
  `posix_spawn` finish helper.
- Add the smallest focused regression that would have caught the failure.
- Commit each significant fix as its own `jj` change after focused validation.

## Out Of Scope For This Handoff

The following dots are intentionally separate from the factorization parent and
do not block closing F04:

- `habu-replace-creates-with-d9c4b404`
- `habu-add-typed-byte-b25e923e`
- `habu-model-engine-builder-38ddc643`
