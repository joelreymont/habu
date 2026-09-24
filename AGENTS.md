# Habu — checked Forth

Habu is a general-purpose Forth with a native compiler and a stack-effect type
checker. [Maki](../maki) is a separate PCB CAD application.
Application-specific policy and model workflows do not
belong in the language core.

## Working

- Use parallel agents for independent work when it materially helps completion.
  The task authorizes bounded delegation without a separate user request.
  Keep small or tightly coupled work direct, with no fixed agent chain.
  Define each worker's ownership and acceptance checks before starting it.
  Editing workers use isolated `.jj-ws/<task>` workspaces; the integrator owns
  review, integration, verification and cleanup.
- Infer routine details and complete authorized work. Ask only for a material
  missing choice or additional authority. User instructions take precedence.
- Use plain English. Keep changes focused and remove obsolete machinery when
  the existing language or library already expresses the requirement.
- Use `jj`, preserve unrelated changes, and commit coherent completed work.
  Use separate workspaces when concurrent edits would conflict. Create Habu
  workspaces under this repository's `.jj-ws/`, never directly under `~/Work/`,
  and only from the repo root: one workspace name maps to one directory (a
  nested duplicate checkout silently resets the other's edits; a divergent
  change id is the tell). An edit exists only after a jj snapshot — `jj
  describe -m 'WIP: …'` after each coherent step; `jj workspace update-stale`
  rebuilds an undescribed working copy, and the lost tree comes back from
  `jj op log` (`jj --at-op <op> log -r <change>`, `jj restore --from <commit>`),
  never from `jj op restore` while peers are live.
- Temporary workspaces need no bookmark. The integrator owns cleanup: verify
  every task change is integrated or explicitly retained, run the required
  checks, and push `master`; then delete its local and remote task bookmarks,
  `jj workspace forget` the workspace and `trash` its directory. Never infer
  equivalence from commit subjects alone. Finish by checking the bookmark and
  workspace inventories; retain only active work with a stated reason.
- A gate and its push never share one unconditional command chain: run the
  gate, read its exit code on its own line, then push.
- Use dots only when the task calls for them; they are not a gate for coding,
  commits, reviews or ordinary communication. `dot off` archives the file and
  orphans every `blocks:` edge naming it — search `.dots/` for the id first and
  sweep the edges in the same commit. Use the session's agent messaging for
  coordination. Do not post messages on the user's behalf without authorization.

## Language and implementation

- Habu tools are written in Habu. Tools, test harnesses and build drivers are
  checked Habu programs run by `bin/hb`; a shell or Python file may only invoke
  `bin/hb` and never carries logic, parsing or policy of its own. Two exceptions
  exist by design: `tools/bootstrap.sh`, the no-binary recovery launcher that
  runs when `bin/hb` does not exist, and the device-peer scripts under `test/`
  (serial, XMODEM, UDP and the embedded assembler hosts) that stand in for a
  foreign machine.
  Anything else under `tools/` or `test/` that is not Habu is a defect to
  convert.
- Read [docs/forth-card.md](docs/forth-card.md) before writing Habu. It is the
  worker-sized card: naming and packages, typed effects and locals, what the
  checker refuses and admits, the storage definers, errors, require order and
  tests, with every refusal measured on the engine. Open
  [docs/forth.md](docs/forth.md) for lookups, at the heading the card names.
- Every module has a real package; public effects preserve meaningful types
  instead of reducing everything to `n`.
- Keep necessary unchecked or foreign boundaries explicit, small and tested.
  First check whether the current language can express the operation normally.
- When checked code behaves incorrectly, reduce the failure and identify whether
  the declaration, checker, compiler or runtime is wrong. Fix the responsible
  layer and verify the behavior through the testing policy below; a type checker
  does not prove unmodeled semantics.
- Use [docs/debugging.md](docs/debugging.md) for native failures. If `bin/hb`
  is missing or stale, follow [docs/bootstrap.md](docs/bootstrap.md).

## Verification and documentation

- Never write unit tests after you write code.
- Highly prefer E2E tests as the sole testing mechanism. Use them to verify
  complex features work. At the end of E2E tests, produce a verifiable and
  repeatable artifact.
- If you must test a system in isolation, first write down all the ways it
  could fail, then write the code.
- Keep an isolated test only when it catches a concrete failure the E2E tests
  miss. Delete redundant assertions, implementation mirrors and change detectors.
- Test changed behavior through the real load path. Include rejected programs
  for type rules and meaningful edge cases for runtime changes.
- Run focused E2E tests while developing. Rebuild and run `bin/hb --load test/run.f`
  for compiler/runtime or broad library changes that need the full suite.
  Documentation, moves and other mechanical changes need proportionate checks.
- Report actual results and untested boundaries. Never weaken a claim to make a
  check pass. [docs/proofs.md](docs/proofs.md) explains verification limits.
- A finding goes where it is checked: a test or a code comment if either can
  hold it; [docs/forth.md](docs/forth.md) (its "Rules learned by refusal"
  section) and the card for a language rule; [docs/bootstrap.md](docs/bootstrap.md),
  [docs/gate.md](docs/gate.md) and [docs/debugging.md](docs/debugging.md) for a
  build, test or diagnosis rule; this file for how agents work. State the rule
  and the fact that proves it, without dates or narrative; search and merge
  before adding. There is no LESSONS.md. Reference material belongs in
  `docs/`. No mandatory ledgers, claim choreography, mutation campaigns or
  ritual response templates.
