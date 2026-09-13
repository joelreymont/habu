# Habu — checked Forth

Habu is a general-purpose Forth with a native compiler and a stack-effect type
checker. [Loom](../loom) is GPU model CAD; [Maki](../maki) is PCB CAD. Both are
separate applications. Application-specific policy and model workflows do not
belong in the language core.

## Working

- Choose the simplest effective approach. Work directly or delegate bounded
  tasks when useful; there is no required agent chain or fixed review round.
- Infer routine details and complete authorized work. Ask only for a material
  missing choice or additional authority. User instructions take precedence.
- Use plain English. Keep changes focused and remove obsolete machinery when
  the existing language or library already expresses the requirement.
- Use `jj`, preserve unrelated changes, and commit coherent completed work.
  Use separate workspaces when concurrent edits would conflict. Create Habu
  workspaces under this repository's `.jj-ws/`, never directly under `~/Work/`.
- Use dots or blackboard only when the task calls for them. Neither is a gate
  for coding, commits, reviews or ordinary communication. Do not post messages
  on the user's behalf without authorization.

## Language and implementation

- Habu tools are written in Habu. Tools, test harnesses and build drivers are
  checked Habu programs run by `bin/hb`; a shell or Python file may only invoke
  `bin/hb` and never carries logic, parsing or policy of its own. Two exceptions
  exist by design: `tools/bootstrap.sh`, the no-binary recovery launcher that
  runs when `bin/hb` does not exist, and the device-peer scripts under `test/`
  (serial, XMODEM, UDP and the embedded assembler hosts) that stand in for a
  foreign machine; `formal/` builds its proofs with rocq's own Makefile.
  Anything else under `tools/` or `test/` that is not Habu is a defect to
  convert.
- Read [docs/forth.md](docs/forth.md) before writing Habu. It documents naming,
  packages, small factored words, typed stack effects and native tooling.
- Every module has a real package; public effects preserve meaningful types
  instead of reducing everything to `n`.
- Keep necessary unchecked or foreign boundaries explicit, small and tested.
  First check whether the current language can express the operation normally.
- When checked code behaves incorrectly, reduce the failure and identify whether
  the declaration, checker, compiler or runtime is wrong. Fix the responsible
  layer and add a regression; a type checker does not prove unmodeled semantics.
- Use [docs/debugging.md](docs/debugging.md) for native failures. If `bin/hb`
  is missing or stale, follow [docs/bootstrap.md](docs/bootstrap.md).

## Verification and documentation

- Test changed behavior through the real load path. Include rejected programs
  for type rules and meaningful edge cases for runtime changes.
- Run focused tests while developing. Rebuild and run `bin/hb --load test/run.f`
  for compiler/runtime or broad library changes that need the full suite.
  Documentation, moves and other mechanical changes need proportionate checks.
- Report actual results and untested boundaries. Never weaken a claim to make a
  check pass. [docs/proofs.md](docs/proofs.md) explains proof/model limits.
- Keep durable technical findings concise in [LESSONS.md](LESSONS.md); reference
  material belongs in `docs/`. No mandatory ledgers, claim choreography,
  mutation campaigns or ritual response templates.
