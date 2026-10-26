# Habu — checked Forth

Habu is a general-purpose Forth with a native compiler and a stack-effect type
checker. Application-specific policy and model workflows stay out of the
language core; [Etch](../etch), a PCB CAD application, is a separate project.

## Repository

- The root checkout stays on `master` with no work of its own; edits happen in
  workspaces under `.jj-ws.noindex/`.
- Build and test candidates in a workspace or a temporary directory. Keep one
  installed Habu tree at a stable path, with matching executable, map and
  library sources, and update it in place: no per-revision release
  directories. Ordinary development needs no installation or publication.

## Writing Habu

- Read [docs/forth-card.md](docs/forth-card.md) before writing Habu: naming and
  packages, typed effects and locals, what the checker refuses and admits, the
  storage definers, errors, require order and tests, with each refusal measured
  on the engine. Look things up in [docs/forth.md](docs/forth.md) at the
  heading the card names.
- Habu tools are written in Habu: tools, test harnesses and build drivers are
  checked Habu programs run by `bin/hb`. A shell or Python file may only invoke
  `bin/hb` and carries no logic, parsing or policy. Two exceptions exist:
  `tools/bootstrap.sh`, the recovery launcher for when `bin/hb` does not exist,
  and the device-peer scripts under `test/` that stand in for a foreign machine
  (serial, XMODEM, UDP, the embedded assembler hosts, and `test/wasm/run.mjs`
  and `test/wasm/data-peer.mjs`, which run a Wasm module under bun). Any other
  non-Habu file under `tools/` or `test/` is a defect to convert.
- Every module has a real package; public effects keep meaningful types instead
  of reducing everything to `n`.
- Keep unchecked or foreign boundaries explicit, small and tested, and use one
  only after checking that the language cannot express the operation normally.
- When checked code misbehaves, reduce the failure, find whether the
  declaration, checker, compiler or runtime is wrong, and fix that layer. The
  type checker does not prove unmodeled semantics.
- Native failures: [docs/debugging.md](docs/debugging.md). Missing or stale
  `bin/hb`: [docs/bootstrap.md](docs/bootstrap.md) "Recovery routes".

## Changes

- Use plain English. Keep changes focused, and remove obsolete machinery when
  the language or library already expresses the requirement.
- Optimize code, data structures and architecture for the long term.
- Never compress code or data without the user's explicit approval.
  Compression shrinks code or data through a codec; dropping zero-filled,
  unused or unreachable storage, packing allocations tightly and choosing a
  compact table encoding are not compression.
- A significant feature gets an independent adversarial review before it lands.

## Testing

- Prefer end-to-end tests as the only mechanism; each ends with a verifiable,
  repeatable artifact. Never write unit tests after the code.
- Keep an isolated test only when it catches a concrete failure the end-to-end
  tests miss, and list the ways the system can fail before writing it. Delete
  redundant assertions, implementation mirrors and change detectors.
- Test changed behavior through the real load path: rejected programs for type
  rules, meaningful edge cases for runtime changes.
- Run compatible cases through one loaded tool or built application. Start a
  fresh process or build only when that boundary is under test, and state why
  beside it. Measure the affected row's wall and CPU time before and after
  adding expensive setup. Never split a slow row just to meet a per-row limit:
  remove repeated work first, counting duplicated setup in the suite total.
- Run focused tests while developing, and rebuild `bin/hb` when the changed
  behavior is baked into it. Run the full suite (`bin/hb --load test/run.f`)
  for shared checker or compiler behavior, runtime ABI, capture format, effects
  across libraries, or an impact focused tests cannot bound; a file's location
  alone does not require it. Check multi-generation convergence when codegen,
  self-hosting or capture changes can alter successive engine output. Gforth
  recovery is an occasional check for seed, mirror, launcher or recovery
  dependency changes, or a release recovery audit. Etch owns its downstream
  checks. Documentation and mechanical changes need proportionate checks.
- Report actual results and untested boundaries; never weaken a claim to make a
  check pass. [docs/gate.md](docs/gate.md) "Verification claims" states the
  limits.

## Where findings go

A finding goes where it is checked: a test or a code comment when either can
hold it; [docs/forth.md](docs/forth.md) "Rules learned by refusal" and the card
for a language rule; [docs/bootstrap.md](docs/bootstrap.md),
[docs/gate.md](docs/gate.md) or [docs/debugging.md](docs/debugging.md) for a
build, test or diagnosis rule. Other reference material belongs in `docs/`.
State the rule and the fact that proves it, without dates or narrative, and
search and merge before adding.
