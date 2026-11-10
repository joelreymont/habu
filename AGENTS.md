# Habu — checked Forth

Application-specific policy and model workflows stay out of the language core.

Agents write, build and debug everything in Habu and on it; no person uses
Habu directly.

Zen, Tender and Maki build, test and run on Habu without issues. A dot that
removes an issue one of them meets goes before any other work.

Until the three are released, a Habu change is measured by what they do on it:
Maki's image and viewer, Zen's server and worker and Tender's server are built
from their own trees, cold then warm, and Tender's `tender.f` is loaded, on the
candidate engine before it lands and on the installed one after. A fix one of
them waits for lands and is installed alone, ahead of any stack it sits in.
Work that does not move that measure waits. Suites, the chain check and a
feature's review run once, before the install, not per change.

## Repository

- Build and test candidates in a workspace or a temporary directory. Keep one
  installed Habu tree at a stable path, with matching executable, map and
  library sources, and update it in place: no per-revision release
  directories. Ordinary development needs no installation or publication.
- `spec/` holds TLA+ specs of Habu's state machines;
  [spec/README.md](spec/README.md) says how to check them.

## Writing Habu

- Read [docs/forth-card.md](docs/forth-card.md) before writing Habu. Look
  things up in [docs/forth.md](docs/forth.md) at the heading the card names.
- Habu tools are written in Habu: tools, test harnesses and build drivers are
  checked Habu programs run by `bin/hb`. A shell or Python file may only invoke
  `bin/hb` and carries no logic, parsing or policy. The one exception is the
  device-peer scripts under `test/` that stand in for a foreign machine
  (serial, XMODEM, UDP, the embedded assembler hosts, and `test/wasm/run.mjs`
  and `test/wasm/data-peer.mjs`, which run a Wasm module under bun). Any other
  non-Habu file under `tools/` or `test/` is a defect to convert.
- Every module has a real package; public effects keep meaningful types instead
  of reducing everything to `n`.
- Keep unchecked or foreign boundaries explicit, small and tested, and use one
  only after checking that the language cannot express the operation normally.
- When checked code misbehaves, reduce the failure, find whether the
  declaration, checker, compiler or runtime is wrong, and fix that layer.
- Diagnose a native failure by [docs/debugging.md](docs/debugging.md), and
  recover a missing or stale `bin/hb` by [docs/bootstrap.md](docs/bootstrap.md)
  "Recovery".

## Changes

- Use plain English. Keep changes focused, and remove obsolete machinery when
  the language or library already expresses the requirement.
- Never compress code or data without the user's explicit approval.
  Compression shrinks code or data through a codec; dropping zero-filled,
  unused or unreachable storage, packing allocations tightly and choosing a
  compact table encoding are not compression.

## Machinery being replaced

Decisions in DECISIONS.md replace the machinery below. Never extend or patch
it: a defect it shows becomes an acceptance case of the replacing dot, and a
change that needs it joins the replacing design. Something one machine, tier
or build path needs and another does not is a split to remove, not a row to
add.

- arm64's machine-code interpreter, definers and startup in `habu1.f` and
  `habu2.f`: one typed interpreter on every machine ("one compiler").
- Building a product by loading it into the running hb and capturing, cutting
  or rewinding its memory: a cross-build that writes the product as data ("how
  Habu builds Habu").
- The per-definition build cache and its source observers: the file linker
  ("incremental builds", "file objects").
- A linker per target: one linker for every target ("one compiler").
- Tier 1 and the JIT as compilers: T0 on every target; tier 1 changes only to
  fix a defect on a path it still owns ("editing tier 1", "the T0 release").
- `TRUSTED:`, engine-only marks and owner rows: checked forms and private
  sections ("TRUSTED:", "privacy").
- Compatibility with older hosts, formats or schemas: hard cutover ("formats").
- A hook where the owner could record data or make a plain call ("the
  checker's switch", "the source loader's hooks").
- fork without exec: fork only to exec ("fork").

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
  alone does not require it. Check multi-generation convergence
  (`tools/chain-run-build.f`) when codegen, self-hosting or capture changes can
  alter successive engine output. [Etch](../etch) owns its downstream checks.
  Documentation and mechanical changes need proportionate checks.
- Stacks that are ready together land together: one engine built at the
  combined tip, and the full suite and the chain check once there. A lane before
  that runs only its focused test, and a rebase over commits that change only
  `.dots/` reruns nothing. Batch fixes before a suite run: a fix waits for the
  others in work and for its review's fold-ins, and the batch takes one run,
  never one per fix or per fold.
- Report actual results and untested boundaries; never weaken a claim to make a
  check pass. Keep each verification claim within the limits
  [docs/gate.md](docs/gate.md) "Verification claims" states.

## Where findings go

A finding goes where it is checked: a test or a code comment when either can
hold it; [docs/forth.md](docs/forth.md) "Rules learned by refusal" and the card
for a language rule; [docs/bootstrap.md](docs/bootstrap.md),
[docs/gate.md](docs/gate.md) or [docs/debugging.md](docs/debugging.md) for a
build, test or diagnosis rule. Other reference material belongs in `docs/`.
State the rule and the fact that proves it, without dates or narrative, and
search and merge before adding.
