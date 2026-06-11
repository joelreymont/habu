# habu — Checked Forth

A checked stack-effect system for Forth, hosted by Gforth. LLM-generated Forth is
verified by a checker (typed, row-polymorphic stack effects) rather than trusting
the model to track the stack by hand. See `PLAN.md` for design and roadmap.

## Conventions

- **Forth words are UPPER-CASE.** Define and call words in upper case
  (`SQUARE`, `CHECKED:`, `DROP`). Lower-case only for `\` comments and prose.
- Gforth target: **0.7.9** at `~/.local/bin/gforth` (built from source; see
  `LESSONS.md`). Keep `~/.local/bin` ahead of `/opt/homebrew/bin` on PATH.

## Forth style (BLOCKING)

See **`docs/forth.md`** for the full Forth standards. In short: our words
UPPER-CASE (built-ins as-is); hyphens not underscores; small factored words with
a `( in -- out )` comment each; shallow stacks (factor / `{: :}` locals, no deep
juggling); `throw` named codes, never silent; named constants; and a
`T{ … -> … }T` test for every word.

- **One concern per file** — never bundle unrelated responsibilities (parser vs
  renderer vs DB vs data table). Split at responsibility seams; it aids review
  and lets files be built in parallel. See `LESSONS.md` § Process.

## Lessons

- **Read `LESSONS.md` at the start of every session, and update it after any
  finding, mistake, or insight.** It is the project's running memory.
- `LESSONS.md` holds lessons only — what worked, what didn't, and why
  (incl. a Process section and Gforth 0.7.9 gotchas).
- NOT for API reference, code snippets, or language patterns — those go in
  `docs/` (see `docs/forth.md`).

## Workflow

- VCS is `jj` (Jujutsu). One change per commit; 50-char imperative subject; no emoji.
- Commit after each significant change or feature; include new files.
