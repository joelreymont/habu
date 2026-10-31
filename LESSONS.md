# Lessons

What stands now in Habu: each decision of Joel's with its date and his words,
and each lesson learned with its evidence. A reversal edits or removes its
entry.

## Decisions

### Language and checker

- 2026-09-16, TRUSTED:: "TRUSTED: is to be retired entirely". 2026-10-31:
  "shrink TRUSTED: after the T0 release, not before". Each use gets its
  checked form, or the checker gets the model it lacks at its owner; only the
  uncheckable ones stay.
- 2026-10-08, privacy: Joel called the owner rows, the marks on engine-only
  names and the per-caller checks "severe over-engineering". A word is
  restricted by putting it in a package's private section, and the product
  build strips private names.
- 2026-10-27, formats: "why do you need to version any schemas? we have no
  legacy. hard cutover only!" A format change rewrites its writer and reader
  together, with no version number.
- 2026-10-31, a checker word programs can see: asked whether
  REPORT-UNCHECKABLE should become a checker word every program sees or be
  copied into the source verifier, Joel answered "what's best for long term?"
  (as on 2026-10-28 for T0's error constants, "dude, what's best for long
  term?!", and on 2026-10-30 for CURL:HEADER+ refusing CR/LF). The lead chose
  the checker word: one copy of the rule at its owner.
- 2026-10-31, names: offered REFUSAL-TEXT for the package of the engine's
  error-message words, Joel answered "what not error text? wtf is a refusal?
  use standard programming terms! and you can have error line and column
  words, yes". Asked why definition.f had its own ERROR-TEXT word: "wouldn't
  it be the same error-text as far as meaning? why have several?" Names use
  standard programming terms (error, not refusal), and helpers with one
  meaning are one word.
- 2026-10-31, protocol terms: offered MCP:ANSWER-BLOCKS for a tool answer
  with image and file content, Joel asked "if mcp calls them context blocks
  then why are you calling them answer blocks?" and "what is ANSWER? again,
  makes no sense", then chose MCP's terms over keeping the ANSWER words. A
  library for a protocol names things in that protocol's words: a tool's
  result and its content blocks, not answers. The same day, for MODEL's
  OpenAI- and Ollama-shaped client, Joel chose MODEL:CHAT and
  MODEL:E-RESPONSE over ASK and E-ANSWER.
- 2026-10-31, model clients: asked whether phil's MODEL client (chat and
  embeddings over CURL, its key from the credential store) should become
  part of Habu's library, Joel chose "Add it to Habu": a general protocol
  client belongs in src/lib beside MCP and CURL.

- 2026-10-31, a public XML word: asked whether lane cown may add
  `XML:EMPTY-CURSOR ( ptr u8 n -- ptr XML:cursor )`, which checks the size and
  zero-fills the record, so the XML C2 reader stops casting bytes into XML's
  OPAQUE cursor from another package, Joel chose "Add EMPTY-CURSOR".
- 2026-10-31, a public verifier code: asked whether the source verifier's
  private missing-signature code may become public as
  `VERIFY:E-MISSING-SIGNATURE`, so tools/check.f names that error with its own
  code, message and fix hint instead of a generic statement throw, Joel chose
  "Export the constant".

### Compiler and builds

- 2026-10-22, incremental builds: "i said that a file is the unit of
  incremental building. all the defining words run when the file is
  compiled." Everything compiling a file produces, TRUSTED: bodies and does>
  definers included, is cached and replayed as one unit; no per-definition
  reuse.
- 2026-10-28, one compiler: "we want a unified backend and frontend that
  allows for cross-compilation. so we want to reuse as much as possible and
  only change arch-specific bits." One T0 front end, one publisher and
  capture path, an emitter per architecture.
- 2026-10-29: "finish T0 incorporating the best of JIT into it, then retire
  JIT". 2026-10-30: "make sure you are not reinventing the wheel when things
  have been implemented in the jit and t1".
- 2026-10-30, the T0 release: "there's no speed bar. we release t0 AS IS. we
  keep JIT and t1 around until we are satisfied we are as fast or faster and
  we'll need them for experiments. what comes first is the release of new
  architecture, though. then we work on speeding it up."
- 2026-10-29, arm64 first: "you need to finish arm64, then all the others like
  x86 and wasm. unless you can prove that editing x86 does not hurt your
  speed." 2026-10-30, on an Intel build in the arm64 release gate: "why are you
  doing intel when the focus is arm64? is that necessary?" 2026-10-31: "after
  you are done with trusted you should start looking at x86 and wasm."
  2026-10-31, on how other work fits the T0 plan: "i want small tight
  improvements on top of t0."
- 2026-10-29, build cost: "i need fast incremental builds to enable you and
  other agents to quickly iterate on features. right now we are in rust cargo
  build times and it fucking sucks because we are forth!" A build whose cost
  is out of proportion has a defect to root-cause first.
- 2026-10-31, startup reuse: asked to approve a public word that compares the
  tree's source hashes with the engine's so hb-build could skip its 3.3 s
  startup reload, Joel answered "why are we developing incremental building if
  we are gonna check hashes of source files? one of the goals was incremental
  building of habu itself". The reload goes through the incremental cache, not
  a second mechanism.
- 2026-10-27, evidence: "so make it happen in your branch and test it out!"
  and "you should be focusing on this, prove me wrong!" When a decision lacks
  evidence because a capability does not exist yet, build it in a branch and
  measure.

### Tests

- 2026-10-20, other targets: "we need elf32 and the TI code. also the intel
  code." On their tests, the same day: "the platform tests should have
  platform switches around them, not be trimmed!"
- 2026-10-29, on a T0-only fixture row: "should you stop creating tests like
  install and print? do they carry their weight?" They do not: once the suite
  runs through T0, every row tests T0.
- 2026-10-30: "we decided to build everything in a single binary, why are you
  launching processes? are you trying to prove that the program you build
  runs?"

### Docs

- 2026-10-23: "if you are trying to sync docs with code it means you should
  drop the docs and document the code instead." A doc passage that must track
  code is deleted and the fact goes into the code's comment.

### Installing

- 2026-10-28: "remember that there's just 1 public hb copy now, no versions!"
  The public Habu is ~/.local/lib/habu, updated in place.
- 2026-10-28: "nobody should ask to hold anything. they should have their own
  copy of habu to run if they need". An install never waits on a client.

## Lessons

- `jj absorb` without `--into` rewrites master: this repository's
  `immutable_heads()` is `none()`, and one bare absorb rewrote master and 4289
  descendants. Name the target commits with `--into`.
