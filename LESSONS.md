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
  MODEL:E-RESPONSE over ASK and E-ANSWER. Asked whether MODEL's journal
  record should keep its "answer" key, which saved Zen job steps already
  carry, Joel chose "Rename to content": stored records use the protocol's
  word too, with a hard cutover.
- 2026-10-31, model clients: asked whether phil's MODEL client (chat and
  embeddings over CURL, its key from the credential store) should become
  part of Habu's library, Joel chose "Add it to Habu": a general protocol
  client belongs in src/lib beside MCP and CURL. The same day: "make sure
  model library logs failures, errors, etc. dumping the textual reason. we
  should have a logging library in habu regardless". Offered names for it,
  Joel chose `LOG:LINE ( level tag -- ) … LOG:;LINE` with levels LOG:ERROR
  and LOG:WARN, LOG:TEXT+ and LOG:INT+; FMT:INT$ ( n -- text ) for a
  number's decimal text; CURL:ERROR$ for libcurl's message for a code; and
  `STDERR:CAPTURE ( path -- ) … STDERR:;CAPTURE` for tests. Asked whether
  MODEL's log line for an unusable reply carries the whole reply body, Joel
  chose "Log the whole body".
- 2026-10-31, typed data: shown that the image save check reads an untyped
  `c,` table's bytes as possible addresses, so a build fails at random by
  where memory was placed, and that a fix recording `c,` runs covers only
  part of it, Joel answered "regardless of how we build this chunk of data,
  why are we not typing it? that would fix all the issues and avoid having to
  record any runs. prove me wrong!" Whether data can hold an address is
  decided by its declared type, not by how it was built or by its value.

- 2026-10-31, a public XML word: asked whether lane cown may add
  `XML:EMPTY-CURSOR ( ptr u8 n -- ptr XML:cursor )`, which checks the size and
  zero-fills the record, so the XML C2 reader stops casting bytes into XML's
  OPAQUE cursor from another package, Joel chose "Add EMPTY-CURSOR".
- 2026-10-31, a public verifier code: asked whether the source verifier's
  private missing-signature code may become public as
  `VERIFY:E-MISSING-SIGNATURE`, so tools/check.f names that error with its own
  code, message and fix hint instead of a generic statement throw, Joel chose
  "Export the constant".
- 2026-10-31, typed storage with a run-time lifetime: for the global storage
  word whose `NAME t` makes NAME-NEW ( -- ptr t ), a fresh zero-filled record,
  and NAME-RELEASE ( ptr t -- ), Joel chose HEAP over RECORDS and MAPPED. For
  the global word that gives each task its own typed cell, he first objected
  that "TASK:TASK-LOCAL or even TASK:TASK-VARIABLE doesn't read well"; told it
  is a core word in no package, beside TYPED-VARIABLE, he chose TASK-LOCAL
  over TASK-VARIABLE and USER-VARIABLE. Asked next to approve moving the band
  counter and slot hook into the core for it, he asked "why in the world do you
  need TASK-LOCAL when you have TASK:+USER?": Maki's per-task typed pointer
  already runs on `TASK:+USER` with one private cast (maki
  src/project-ops.f:176-182), so TASK-LOCAL is dropped.
- 2026-10-31, cast errors: asked whether lane casterr may make
  CAST-ERROR:START, TOKEN!, FAMILY!, RAISE and SKIP and
  LOWER-CERT-HOOK:LINE-AT public, because checker words outside those
  packages call them, until nested packages make CAST-ERROR a part of CHECKER
  and hide them, Joel chose "Approve, DIAG-XT private": the print hook, used
  only inside CAST-ERROR, stays private.
- 2026-10-31, error places: asked whether SRCLOC:FILE-BUFFER? ( -- bool ),
  true while the text being read is the open file's own bytes, may be public
  so the Habu loop's and the native compiler's error-place writers share one
  rule (the file's line for the file's text, the file alone for an evaluated
  string), Joel chose "Add SRCLOC:FILE-BUFFER?".
- 2026-10-31, packages: offered a third package section so engine-only
  checker words stay callable from other engine packages but hidden from
  programs, Joel answered "private means NOT FUCKING AVAILABLE OUTSIDE THIS
  PACKAGE" and "my point is that compiler words should live in the comp
  package, called internally without the package prefix. why would we split
  the compiler package into several packages?", chose "One compiler package",
  and added "if we find that we are clashing too much then we ought to look
  into nested packages but that's about it". A subject's files reopen one
  package and call its shared words bare, in its private section; no third
  section. The same day, shown that 35 of the 40 compiler packages share
  names with another (336 clashes, plus 315 arm64/x86-64 twins), Joel
  answered "add nested packages but i approve the design": a package opened
  inside another is its part, whose name and public words exist only inside
  the parent. On names: "COMP and XCOMP are the compiler and cross-compiler
  packages. CHECKER is fine". Asked which files XCOMP holds: "that is for you
  to decide. do what's best for long term". The lead chose COMP for compiling
  (front end, IR, T0 and every architecture's back end as parts, so each
  engine compiles itself through COMP) and XCOMP for building an image for a
  target. Joel chose "One level" of nesting: a part holds no parts.
- 2026-10-31, a part's head resolver: inside package A, `: B:Q … ;` defines
  into part A:B, but the native compiler's name check found only top-level
  packages and the Habu loop's resolver was private to OUTER. Asked whether
  xref.f's XREF package may get a public `XREF:HEAD-ROW ( ptr u8 n -- ptr n )`,
  the row a qualifier's head names (the open package's part first, then a
  top-level package), called by the Habu loop and the compiler, Joel chose
  "Add XREF:HEAD-ROW".
- 2026-10-31, a public SSE word: asked whether SSE:FEED's callback context
  may change from `ptr a` to `a`, so MCP passes its client record without a
  cast, Joel chose "Context becomes `a`".
- 2026-10-31, comparing signed builds: codesign writes the signing time into
  every signature, even with `--timestamp=none`, so two team-signed builds of
  the same program differ. Asked to name the one shared word that compares
  two images, skipping only the signature block that holds the time, Joel
  chose "CODESIGN:SAME-IMAGE?" over a global SIGNED-SAME? in sign2.f.
- 2026-10-31, a PostgreSQL limit: asked whether PG:CONFIGURE should lose its
  fixed per-connection parameter count so each connection's list grows as BIND
  needs, up to PostgreSQL's own 65535, Joel chose "Grow to PostgreSQL's limit".
  A library takes its limits from the protocol it speaks, not from a preset.
- 2026-10-31, Maki's Habu words (design r2): offered CMD:RUN-LINES for a run
  that hands each stdout line to a callback and ends the child after a silence
  window, Joel asked "why are we using \"a stretch of silence\" as terminator?";
  told the child ends by exiting and silence is only his Maki stall rule (5
  minutes with no progress line, no total limit), he chose CMD:RUN-STREAM. For
  holding off idle sleep he chose POWER:KEEP-AWAKE / POWER:ALLOW-SLEEP over
  WAKE-LOCK and HOLD; for runner sizing, OS-CPU:PERFORMANCE-CORES and
  OS-MEMORY:TOTAL.

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
- 2026-10-31, what makes a file stale: offered an engine hook that hands each
  word read, comments skipped, to the build so a comment edit in memory.f
  would rebuild nothing, Joel answered "why are you inventing hooks instead of
  a system that tracks file dependencies, like make does? if everything truly
  depends on memory, which i doubt it is, then we rebuild everything, comment
  or no comment!" A compiled file is stale when a file it depends on changed,
  whole file, as in make; a comment edit counts. The compiler requires
  memory.f (src/compiler/ir/context.f:9), so in an engine build a memory.f
  edit rebuilds every file the tree's compiler compiles.
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
- 2026-10-31, the real-Chrome browser display test: "this is a stupid test,
  prove me wrong! why the fuck would you test if chrome actually works?" Its
  other checks cover Habu's own browser code (released GPU devices and
  listeners, revoked image URLs, collected modules, SVG isolation), so Joel
  chose "Only when asked": Chrome runs only on an explicit `--chrome`, never
  because Chrome is installed, and the run leaves no process, clone or
  profile behind.
- 2026-10-31, fixtures Finder can delete: "find and fix this degenerate
  test", "first it was creating filenames too long for finder. now it's too
  deep for finder. we don't need a test that breaks finder, prove me wrong!
  same thing could be accomplished much simpler". No test creates a tree or
  path Finder cannot delete, and each proves its behaviour with a small
  fixture. Asked whether ATOMIC-PATH-CAP, which needs a real canonical chain
  near PATH-CAP, should stay, Joel asked "does this test carry weight or is
  it testing the obvious?"; shown it is the only test that catches an atomic
  writer refusing a target within 27 bytes of PATH-CAP, he chose "Keep it",
  then "fix the test, though". Asked the same of the fs-mutate-test fixture
  for a TMPDIR with no canonical path, whose realpath fails on a directory
  stat accepts only past PATH_MAX (search-only modes do not do it, and a
  chain of links long enough fails stat too), Joel asked "does this test
  carry its weight? how can it be fixed?", then chose "Delete test, keep
  refusal": TMPDIR-CHECK keeps the refusal, untested.

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
