# Lessons

What stands now in Habu: each decision of Joel's with its date and his words,
and each lesson learned with its evidence. A reversal edits or removes its
entry.

## Decisions

### Language and checker

- 2026-11-01, public words: after leads asked Joel to approve guard words
  where the fix was reading a request body on demand, he said to "ask
  whenever they want to create new forth (habu) words" and "it should be a
  habu rule"; on 2026-11-02 he narrowed it: "approval for internal habu words
  not required, only public ones, which is, again, public api". A new
  package's public words are included; the card's section 1 states it.
- 2026-11-02, the T0 x86-64 stack's public words: shown that
  T0:COMPILE-FIXED, NCOMP-DISPATCH:FIXED-DEFER-KIND and RUNTIME-RUNNER:JIT?
  follow the older names rather than card section 1, Joel asked "what do they
  do? why do they not match our naming convention? are they best for long
  term?" He chose "FIXED-COMP-CELL": a cell beside COMP-CELL through which the
  Habu loop calls tier 0's compiler for constant, variable and defer bodies,
  so T0's word goes private and the Habu loop stops loading T0. Then "fine,
  approved all, keep going": JIT? goes for the tests' existing
  HB-TARGET-LINUX-X86-64? switch, and the fixed-body kinds become one typed
  enum in a lane of their own. Offered FIXED-KIND:VALUE for its values, Joel
  answered that "fixed-kind as a package name violates naming conventions"
  and asked for fixed body and fixed kind in plain English. Shown that a fixed
  body is the machine code the compiler stamps from a pattern for a word with
  no source text (a constant pushes its number, create and variable push
  their data address, defer jumps through its cell, CAST: passes the value
  through) and that the kind says which pattern, he chose "COMP:body": values
  COMP-BODY:compiled, value, address, defer and cast, and COMP:BODY>N for the
  number. Shown that the enum's review found "is this kind X?" helpers
  written privately in several compiler packages, one of them word for word
  in two, because the enum had no equality, Joel chose "Add DERIVE eq
  (Recommended)": COMP-BODY:EQ compares two kinds and the helpers go.
- 2026-11-02, the checker's switch: asked about the seal lane's
  LOWER-CERT-HOOK:AT-CURSOR, Joel wrote "lower-cert-hook is a crap name, it
  does not explain what it does" and "why does it need to be a hook instead
  of a function? ... is this the best for long term?" Shown that the
  machine-code compiler calls whatever sits in one cell, that `set-check`,
  `check@` and `set-preflight` make that cell a public switch, and that
  `0 set-check` before `: BAD ( -- n ) dup ;` lets BAD compile and run (exit
  70) where it is otherwise refused, Joel chose "Remove the switch": after the
  kernel loads every definition is checked and no word turns checking off; the
  typed interpret loop calls the checker as an ordinary word; only the kernel
  compiles unchecked. `set-check`, `check@`, `set-preflight` and
  LOWER-CERT-HOOK go. Shown the replacement names, Joel chose
  "CHECK:DEFINITION" for the word the compiler calls on every definition's
  source, adding "we already have COMP and XCOMP, so CHECK fits": the
  checker's package is CHECK. `discard-check` and `set-top-check`, a second
  switch, go with the others. Offered a slot in the checker's owner record
  through which the checker would tell engine-origin about each definition it
  passed, as it already does for casts and exports, Joel answered "current
  approach smells. why not set a flag on the definition? the approach for
  exports and cast should be reviewed too". Shown a design in which the engine
  writes one cell on every dictionary record, the number of the file that
  defined it and the offset of its name, replacing the origin journal, its
  hooks, the checker's export and cast notices and the cache's origin section,
  Joel chose "Approve SOURCE-FILE, XREF:SOURCE (Recommended)":
  `SOURCE-FILE:PATH$ ( n -- ptr u8 n )` and `SOURCE-FILE:FIND ( ptr u8 n -- n
  bool )` over the loader's table of the files it opened, and `XREF:SOURCE (
  ptr n -- n n )`, a record's file number and name offset, 0 0 when
  unlocated; ENGINE-ORIGIN keeps TARGET, SYMBOL-AT and EACH-IN, and its other
  words, ENGINE-ORIGIN-EARLY, NATIVE-BODY and the export notice go. Told
  that the loader's frame code (SOURCE-ROOT) and the snapshot step can reach
  the table only through public words, Joel wrote "intern and persist
  approved for source-file": `SOURCE-FILE:INTERN ( ptr u8 n -- n )` and
  `SOURCE-FILE:PERSIST ( -- )`, and asked "why do we have both source-file
  and source-root?". Shown that src/core/include.f splits loading source into
  six packages, so each seam forces public words, he asked "shouldn't all
  these be in the SOURCE package?", then "either SOURCE or LOADER". The lead
  chose SOURCE: the subject, not the machine, and src/lib/source.f is already
  package SOURCE for reading source text. The loader's packages and the
  table become one package SOURCE; words only the loader calls go private,
  INTERN and PERSIST included. Asked
  where the error-place words go, he asked "what is TOP-LINE and why do we
  need it? I'd think we just need LINE (or LINE-AT? worse?) and COLUMN";
  shown that the top-level site is only a byte's line taken at the checker's
  token, he chose "SRCLOC:LINE, COLUMN, PATH$": `SRCLOC:LINE` and
  `SRCLOC:COLUMN ( ptr u8 -- n bool )` place any byte of the open file,
  `SRCLOC:PATH$ ( -- ptr u8 n )` names it, and top-level errors print a column
  too once tools/check.f's run file stops shifting columns.
- 2026-11-03, the source loader's hooks and names (design-source.md rev 6):
  shown that the build cache fills SOURCE-INPUT to see what each load reads
  and SOURCE-UNIT to swap a file load for its stored object, Joel chose
  "Remove both hooks (Recommended)": the loader logs what it reads between
  `SOURCE:RECORD-INPUTS` and `SOURCE:;RECORD-INPUTS`, the cache passes its
  file runner to `SOURCE:LOAD-WITH` for one build, and the interpreter defer
  stays private with `SOURCE:USE-INTERPRETER` public for the two tests that
  run x86-64's Habu loop over file loads on arm64. He then asked "what is
  SOURCE-UNIT and why do you need it in addition to SOURCE?"; told it is one
  of the six loader packages and is deleted, and shown the names that clash
  once they merge, he chose "Renames as listed (Recommended)": the registry's
  public words carry REQUIRE-, the input log's INPUT- and discovery's events
  EVENT- (REQUIRE-COUNT, EVENT-ROOT@; privately FRAME-SOURCE, REQUIRE-PERSIST,
  FILES-PERSIST). He chose "Keep LOAD and kinds (Recommended)": `SOURCE:LOAD (
  ptr u8 n SOURCE:load -- )` is the one body of the global `included`,
  `required`, `script-required` and `provided`, with kinds SOURCE-LOAD:always,
  once, script and provide. Shown the rest of the public list as one question,
  he answered "don't do this, interview one by one instead?": a public list
  goes to him one word at a time. Shown INCLUDE-EVALUATE (evaluate-closed
  plus undoing a failed text's requires), he answered "keep it but name it
  EVAL instead of EVALUATE": `SOURCE:EVAL ( ptr u8 n -- )`. Asked "why do you
  need eval-range? where and how is it used? and when", and shown that
  INCLUDE-EVALUATE-RANGE and OUTER:INTERPRET-RANGE have only test callers, he
  chose "Drop it; tests use loads (Recommended)": no SOURCE:EVAL-RANGE,
  INCLUDE-EVALUATE-RANGE, OUTER:INTERPRET and INTERPRET-RANGE go, and the tests
  run the Habu loop over file loads through SOURCE:USE-INTERPRETER. Shown
  SOURCE:E-RANGE, the code thrown when the build cache's runner asks for a
  byte range outside its file or after the load ended (today -2110, also
  E-FS-TIMEOUT's number), he chose "Public, own number (Recommended)". Asked
  "why do you need parent action?" and "why do you need to discriminate
  instead of recording a stream of actions for the loader to perform?", and
  shown that the cache finds each load by watching the tokens a file's top
  level runs (LOADER-CALL?, ACTION$, PARENT-ACTION$) so it can cut the file's
  object there, he chose "Loader records loads (Recommended)": the loader
  writes each load it performs into its log, and the object's loads come from
  those rows; LOADER-CALL?, ACTION$ and PARENT-ACTION$ go.
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
- 2026-11-01, long model calls: Tender lost documents when MODEL's total
  call bound ran out at about 300 s. Offered OpenAI's background mode (start
  the response, poll its status every 2 s, no total bound, a dead call shown
  by a failed status or a stalled connection), at the cost that OpenAI keeps
  the response on disk about 10 minutes even with store=false, Joel chose
  "Allow 10-min hold" over one long held request. He then approved removing
  MODEL:TIMEOUT!, DEFAULT-MS and E-DEADLINE, renaming FINISH-REASON$ to
  INCOMPLETE-REASON$, and CHAT speaking only the Responses protocol (Ollama
  stays for embeddings; its chat is built when a client needs it). Asked
  whether usage counts take the protocol's input_tokens and output_tokens
  names, Joel chose "Rename, one cutover": record keys, MODEL's words and
  the Tender and Zen readers together. Asked how a test should see a call
  waiting between polls, Joel answered "is polling the best way for the long
  term? are there better alternatives, e.g. websockets?" Shown that OpenAI
  streams a background response's events over the connection that starts it
  and resumes the stream after a drop, that its WebSocket mode is for agent
  loops of short turns and takes no background mode, and that webhooks need a
  public URL, Joel chose "Stream it": no polling.
- 2026-11-01, a live check of MODEL: asked how the one live OpenAI run
  should happen before MODEL's streamed background responses land, given this
  session may not read the key from the keychain, Joel chose "You run it
  (Recommended)": the lead writes a probe file. On 2026-11-02, asked to run
  it, Joel answered "you run it": the lead ran it, and gpt-6-luna answered a
  streamed background CHAT and replayed a stream from `starting_after=1`
  (events 2 to 68, status 200). Under Joel's relayed "use your best judgement" the
  lead also chose, after the lane review: a call stays alive while its
  connections carry bytes, a keepalive included, so a resume that heard only
  keepalives resumes again; and a call that fails after its response id is
  known cancels that response at OpenAI, logging a failed cancel.
- 2026-11-03, a model server's replies for tests (relayed by phil from Zen):
  asked to approve the public changes for moving Zen's model calls onto
  Habu's MODEL client, among them TEST-MODEL:COMPLETED!, INCOMPLETE! and
  REFUSAL! ( HTTP:response ptr u8 n -- ), which write the event streams
  MODEL:CHAT reads so a test drives it against a local route, Joel chose
  "Approve all (Recommended)": "Build the switch now as listed; Tender gets
  the change id."
- 2026-11-01, indented JSON (relayed by stuart): shown that Tender lays out
  every indented report itself, about 600 line-break and indent calls each
  passing its depth, against a writer that tracks nesting and indents itself
  from one width setting, Joel chose "Writer indents itself (Recommended)",
  then wrote "also, i approve your json words": JSON:WRITE-INDENT! (spaces
  per level, 0 for compact), JSON:WRITE-STRING-START, -PART and -END, and a
  writer holding its indent and depth. On the packages (relayed by stuart):
  "the point is that the package is JSON, not JSON-WRITE. and there's no JR
  either. it's JSON:WRITE-... and JSON:READ-.... should be explained in the
  forth style guide", and "habu library needs to be brushed up, yes. not
  necessarily everything right now. json would be sufficient for now". JSON
  reading and writing are one package, JSON, and card §1 states the rule.
- 2026-11-01, a client token's lifetime (relayed by kevin from Tender): Joel
  chose "Token word gives lifetime (Recommended)": "BEARER:TOKEN also returns
  expires_in; tenderd renews before expiry. One changed Habu word."
- 2026-11-01, request bodies (relayed by phil from Zen): Joel chose "Read on
  demand (Recommended)": "BODY$ reads on first call; an unread body closes the
  connection." A server can refuse a request from its headers alone, and
  E-HTTP-BODY-READ names a failed read.
- 2026-11-01, streaming a child's output: the CMD:RUN-STREAM lane handed each
  stdout line to a callback, and its review found that a callback starting
  another run on the same task corrupts the outer run, and that the idle
  window counts the callback's own time as the child's silence. Asked to
  refuse a nested run, Joel asked "why should it pass each stdout line to a
  callback? is this good design? best for the long term? how is this done
  usually?" Shown that Python, Go, Rust and C hand the caller the child's
  stdout to read (callbacks appear in event-loop runtimes such as Node), that
  a caller reading lines keeps its own time out of the window and may run
  other commands between reads once each run holds its own process state,
  and that the linear CMD:command makes the checker require the run's end,
  Joel chose "Pull: caller reads lines (Recommended)".
- 2026-10-31, typed data: shown that the image save check reads an untyped
  `c,` table's bytes as possible addresses, so a build fails at random by
  where memory was placed, and that a fix recording `c,` runs covers only
  part of it, Joel answered "regardless of how we build this chunk of data,
  why are we not typing it? that would fix all the issues and avoid having to
  record any runs. prove me wrong!" Whether data can hold an address is
  decided by its declared type, not by how it was built or by its value.
  Asked what a PTR-VARIABLE should hold once a saved image starts again, Joel
  chose "Cleared to null" over merging it with PERSISTED-PTR-VARIABLE: the
  scratch pointer forms are written as null into the image, and only the
  PERSISTED- forms carry an address across a save. Asked whether the old byte
  scan's code, public CELL-BODY:DECLARE included, goes once the save checks
  by type, Joel chose "Remove all of it". Asked whether each scratch pointer
  cell should be marked so the save writes it as null, or scratch storage
  should live in a separate area the save never writes, like C's .bss (a
  second storage area across the engine, compiler and build cache), Joel
  answered "i want you to mark each cell AND record this decision in case we
  want to go back to option 2 where scratch storage lives apart", and named
  the marking word `scratch-cell-mark`, beside ptr-cell-mark. Shown that the
  only scratch cells holding an address at the engine save are the opcode-name
  tables of src/compiler/native/a64ir.f and hir.f, which mark their cells by
  hand, Joel chose "Add the word": `PERSISTED-PTR-U8-TABLE ( n -- )`, the
  persisted twin of PTR-U8-TABLE, declares a table whose cells keep their
  addresses across a save.

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
  src/project-ops.f:176-182), so TASK-LOCAL is dropped. That cast was then
  refused (E-CAST-OWNER), which raised the next entry.
- 2026-10-31, casts naming another package's type: asked about the cast check
  that refused Maki's per-task pointer, Joel answered "fix the cause not the
  symptom! what the hell is the anti-forgery check? prove that we need it! why
  in the world can you not hold another package's cursor?" On 2026-11-01, via
  phil: "use your best judgement. pick what's best for the long term. tell
  everyone the same". Fable and Astra each showed the check guards nothing: a
  package rewrites a real JR:cursor or NEWTYPE cell through an admitted
  reverse cast or a raw pointer and c! (/tmp/carl-cast/p1.f, p4.f), and it
  refused only honest code (Maki, Zen). The lead chose to drop the question: a
  cast may name another package's type, E-CAST-MINT stays, and E-CAST-OWNER
  names only arena heads.
- 2026-10-31, cast errors: asked whether lane casterr may make
  CAST-ERROR:START, TOKEN!, FAMILY!, RAISE and SKIP and
  LOWER-CERT-HOOK:LINE-AT public, because checker words outside those
  packages call them, until nested packages make CAST-ERROR a part of CHECKER
  and hide them, Joel chose "Approve, DIAG-XT private": the print hook, used
  only inside CAST-ERROR, stays private. START later became DECLARE, which
  runs the declaration and clears the record when it ends, because a record
  left holding the last declaration made cold and warm builds' images differ.
- 2026-10-31, error places: asked whether SRCLOC:FILE-BUFFER? ( -- bool ),
  true while the text being read is the open file's own bytes, may be public
  so the Habu loop's and the native compiler's error-place writers share one
  rule (the file's line for the file's text, the file alone for an evaluated
  string), Joel chose "Add SRCLOC:FILE-BUFFER?".
- 2026-10-31, throw codes: shown that lane sealed's public
  VERIFY-DIAGNOSTICS:THROW-CAUSE$ (a code's short message) repeats
  THROW-CODE$'s list of codes, so each new code is written in both, Joel
  chose "THROW-CODE$ gives message": one list answers a code's name, repair
  class, suggestion and message, and THROW-CAUSE$ goes.
- 2026-10-31, the failed token's text: lane dtail had changed public
  CHECKER-OWNER:REFUSED-SPAN to also return the text of the token the
  checker failed on, which an older hb building the tree would misread.
  Offered a new word reading an appended owner field instead, Joel chose
  "Add ERROR-TOKEN": `CHECKER-OWNER:ERROR-TOKEN ( -- ptr u8 n )`, with
  REFUSED-SPAN keeping its shape.
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
  target. Joel chose "One level" of nesting: a part holds no parts. The lead
  added a refusal the design did not list: `package` opens a part only in the
  file or evaluated string its package is open in, and elsewhere stays
  `package at <loc>`, rc 75, as `;using` refuses to close a using opened
  outside its buffer. Without it a library file required inside package APP
  that opens its own `package LIBF` became the part APP:LIBF, the require
  registry then skipped it at top level, and `LIBF:FW` was E-UNDEFINED.
- 2026-10-31, a part's head resolver: inside package A, `: B:Q … ;` defines
  into part A:B, but the native compiler's name check found only top-level
  packages and the Habu loop's resolver was private to OUTER. Asked whether
  xref.f's XREF package may get a public `XREF:HEAD-ROW ( ptr u8 n -- ptr n )`,
  the row a qualifier's head names (the open package's part first, then a
  top-level package), called by the Habu loop and the compiler, Joel chose
  "Add XREF:HEAD-ROW". Shown that the lane wrote a second copy of the rule in
  the checker under top-level CK- names, and that the checker loads before
  the Habu loop and the compiler, Joel chose one word there,
  `CHECKER-RESOLVE:HEAD-ROW`, for all three: XREF:HEAD-ROW and the CK- copies
  go, and the helpers are private.
- 2026-10-31, a public SSE word: asked whether SSE:FEED's callback context
  may change from `ptr a` to `a`, so MCP passes its client record without a
  cast, Joel chose "Context becomes `a`".
- 2026-10-31, comparing signed builds: codesign writes the signing time into
  every signature, even with `--timestamp=none`, so two team-signed builds of
  the same program differ. Asked to name the one shared word that compares
  two images, skipping only the signature block that holds the time, Joel
  chose "CODESIGN:SAME-IMAGE?" over a global SIGNED-SAME? in sign2.f.
- 2026-11-01, a missing signing certificate: every macOS build signs with
  the team identity; asked what a build does when that certificate is
  missing, Joel chose "Approve (Recommended)": "Missing certificate stops the
  build with a named error." The build stops with CODESIGN:E-IDENTITY.
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
  WAKE-LOCK and HOLD. On 2026-11-01, asked to add POWER:E-PLATFORM (no
  keep-awake support on this system) and POWER:E-ASSERTION (macOS refused to
  hold or release it), Joel chose "Add both (Recommended)".
- 2026-11-02, Maki's ChatGPT sidebar view (lane H2): shown the list for a
  page that runs a Habu Wasm module as an MCP App, Joel chose "Approve:
  MCP-APP, rpc (Recommended)" over BROWSER-APP and bridge: `MCP-APP:PAGE+ (
  ptr u8 n ptr u8 n ptr a -- )` appends the page for a module and title to
  the caller's BUF; turn records 10 `rpc` (a JSON-RPC request to the host,
  answered by event 3), 11 `after`, 12 `read`; events 14 `notified`, 15
  `tick`, 16 `file`, 17 `chunk`; mount options `Module` and `rpc`, viewport
  `notify` and `file`, and host-cli's `--notify`, `--tick` and `--file`.
  host-cli's `--answer` and `--fail` already answer HTTP requests, so the
  lead named the rpc steps `--answer-rpc ID FILE` and `--fail-rpc ID`.
- 2026-11-02, sizing Maki's job runners: OS-CPU:PERFORMANCE-CORES and
  OS-MEMORY:TOTAL, approved on 2026-10-31 so Maki could compute its runner
  count from the host, needed a shared reader of Linux container (cgroup)
  limits. Asked to approve a CGROUP package for it, Joel answered "do not
  overcomplicate things. discard the whole thing and just make it a config
  file setting. we do have a config file, do we?" Told that Maki reads its
  settings from environment variables, he chose "New Maki config file": the
  runner count is a setting in a new Maki config file that takes all of
  Maki's settings, and Habu gets no host-size words.
- 2026-11-01, errors the checker reports itself: under Joel's word relayed by
  phil, "use your best judgement. pick what's best for the long term. tell
  everyone the same", the lead made two choices. E-UNSAFE's repair class is
  `remove_unsafe_word`, not `trusted_boundary_required`: a checked body cannot
  name an unsafe word at all, and TRUSTED: is being retired. One global checker
  word, `REPORTED-ERROR? ( n -- bool )`, lists every code the checker renders
  before it throws, for CAST-ERROR:SKIP and the check tools alike, as
  REPORT-UNCHECKABLE became one checker word on 2026-10-31.
- 2026-11-01, a quotation's fresh region: under the same relayed word, the
  lead chose the rule the checker implements. Quotations are xts, not
  closures, so a fresh name a quotation's own inputs do not carry is minted by
  each execution of the outermost quotation holding it at the top of its
  outputs or in a callback input's rows, and shared by the quotations that
  execution returns or hands on; a direct call, a tick and an inferred effect
  read it alike. Its rows are test/rigid-region-suite.f section (32); no
  checked body implements a name renewed at two levels.

- 2026-11-01, sealing a package: Maki needs a package closed so later
  source cannot reopen it. Offered a new word, Joel answered "we already have
  package sealing, prove me wrong!". Measured: `private get-current
  prot-wid-add public get-current prot-wid-add` before `;package` makes a
  later reopen fail with E-SEALED-PACKAGE, exit 84, at the run and under
  tools/check.f, but check's `--verify-only` scan passes it (exit 0). Joel:
  "the cause is the checker, no? why not fix the checker instead? what's best
  for long term?" Shown that the checker can follow a seal written as a
  wordlist number only by guessing which wordlist the number names, Joel chose
  "One word, seal-package": `seal-package ( -- )`, written inside the
  package, which the run executes and the checker's scan reads by name; it
  replaces the idiom in the engine's files. Application images otherwise stay
  reopenable.

### Compiler and builds

- 2026-11-02, compile state: Joel wrote "you would benefit from using state
  machines, prove me wrong". The lead could not: one T0 compile's lifecycle
  is held in flags across five owners (M-OPEN, the seam's CUR and PLACED-AT,
  NEMIT's rows, NCBSTUB's ARMED, NSHADOW's OPENED and PENDING), its legal
  order lives in call sequences and comments, and a review found one guard
  unreachable while other wrong orders pass. Each owner's lifecycle becomes
  one typed state, written only by transition words that refuse a wrong
  state.
- 2026-11-03, the compiler lease: shown a design in which T0's compiles take
  tier 1's compiler lease (NLEASE) so NDEFINITION's M-OPEN flag could go,
  Joel asked "why in the world do you need to take a lease? this smells! are
  you doing multi-threaded compiles?" They are not: only the main thread
  compiles (src/lib/task.f's contract), and the lease's other check, a
  compile started inside another, is the one M-OPEN already makes. T0 takes
  no lease and keeps M-OPEN. Told the lease would stay tier 1's until tier 1
  goes, Joel asked "if you are taking no lease then why are you modifying
  calls to NLEASE? why have NLEASE at all?" The lead read that as removing
  NLEASE from tier 1 and designed it; shown that tier 1 cannot retire yet (it
  compiles every Wasm module, stripped images and saved app images, and is
  kept for experiments) and that 63 of NLEASE's 65 uses sit in tier-1-only
  files, Joel chose "With tier 1 (Recommended)": NLEASE goes when tier 1 is
  deleted. He added "the reason i pointed out nlease is because i saw you
  modifying nlease or using nlease in some other code".
- 2026-11-03, editing tier 1: Joel asked "why do we not stop modifying T1 and
  focus on T0? why do we keep modifying T1?" Told that T0 calls tier 1's
  packages for everything after its own compiling (NDEFINITION, NEMIT, NPUB,
  NSHADOW, NDICT, the callback stub), so T0 changes there move tier 1's calls,
  he asked "what about copying T1 code to T0?" and wrote "i want to share
  frontend code, not build hooks and ties into code we are about to retire!"
  T0 shares only front-end code with tier 1. What T0 needs from tier 1's
  packages is copied into T0's own package and owned there; tier 1 keeps its
  copy, unedited, until it is deleted. Tier 1 is edited only to fix a defect
  on a path it still owns (the next entry).
- 2026-11-03, tier 1 bugs before Wasm moves to T0: asked to approve
  NELAB:FIXED building a CAST: word's body, so tier 1 stops refusing a cast
  with a quotation input (test/wasm/dynamic.f), Joel asked "what is NELAB? and
  why do you need to cast in the compiler?", then "do we still have
  intermediate code with t0? why bother with nelab if we do not?" Shown that
  T0 compiles the checker's tape straight to machine code, that NELAB and the
  IR are tier 1's, and that tier 1 still compiles every Wasm module
  (tools/wasm-build.f), the stripped-image driver (tools/aot-build.f) and the
  experiments kept on 2026-10-30, he chose "Fix in tier 1 now (Recommended)":
  a tier-1 defect on a path tier 1 still owns is fixed there, not parked until
  T0 takes the path over.
- 2026-11-03, T0's callback request and shadow cell (t0copy design rev 4):
  asked one item at a time, Joel named the word with which T0 reads a callback
  request's slot and thunk `NDEFINITION:CALLBACK-REQUEST@ ( -- n n )`, and
  chose "CALLBACK-ENTRY! (Recommended)" for the word that stores the placed
  stub's entry ( n -- ). Shown that CALLBACK-STUB@ would end the request and
  answer 0 when no stub was placed, with FFI-CB's EMIT refusing 0 with
  E-NCOMP-STATE, he answered "we don't have native compilation. do we? why are
  you carrying NCOMP over? rename it to COMP or similar". Told that T0
  compiles natively, that NCOMP is tier 1's old package name, and that
  E-NCOMP-STATE heads 8 global E-NCOMP- codes with 78 uses in 28 files, mostly
  front-end codes that outlive tier 1, he chose "E-NCOMP- family → E-COMP-
  (Recommended)": the 8 codes keep their numbers and every use takes the new
  name; NCOMP-DISPATCH and NDEFINITION keep theirs. He then chose "Approve new
  contract (Recommended)" for CALLBACK-STUB@, refusing with E-COMP-STATE. Asked
  to name T0's cell for recording a fixed body's target copy, he wrote "what is
  a fixed cell? do you nave non-fixed cells? COMP-SHADOW-CELL sounds good to
  me but i have questions about fixed"; told that fixed names the body (a
  constant's, variable's, create's, defer's or CAST:'s, stamped from a
  pattern) and that T0 records a colon body's copy inside its own compile, so
  this is T0's only shadow cell, he chose "COMP-SHADOW-CELL (Recommended)":
  `NCOMP-DISPATCH:COMP-SHADOW-CELL`.
- 2026-11-02, T0 compiling for a second machine (dual emission): shown a
  proposal that added an unplaced seal word beside T0's placing SEAL and made
  tier 1's SHADOW-TAKE public in NDEFINITION, Joel answered "this smells. is
  this the best for long term? review and justify." The design he then
  approved seals unplaced on every pass and places only on the host pass,
  before sealing; T0 keeps its own private SHADOW-TAKE. Shown it as
  T0-EMIT:PLACE, he answered "why T0-EMIT:PLACE and not COMP:EMIT-PLACE? you
  are violating naming rules, again. otherwise, i approve", then chose
  "Approve the move": T0's five files become the COMP package, the emitter
  seam's words named EMIT-*, the arm64 and x86-64 emitters and the label layer
  its parts ARM64, X86-64 and LABEL; COMP:INSTALL replaces T0:INSTALL and every
  other public word of those packages goes private.
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
- 2026-10-31, files built into hb: asked to let the hb build record a
  fingerprint for each file built into it, so an app build knows which of
  them changed, Joel asked "are you keeping dependencies BETWEEN files,
  though? because you could do with a simple timestamp, no hashes needed".
  Told that each stored file already lists the files it read, checked by
  change time and size first and re-hashed only when those moved, and that
  times recorded in hb would rebuild every app file after a same-content hb
  rebuild and make hb's bytes vary by checkout, Joel chose "Hash, word
  FILE-HASH": the hb build records each built-in file's path and SHA-256, and
  `SOURCE-PRODUCER:FILE-HASH` looks one up. Joel added: "fine, use hashes
  everywhere if that's best for long term and keep track of habu files too,
  dependencies, etc." A built-in file's recorded hash covers the files it
  required, so a change reaches every file that depends on it, as in make.
- 2026-11-03, where timings run: told that build timings waited for a quiet
  machine while this Mac ran at load 26 on 12 cores, Joel wrote "spark is
  doing nothing". A before-and-after timing runs on an idle host (spark, Linux
  arm64), both sides on the same machine and engine, not on the shared Mac.
- 2026-10-27, evidence: "so make it happen in your branch and test it out!"
  and "you should be focusing on this, prove me wrong!" When a decision lacks
  evidence because a capability does not exist yet, build it in a branch and
  measure.

### Processes

- 2026-10-28, fork: shown that SUBJECT:RUN, the gate pool, eval and the
  property-test shards fork the loaded process and keep running Habu, which
  needs a registry resetting task state in the child, Joel asked "did unix
  not solve forking ages ago? why are we trying to invent stuff here?" and
  "why are we mixing forks with tasks?", then chose "fork to exec. also,
  rename the subject library and package to something more appropriate!"
  Habu forks only to exec: a child that runs Habu is a fresh hb. Tasks are
  OS threads, and POSIX supports little in a threaded process's child before
  exec; on macOS a fork-without-exec child crashed in curl_global_init and
  objc once the parent had a live task or had used Security or curl (dot
  habu-fork-only-to-59d3557e).

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
- `jj squash --from X --into Y` opens an editor to merge the two descriptions
  when both have one, and an agent's call then waits in vim until killed (a
  pkg1d fold squash waited 12 minutes on 2026-10-31). Pass `-u` to keep Y's
  description.
- A baseline engine runs the tree's source-loaded tools (tools/check.f and
  what it loads), so swapping engines cannot show a tool change failing
  before: casterr fold 4's worker ran diag-position on the old engine with the
  new tools/check-all-errors-core.f, saw the same 7 failures on both, and
  called them its own setup; they were the change. Show "fails before" by
  reverting the tool source in a scratch copy of the tree, run through an
  entry file at the copy's root: `--load` of a file below the root resolves
  `require tools/…` against the running hb's tree (card section 7): a
  before run that loaded a scratch copy's tools/lint/clobber-lint-test.f got
  the workspace's lint, as a word only the new lint defines showed.
