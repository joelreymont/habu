# Decisions

What stands now in Habu: each of Joel's decisions with its date, in his words,
recorded once it is settled. A reversal edits or removes its entry. Lessons
learned live in the docs for their subject.

## Language and checker

- 2026-11-04, scope memory (relayed by stuart from Tender): shown that every
  MEM:WITH-BYTES scope maps and unmaps its memory (1,000,000 empty scopes
  4.69 s, 2.04 s of it in the kernel) and a design that keeps small scopes in
  chunks each task reuses, large ones still mapped, Joel chose "Approve
  (Recommended)": "Pooled scopes; MEM:RELEASE-SCOPES frees a task's chunks at
  join or kill." `MEM:RELEASE-SCOPES ( ptr u8 -- )` takes a task's data
  region. Asked whether the task's scope cell MEM-ABI:HEAD-OFF may point at
  the task's first reused chunk instead of the innermost scope, with the same
  name and place, Joel chose "Approve (Recommended)": "Keep the name
  HEAD-OFF; the cell now points at the first chunk."
- 2026-11-03, TASK's waits (asked from Tender): "what do you need STOP-FOR?
  what does it do? if it stops for some milliseconds then should it not be
  WAIT-MS, for example?"; then "i think anything that takes a semaphore should
  say so, e.g. TASK:WAIT-SEM-MS, etc."; then chose each "(Recommended)"
  option: STOP-FOR becomes TASK:WAIT-MS; STOP becomes TASK:WAIT, the task's
  own wait beside WAIT-MS and WAKE; the semaphore's WAIT, SIGNAL and TRY-WAIT
  become WAIT-SEM, SIGNAL-SEM and TRY-WAIT-SEM, with no WAIT-SEM-MS; GET and
  RELEASE, which take a lock, keep their names (paraphrase).
- 2026-11-03, names that read as numbers: asked where the refusal of a
  definition named like a number goes, Joel answered "what's the problem? use
  human english. : 42 is invalid, why are we deliberating?": `: 42` is
  invalid for every definer, refused where the engine already refuses `: I`.
  Offered E-VS-WORD-NAME for the code the source scan stops with on such a
  name, Joel asked "what is VS and why do you need it? why not just
  E-BAD-NAME?": the code is E-BAD-NAME.
- 2026-11-03, the language server's checking package: shown that
  tools/check-verify-core.f's package shares the name CHECK with the checker's,
  Joel chose "LSP-CHECK (Recommended)": it joins tools/lsp-check.f's package.
- 2026-11-03, the property test: asked to remove PROP-CORE:RC!, a cell that
  carried a compile's result code out of a text that turned the checker off,
  Joel chose "Remove it (Recommended)". He then wrote "the file should be
  prop-test.f and there should be a single PROP-TEST package, prove me
  wrong!" and asked "why the hell do you need hooks for prop testing?": the
  test becomes test/prop-test.f with one package PROP-TEST and no hook.
- 2026-11-03, generated family words: a public `STRUCTURE` declared in a
  package part failed to load, because a public family's generated words lived
  in a namespace built from the package's name (`PA:PB-ZQVW:MAKE`). Joel said
  "swiftforth handles public structures in packages just fine. perhaps, the
  approach should be to not make packages of structures but use
  STRUCTURE-NAME- prefixes instead?" and asked "what is best for long term?"
  Shown that SwiftForth's structure words are plain words in the current
  wordlist, he answered "do it": every word a `STRUCTURE` or `ENUM` generates
  is defined in the declaring package, and no family gets a namespace of its
  own. Shown `PCB:POINT-MAKE`, Joel said "hang on, it's not PCB:POINT-MAKE
  it's PCB:MAKE-POINT", then "unless is POINT is a structure then we need to
  change the Habu convention so that the constructore is named MAKE-<STRUCT
  NAME>", and chose "Only MAKE/UNMAKE first": `PCB:MAKE-POINT` and
  `PCB:UNMAKE-POINT`; every other generated word keeps the family first
  (`PCB:POINT-EQ`, `PCB:POINT-X@`, `OPTION-SOME`). Offered a checked rename
  tool, he chose "Rename by hand". Asked about a public `OPAQUE` family's
  words, he said "i said no such thing! don't touch opaque": `OPAQUE` stays
  as it is. Shown that then no public `STRUCTURE` or `ENUM` gets a namespace
  of its own (`OPTION:SOME` becomes `OPTION-SOME`), he chose "Delete them
  all": everything that only builds or reads those namespaces goes, the
  public error E-PROTECTION-CAP included. The private path's protections and
  E-CTOR-PROTECTED, which now guards public families too, stay; package
  sealing is the entry "sealing a package".
- 2026-11-03, how the compiler reaches the checker: shown that the compiler
  can call the word named CHECK:DEFINITION whenever the dictionary has it,
  with no hidden cell and no install step, so that only the kernel, before
  the checker loads, compiles unchecked, Joel chose "Find by name
  (Recommended)".
- 2026-11-03, removing a word of a sealed package: asked to approve refusing
  it once hb has started, Joel asked "why would you ever remove a word from a
  sealed package? once a package is sealed you have no access to its
  internals, prove me wrong". Shown that nothing after startup needs it, he
  chose "Refuse every removal (Recommended)": after startup, removing a word
  of a sealed package is an error on every removal path.
- 2026-11-03, the checker's other words found by name: offered TOP-ROW:TOKEN
  for the check of each word run outside a definition, Joel asked "the fuck
  is top-row? do you not question the package naming? why do you need
  TOP-ROW?" and "why does the engine need to find it? if you mean the
  compiler then why does it not live in the COMP package?" Shown that the
  caller is the top-level interpreter, built before any checker exists, and
  that a program's top level runs and is checked in the building hb, he
  chose "CHECK:TOP-LEVEL (Recommended)": package TOP-ROW goes and the check
  joins CHECK beside CHECK:DEFINITION. For the check the compiler runs before
  an immediate word the checker has no model for, he chose "CHECK:IMMEDIATE
  (Recommended)".
- 2026-11-03, a public and a private word with one name: told that a package
  may hold both (forth.md's forwarder rule), Joel said "it makes no sense to
  have public and private words with the same name since they will clash
  within the same package. how is this even possible today?!" He chose
  "Refuse all twins", and again after forwarders were explained with
  src/lib/task.f's HALT (private :1645, public forwarder :2329).
- 2026-11-03, name lookup: told that the engine's dictionary index and the
  checker's symbol table each hash a name's bytes while INTERN maps a string
  to one address, Joel wrote "yes, the other 2 hash tables should use the
  string address instead of hashing again". Told that both tables could then
  become one, he answered "yes if they both point to the same type of data":
  both key on the interned name's address, and they merge where both map a
  name to the same kind of record.
- 2026-11-05, where the checker's facts live: on whether the checker's facts
  should piggyback on the wordlist, Joel wrote "you don't need to send of a
  designer to answer the question of whether it should piggyback on the
  wordlist, it's something you can decide here and now!" The lead decided
  that they do, and on 2026-11-06 Joel wrote "we decided just yesterday that
  you are storing types with the words they apply to". What the checker
  knows about a word (its effect, its kind, its flags) is stored on that
  word's dictionary entry, and a type, its fields and the primitives' effects
  live in the wordlists of the program that declares them. The checker finds
  them through the dictionary's own lookup, and they travel in the records
  the linker writes.
- 2026-11-06, type names: shown that a bare type name also resolves when
  exactly one package anywhere makes a type of that name public (type-family.f
  TFAM-FIND-PUBLIC), a step bare words do not have, used at about 130 places
  in Habu and Tender (Tender's `request` and `response` from HTTP), and asked
  whether type names should follow the word scope rules, Joel wrote "a bare
  type name is still a word, prove me wrong!" No case was found where a type
  name must resolve differently: a bare type name is found as a bare word is
  (the open package, the globals, `using` imports), the fallback goes, and its
  users name the package or import it.
- 2026-11-03, state machines: asked to approve a package STATE-MACHINE with
  one definer, `MACHINE <name> ( context ) <states> <inputs> ... ;MACHINE`,
  and one error code for a badly formed table, Joel answered "call package
  FSM and MACHINE should be CREATE instead. then it reads like FSM:CREATE (or
  DEFINE) instead of STATE-MACHINE:MACHINE": package FSM with `FSM:DEFINE`
  (CREATE is Forth's word) and the code `FSM:E-SYNTAX`; the row keywords IN,
  FINAL and `>state` stand. He added that "state machines are easy to verify
  with formal methods, e.g. TLA+ so you need to think in state machines if at
  all possible and appropriate", and asked to "commit state machines and move
  it to habu master so that others can use it".
- 2026-11-03, package names: asked in Zen whether its package NUMFMT (for
  OOXML's numFmt, a number format) may keep a name the card's rule against
  coined abbreviations forbids: "NUMFMT is perfectly fine with me. the rule
  that forbids this should be changed!"
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
  these be in the SOURCE package?", then "either SOURCE or LOADER". The
  loader's packages and the table become one package SOURCE (the names he
  approved in the next entry); words only the loader calls go private,
  INTERN and PERSIST included. Asked
  where the error-place words go, he asked "what is TOP-LINE and why do we
  need it? I'd think we just need LINE (or LINE-AT? worse?) and COLUMN";
  shown that the top-level site is only a byte's line taken at the checker's
  token, he chose "SRCLOC:LINE, COLUMN, PATH$": `SRCLOC:LINE` and
  `SRCLOC:COLUMN ( ptr u8 -- n bool )` place any byte of the open file,
  `SRCLOC:PATH$ ( -- ptr u8 n )` names it, and top-level errors print a column
  too once tools/check.f's run file stops shifting columns. On 2026-11-03,
  shown that tools/tier0-profile.f's `corpus-raw` and `trivial-raw` modes time
  compiling with checking off and that the normal modes already report the
  checker words' share, he chose "Remove both (Recommended)"; shown that
  `top-check@`, the read of the top-level checker's cell, has no caller outside
  the switch's own tests, he chose "Remove it (Recommended)".
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
  build strips private names. On 2026-11-06, told that the installed hb keeps
  its private names and that nothing strips them, Joel wrote "we need to
  deploy T0 ASAP and can tweak sealing and stripping later": the hb the
  cross-build writes keeps its private names until after the T0 release.
- 2026-10-27, formats: "why do you need to version any schemas? we have no
  legacy. hard cutover only!" On 2026-11-03, told that a cached native unit
  file carries a format number that the family-words change alters, and that
  the sentence once recorded here after his words ("A format change rewrites
  its writer and reader together, with no version number.") was the
  recorder's, not his, Joel chose "Bump 26 → 27": a cache file built before
  the change is refused when opened and rebuilt, and nothing reads the old
  format.
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
- 2026-11-05, the OS's text for an errno (relayed by phil from Zen): offered
  FS:ERRNO-TEXT ( errno buf size -- text ) writing into the caller's buffer,
  Joel chose "FS:ERRNO$ ( errno -- text )": "Like FMT:INT$: text in a
  per-task buffer of fixed size."
- 2026-11-06, an error code's name (relayed by kevin from Tender): asked when
  one Habu word that turns an error number into its constant's name and
  comment, for Habu's codes and an application's own, should be designed,
  given that it means a definer for every error constant while incremental
  builds come first, Joel chose "Design now, in parallel". Shown the design,
  he chose "ERROR: definer (Recommended)": `-9701 s" text" ERROR: E-KEY`
  defines the constant as today and records its qualified name and text in
  the program's data, so a stripped program can name a code; every error
  constant converts, and the lead made the text required. The 107 codes
  minted before the error file loads stay plain constants for now. Asked
  about ERRORS:NAME$, Joel wrote "the package should be ERROR in error.f, why
  invent ERRORS?": package ERROR in src/lib/error.f (errors.f renamed), with
  `ERROR:NAME$ ( n -- ptr u8 n )` and, chosen "(Recommended)",
  `ERROR:TEXT$ ( n -- ptr u8 n )`, empty for an undeclared code. He chose
  "LOG:CODE+ (Recommended)", then wrote "what i want to see in the logs is
  the description of what happened" and "why not reverse this instead and
  write the number / name and then the description?": `LOG:CODE+ ( n -- )`
  writes `-9701 MODEL:E-KEY: the provider refused the key`, the number alone
  when undeclared. For tools/check.f's records of a thrown code he chose
  "Real name (Recommended)" over E-STATEMENT-THROW, and for building it "Now,
  in parallel". Shown that the design made `ERROR:` a global word, which
  needed a public ERROR:DECLARE to reach the package's table, Joel wrote
  "words ending in a colon should either be rejected OR they should be part
  of the package they are defined in. there's no rule that says they should
  be global!", chose "ERROR:DEFINE (Recommended)", then wrote "actually, you
  don't define an error so make it ERROR:CODE": the definer is package
  ERROR's public word, `-9701 s" text" ERROR:CODE E-KEY`, with no global and
  no DECLARE.
- 2026-11-06, outages apart from bad replies (relayed by kevin from Tender):
  Joel chose "Add E-UNAVAILABLE (Recommended)" for MCP: MCP:E-UNAVAILABLE for
  an HTTP 408, 429 or 5xx, as MODEL:E-UNAVAILABLE; E-RESPONSE keeps every
  other non-2xx and malformed reply. For BEARER he chose "Add E-UNAVAILABLE
  (Recommended)": BEARER:E-UNAVAILABLE for 408, 429 and 5xx, E-REFUSED the
  other 4xx, E-TRANSFER no answer; and "Add E-RESPONSE (Recommended)":
  BEARER:E-RESPONSE for a 2xx without an access_token or a positive
  expires_in. Asked whether the check of a reply's status for 408, 429 or
  5xx becomes one public word, he asked "why do you need to poll for
  errors? should those not be returned to you?"; told that nothing polls
  and that the status picks which error the call throws, he chose "One
  shared word (Recommended)": `CURL:UNAVAILABLE? ( http-status -- bool )`,
  called by MODEL, MCP and BEARER, replaces MODEL's private copy. Shown that
  the three clients turn the status into a number on arrival, he chose
  "Typed, clients carry it (Recommended)": they keep the typed status until
  the check.
- 2026-11-06, Maki's load cost (relayed by tim from Maki): shown that the
  checker costs about 116k instructions per token on Maki's 17.6 MB generated
  design (type unification, re-walked stack rows, guards on the checker's own
  stores) and that Habu has no one-token exact float literal, so
  `$403E… IEEE754:BITS>F64` makes 41% of its tokens, Joel chose "Both now, in
  parallel (Recommended)": workers find the checker's three costs and the
  literal is designed for his approval. Shown the literal's design, Joel wrote
  "the issue here is that maki needs precision. we are dealing with
  solidworks models that are often very precise. what's best for long term
  here?" The lead chose widening the decimal literal: any number of digits,
  correctly rounded to the nearest double, no exponent; every double has a
  decimal text that reads back to its exact bits, so no spelling is more
  precise than another. He chose "Keep num-parse (Recommended)": one reader
  in checked Habu, `num-parse ( ptr u8 n -- n bool bool bool )`, the last
  flag a number-shaped token out of range, replaces the machine-code readers
  and OUTER:NUMBER. It lands with arm64's move onto the typed interpreter.
- 2026-11-05, durable atomic writes (relayed by kevin from Tender): shown
  that FS:ATOMIC-WRITE-FILE renamed its sibling over the destination with no
  sync, so after a crash or power loss the destination could stand empty
  under its final name, Joel chose "Approve as designed (Recommended)":
  ATOMIC-WRITE-FILE and ATOMIC-WRITE-PRIVATE sync the sibling before the
  rename and the directory after, with F_FULLFSYNC on macOS, and no new word.
- 2026-11-05, a write-once file (relayed by stuart from Tender, whose
  admission replies Joel ruled are "built on a public Habu write-once
  word"): offered names for a word that writes a file only its owner may
  read only when its path is free, answering false and leaving an existing
  file untouched, Joel chose "FS:ATOMIC-CREATE-PRIVATE (Recommended)":
  FS:ATOMIC-CREATE-PRIVATE ( path pathu text textu -- bool ).
- 2026-11-05, which wait finished: shown that curl, the WebSocket server and
  the HTTP listener each cast AIO's ticket to a number to compare it and scan
  their slots for the one AWAIT-ANY returned, while epoll, kqueue, io_uring
  and mio hand back a number the caller attached to the wait, Joel chose "Tag
  each wait (Recommended)": GROUP+ ( ticket n group -- ) takes the caller's
  number and AWAIT-ANY ( group -- n outcome ) hands it back.
- 2026-11-01, long model calls: Tender lost documents when MODEL's total
  call bound ran out at about 300 s. A call has no total bound; a dead call
  shows as a failed status or a stalled connection. On 2026-11-04, shown that
  OpenAI's background mode queued requests a median 46 s and left 2 of 7
  unstarted after 10 minutes (Tender's hang), while held streaming requests
  started in 0.4-1.9 s and carried an event about every 8 s while thinking,
  Joel chose "Held stream (Recommended)" over background mode with a queue
  limit: CHAT sends one held streaming request, and a dropped connection ends
  the call E-TRANSPORT, which Tender retries. He then approved removing
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
  it, Joel answered "you run it".
- 2026-11-03, a model server's replies for tests (relayed by phil from Zen):
  asked to approve the public changes for moving Zen's model calls onto
  Habu's MODEL client, among them TEST-MODEL:COMPLETED!, INCOMPLETE! and
  REFUSAL! ( HTTP:response ptr u8 n -- ), which write the event streams
  MODEL:CHAT reads so a test drives it against a local route, Joel chose
  "Approve all (Recommended)": "Build the switch now as listed; Tender gets
  the change id."
- 2026-11-04, embedding limits (relayed by phil): for MODEL:INPUT-MAX, Joel
  chose "2,048, OpenAI's (Recommended)": "One request carries up to 2,048
  texts; applies to Ollama too." For MODEL:TOKENS-PER-EMBED-MAX 300000 and
  MODEL:TOKENS-PER-INPUT-MAX 8192, OpenAI's documented limits, which MODEL
  does not check, he chose "Approve both (Recommended)": "Names and values as
  listed; Zen groups texts into requests by them."
- 2026-11-05, a saved reply checked before sending (relayed by phil from
  Zen): for MODEL:JOURNAL-INPUT$, which finishes a chat request and answers
  the journal input a lookup needs, and MODEL:REPLAY, which restores a saved
  reply for that request and sends nothing, Joel chose "Add both words
  (Recommended)": "Check saved reply first, then send; carl lands them."
- 2026-11-06, many streams on one task (relayed by phil from Zen): shown
  that Zen's worker kept a thread per model call because CURL:NEXT arms,
  waits and takes in one word, Joel said "fix the cause and not the
  symptoms!" and chose "Approve three words (Recommended)": "ARM, READY?,
  TAKE as listed; NEXT and its tests unchanged." CURL:NEXT-ARM ( handle ptr
  u8 len -- ), NEXT-READY? ( handle -- bool ) and NEXT-TAKE ( handle --
  chunk-result ).
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
  addresses across a save. On 2026-11-04, asked what a pointer field in typed
  storage (TYPED-VARIABLE, TYPED-BUFFER, LAYOUT-BUFFER, DYNAMIC-BUFFER) holds
  when a saved image starts again, and shown that saving them as null fails 18
  of 520 suite rows (the save refuses image-lifecycle's PROCESS buffer; a
  route record declared at load time reads null text after restart and
  crashes) while keeping them passes all 520, Joel chose "Keep the address
  (Recommended)": typed storage's pointers are saved and moved with the image
  as PERSISTED-PTR-VARIABLE's are, a pointer into memory the build mapped for
  itself is refused at save naming the cell, and PTR-VARIABLE and the other
  scratch pointer words stay null. Asked to name TFAM-STORAGE-HAS-QUOT? once
  it answers for pointers too, Joel asked "why the TFAM prefix? do you have
  any other storage?"; told that the prefix only repeats the package and that
  only typed storage asks, he chose "TFAM:HAS-ADDRESS? (Recommended)".

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
  packages. CHECKER is fine". Joel chose "One level" of nesting: a part
  holds no parts.
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
- 2026-11-05, a pooled connection the server ended (relayed by phil from
  Zen): offered PG:ENDED? ( connection -- bool ), Joel asked "why do you need
  to poll? aren't you using epoll, kqueue, etc. to get notified?". Shown that
  an idle pooled connection has no task waiting on its socket, so no
  notification reaches anyone, and that pools check a connection as they hand
  it out, he chose "Approve, check on reuse (Recommended)": one zero-wait
  readiness query when Zen's pool takes a connection out.
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
- 2026-11-02, sizing Maki's job runners: OS-CPU:PERFORMANCE-CORES and
  OS-MEMORY:TOTAL, approved on 2026-10-31 so Maki could compute its runner
  count from the host, needed a shared reader of Linux container (cgroup)
  limits. Asked to approve a CGROUP package for it, Joel answered "do not
  overcomplicate things. discard the whole thing and just make it a config
  file setting. we do have a config file, do we?" Told that Maki reads its
  settings from environment variables, he chose "New Maki config file": the
  runner count is a setting in a new Maki config file that takes all of
  Maki's settings, and Habu gets no host-size words.
- 2026-11-04, Maki's file store (relayed by tim): Maki writes an upload,
  syncs it and renames it into place before the PostgreSQL row naming it
  commits, and after a crash cuts a file back to its last committed part.
  Asked to add FD:SYNC ( fd -- ), fsync for a file or a directory, plain
  fsync on macOS as PostgreSQL's default is, Joel chose "Add FD:SYNC
  (Recommended)". Asked to add FD:RESIZE ( fd n -- ), ftruncate, replacing
  fs-mutate's private binding (TRUNCATE is a global word), Joel chose "Add
  FD:RESIZE (Recommended)". No write-at-offset word: cutting back and
  appending covers the case.
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
  reopenable. On 2026-11-06, asked "why do we need to seal anything? are you
  fixing the cause or the symptoms?" and shown that any program can run a
  private hb word through the dictionary's public lookup words and a cast,
  Joel wrote "there's a fine balance here. i don't want to complicate things
  unnecessarily. this is forth and you could get to the private words by
  calculating offsets, etc. i only care about sealing if it's light and
  easy!" The lead decided: a sealed package is one flag on the package's own
  record, which `seal-package` sets; `package` refuses to reopen a sealed
  package, and nothing else reads the flag. The fixed list of sealed package
  names, the header's list of protected wordlists, `prot-wid-add` and the
  build pass that seals every package go; the engine files' `get-current
  prot-wid-add` becomes `seal-package`. The image's own words stay
  unremovable through the dictionary floor, which the linker writes into the
  image it builds instead of the startup text setting it.

## Compiler and builds

- 2026-11-04, the check tool before incremental builds: asked to approve
  SOURCE-SYNTAX:COMPILE-KEYWORD?, a new public word so that checking a file
  before running it (the language server, tools/check.f) refuses `: 42` and
  `: if` as loading does, Joel answered "the fuck do you need this for? why is
  the package named source-syntax? is this part of the LSP? drop the LSP work
  and move forward with incremental builds, we'll fix the LSP when we are
  done". The check tool's work, the number-name refusal's second half
  included, waits until incremental builds are done, apart from building the
  verifier into hb (2026-11-05, tools in hb); loading's refusal of `: 42`
  stands.
- 2026-11-05, tools in hb: asked why tests need their own images and "why
  not build those tools in NOW?", Joel wrote "tools should be built in to
  avoid rebuilding images!" hb carries the tools its tests and builds drive:
  the cross-build's linker goes into hb at step 5, then app builds and REPL
  images run on it, and each tool image goes with the step that moves its
  last user. Asked when hb should carry the verifier, he chose "Right after
  step 5 (Recommended)".
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
- 2026-11-03, the string literal store: told that tier 1's NSTR
  (src/compiler/native/string.f) is the store every compiler, the image
  capture and the hb build use for string literals, and that STR is the string
  library's package, Joel asked "i think interning is its purpose, correct me
  if i'm wrong", then "call the package INTERN then?", and on the plan to move
  it to src/habu/intern.f as package INTERN, with the old build cache's and
  dead words out and tier 1's calls following, answered "do it". That plan
  named the store's main word INTERN:STRING; asked why the move had become a
  redesign, he wrote "we discussed INTERN:STRING so that one is fine".
- 2026-11-03, where strings and their lookup table live: shown that the
  store keeps strings in 512 KB blocks chained through DATA with a per-block
  lookup table and a 512 KB limit per string, Joel asked "why are we
  chaining anything", then "what's wrong with mixing all kinds of data in
  DATA, including strings? why did we bother with blocks?" and "how does your
  hashtable grow?", and ruled "your hashtable cannot live in data. strings can
  but hashtable cannot. that one needs an arena of its own, or somethibng like
  that". Strings are stored in DATA among other data; the table that finds an
  existing string lives in growable memory of its own, outside DATA. He then
  ruled "any strings stored during a failed repl line should be discarded,
  proev me wrong!": they go with DATA's rewind and the table forgets them.
  Asked "are you gonna search the list? why not the hash table?", "why does a
  slot need to store the length when we store the length with the string in
  DATA?" and "why do we not store each string Pascal-style, with length in
  front of it? we could add a zero at the end for compatibility with C
  strings if need be", and shown that FFI copies every string it hands to C,
  so a trailing zero would spare no copy, Joel answered "great, keep going":
  each string is a length cell followed by its bytes, the table's slots hold
  only addresses, and no trailing zero is added until a caller can use one.
  On 2026-11-05 he asked "why are you keeping a list of strings? i thought
  strings are stored at different addresses in DATA and you just have a hash
  table of them" and "why are you considering moving HERE back in the first
  place if you don't have any cases where it does move back? why not just
  prohibit moving HERE back for now?" Shown that nothing moves HERE back
  while a build writes the next hb (a failure ends the build and drops its
  data space) and that the failed REPL line keeps today's store until the
  running hb's own strings move onto this one, he chose "Prohibit
  (Recommended)": while a build's data space is open the table only adds,
  and if HERE moved back the build fails at its next string. He named that
  error E-INTERN-REWIND. Shown that a build's string table outlives its data
  space, so a second build the OS maps at the same address could find the
  first build's strings at addresses it has since filled with other data, and
  asked to name the word a build's close calls to drop that table, Joel asked
  "is there a better name for SPACE here?" and "why not INTERN:RELEASE?", then
  wrote "better yet, INTERN:CLEANUP": `INTERN:CLEANUP ( -- )` frees the
  table of the build whose space is closing.
- 2026-11-03, the callback package: asked what the callback library's
  package FFI-CB should become, and shown that FFI is built into hb and sealed
  there, so a library loaded later cannot add words to it, Joel first chose
  "CALLBACK (Recommended)", then answered "i was wrong. callbacks should live
  in the FFI package since they are FFI callbacks". The callback words join
  FFI, so src/lib/ffi-callback.f is built into hb with it.
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
  `NCOMP-DISPATCH:COMP-SHADOW-CELL`. Asked afterwards to rename CALLBACK-STUB@,
  which now ends the request, he answered "i did not approve any use of
  NDEFINITION, i object to the use of the N* packages and thought you were
  redoing them", and asked why the request must be ended and how the FFI
  callback setup works. His approvals covered the words' names and
  contracts, not their place in NDEFINITION.
- 2026-11-03, defining a callback (design rev 3 after critic round 3): asked
  one item at a time, Joel chose "Engine word (Recommended)": the C entry a
  `CALLBACK:` declaration needs is made by a new engine word beside
  `constant`, not by a TRUSTED: word in FFI, and he named it "callback-stub
  (Recommended)", used as `3 callback-stub CMP-ENTRY`. He asked "how is this
  gonna work with cross-compilation, though? and how do you reuse the callback
  frontend for x86, wasm, etc?", was shown that T0 stamps the stub for both
  machines as it does a constant's body, then chose "Name, then word
  (Recommended)": `CALLBACK: CMP ROW-ORDER ( ptr u8 ptr u8 -- i32 ) 0 FALLBACK
  ;CALLBACK`, the bridged word named in the declaration, with no later store,
  and "callback (Recommended)": `COMP-BODY:callback`. For the word that binds
  a callback to a context and answers its C pointer he wrote "use
  BIND-CALLBACK if it's FFI:BIND-CALLBACK and internal. if you do no other
  binding then just use BIND"; FFI binds nothing else, so it is `FFI:BIND`,
  and by that rule a callback word in FFI drops the CALLBACK- prefix where FFI
  has no other word of its kind (UNBIND, ARG@ and its typed forms, RUN,
  DESCRIPTOR, FAULT@; SET-FALLBACK, SET-FFALLBACK, OPEN-CALLBACK,
  CLOSE-CALLBACK and the type `callback` keep their names). He chose
  "RESULT!" over CALLBACK-RESULT! for the word that sets C's answer (FRESULT!
  beside it), then asked "but why would you ever set it? and when and why
  would you check for it?" and was told only the generated dispatch calls it,
  and he chose "CLEAR-FAULT (Recommended)" for today's CLEAR.
- 2026-11-03, sharing strings: shown that an app image gives each string it
  compiles its own copy instead of sharing one stored before the image's
  window, Joel wrote "it's not a matter of enabling sharing it's that SHARING
  IS THE FUCKING WAY TO GO!!! ... you do not copy strings when linking C
  object files, you just use the strings, prove me wrong!" Every string is
  stored once and shared; a link writes each string an image uses once and
  points every use at it. Shown that a top-level `s"` lays a fresh copy at
  `here` each time, he wrote "this is wrong, all strings should be interned!"
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
- 2026-11-03, file objects: Joel wrote "whatever creates the object files (is
  it loader? should it be linker instead if it both saves and load?) has
  control over what executes when those object files are linked into an
  executable. it sounds like the current design is bad, prove me wrong!" and
  "we are not specifying the loading instructions correctly at the moment and
  have to do a lot of extra work, e.g. to discriminate loads". Told that a
  file's top level also prints, writes files, reads the environment and opens
  foreign libraries, he answered "whatever executes when linking has nothing
  to do with the file top-level as they are instructions added by the
  linker. prove me wrong!", then "we also need to rename LOADER to LINKER,
  right? otherwise, keep going and keep it simple!" A linker writes a file's
  object as the instructions that remake the file's result, and a reused
  file is linked without reading its source.
- 2026-11-06, the saved file object: shown the object a build saves for each
  file it loads (one file under <out>/obj named by a key over hb's own
  bytes, the build's mode and target, the file's path, load kind and loader;
  overwritten when the file compiles again; references by name, not
  address), against keeping every version or saving raw memory, Joel wrote
  "i don't know enough to approve it. is it what's best for long term?" The
  lead chose one object per file, overwritten on recompile: objects are a
  cache any change of hb rebuilds, so a later layout change costs one cold
  build; keeping every version helps only a return to an old edit and needs
  a cleaner; raw memory would rebuild every later file after an edit, not
  just its dependents.
- 2026-11-03, x86-64 builds: asked whether to remove two of SOURCE-EXEC's
  words while a lane gave the x86-64 kernel the observer the build cache
  watches a file's re-run through, Joel asked "what is source-exec and why do
  we need it?", then wrote "we don't have the loader anymore. you need to
  implement the linker and then switch x86 (then wasm?) over to it". The
  linker comes first; x86-64 gets its file builds from it, not from the
  observer.
- 2026-11-03, the linker's build word: shown the critic's second-round
  rejection of the linker design and asked to approve `LINKER:WITH ( ptr u8 n
  [ -- ] -- )`, today's NDEF-CACHE:WITH renamed, whose mode string "engine"
  marks an engine build, Joel chose "Approve with mode (Recommended)": every
  build caller (hb-build-core.f, aot-builder.f, native-build-core.f,
  app-build.f) runs inside LINKER:WITH, and RUN, APP-LOAD and WITH-QUIET go.
- 2026-11-04, how Habu builds Habu: shown that five linker designs kept the
  build that loads the product into the running hb's memory and cuts it out,
  Joel wrote "the reason you cannot compile hb inside a running hb that also
  holds the compiler, build tools and state is because your design is crap.
  and you keep piling more rules on top instead of re-evaluating the design,
  prove me wrong! you have cross-compilation it does a similar thing,
  compiling a habu from a habu". Shown that the docs describe every build as
  a cross-build and that the x86-64 build writes its product as data, he
  ruled "building habu from habu is like cross-compiling from arm64 to arm64
  indeed. make it happen". An arm64 build is a cross-build whose target is
  arm64: the product is data the build writes, never a slice of the running
  hb's memory. Shown the design (hb writes the next hb as a file; while it
  builds, the product's data, names and strings live in a space of their own,
  so nothing is rewound and the tree's checker never takes over mid-build; the
  arm64 hb takes x86-64's shape, a primitive kernel plus the typed
  interpreter, written by one linker with a code patcher per machine and a
  file writer per OS; then x86-64 and Wasm move onto that linker, then files
  are reused whole), Joel chose "Approve, start building (Recommended)":
  "Small green commits; the old build path stays until the new one is
  proven." The JIT and the Gforth chain stay untouched until then, and their
  retirement is asked before the step that needs it. Asked to name the header
  cell holding the lowest address the data pointer may take, Joel chose
  "DP-FLOOR-CELL (Recommended)"; its partner is DP-CEILING-CELL. Asked to name
  the header cells holding the wordlist the engine treats as global (0 today)
  and the marker a package's row carries (-1 today), which point at fresh ones
  while a build writes the next hb, Joel chose "GLOBAL-WL-CELL,
  NAMESPACE-WL-CELL (Recommended)". Asked which public word reads the marker
  for the checker, which loads before layout.f, he asked "why do you need a
  boot-layout package? do you have a boot-<something else>? why not just
  BOOT?", then chose "BOOT, fold BOOT-SRC (Recommended)": the header cells the
  boot writes (the heap start, the DP bounds, the wid cells and BOOT-SRC's
  USER-END) are package BOOT. He chose "BOOT:NAMESPACE-WL (Recommended)", the
  owner's reader in a file loaded before the checker, which every file calls in
  place of its own read; BOOT:GLOBAL-WL reads the global wid beside it.
  Asked on 2026-11-05 to name the header cell holding the start of the hb
  being built (0 while no build runs), Joel chose "BOOT:IMAGE-BASE-CELL".
  Shown the same day that the product's fresh global wordlist cannot see the
  kernel's built-in words, and asked to name the wordlist id the design gives
  them in place of about 700 alias records per build, Joel asked "WL is
  wordlist? what is DICT here and what does DICT-WL mean?", then wrote "Why
  not WL? Otherwise, what does <World List ID>:PRIMITIVES mean? WL:PRIMITIVES
  makes sense" and "collect worldlist-related things in the WL package, not
  the WID pacakge". The built-ins' wordlist id is WL:PRIMITIVES, and
  DICT-WL's markers and WID:MAX move into package WL. Shown that eight
  places test whether a word is global by its wordlist and asked how they
  should count the built-in words, Joel asked "why do you need to know if a
  bare name looks in this wordlist? what is a bare name?", then chose
  "WL:GLOBAL? (Recommended)": one word in package WL that answers yes for
  the global wordlist and WL:PRIMITIVES.
- 2026-11-05, the checker during a build: shown that hb's checker refuses the
  product's IMK-NDICT0 as a duplicate of its own, and asked to name the two
  words the build's open and close call so the checker checks the product
  apart from hb's own names, Joel chose "CHECK:OPEN, CHECK:CLOSE
  (Recommended)": `CHECK:OPEN ( -- )` and `CHECK:CLOSE ( -- )`, called only
  by LINKER:WITH's open and close.
- 2026-11-05, the old builders: shown that DATA-FLOOR-CELL is a second floor
  for DP, kept only by the old string store so a failed line's rollback never
  drops DP below strings compiled code points at, and asked when it should go,
  Joel chose "Move hb's strings now" and wrote "always hard cutover" and "why
  are we keeping old builders around?! is this a must?" Shown that until the
  cross-build writes a working hb only the old hb builder can put a change to
  hb's own files into bin/hb, and that the Wasm and x86-64 builders use the
  same string windows, he wrote "you do arm64 first, disable x86 and wasm
  temporarily. once arm64 works you do the cutover of x86 and wasm. this is
  the way, prove me wrong!" When the arm64 cross-build writes a working hb,
  the old hb builder, its string windows and DATA-FLOOR-CELL go and hb's own
  strings move onto the one table; x86-64 and Wasm builds are off until each
  moves onto the linker.
- 2026-11-05, cutting the address-cell list: asked "what does
  ADDRESS-CELLS:KEEP-BELOW do? what else is in that package? is this the best
  package for it?", and shown that the list records each DATA cell holding an
  address for whoever writes the program out (the linker, as C's relocations),
  that a failed line's rollback cuts it, and that an offset from hb's DATA
  makes a failed line inside a build cut hb's own rows, Joel chose "Address,
  keep package (Recommended)": `ADDRESS-CELLS:KEEP-BELOW` and `REMOVE-SPAN`
  take an address and cut the list of the region holding it, as EMIT-MARK
  picks it; the package stays the engine's.
- 2026-11-05, where a built program's code runs: asked to name REGION-VA, a
  fixed address the arm64 boot would copy a built program's code to so that
  no stored code address needs adjusting, Joel asked "why do you need any
  fixed code addresses at all? justify". Shown that the copy costs a copy of
  all the code at every start, a private copy per process in place of shared
  file pages, code that is not the signed file's, and a fixed address
  something else can take, he chose "Drop it, fix up (Recommended)": "Code
  runs from the signed file, shared; start-up adjusts listed addresses." A
  built program's code runs where the OS loads it, calls inside it are
  relative, the linker lists every stored address that points into the
  image, and start-up adds the load offset to each, as dyld and ld.so do.
  Shown the S6 design that follows (one code section loaded anywhere:
  start-up, the arm64 kernel, the app's words, then a read-only tail with
  DATA's bytes and the address list; code holds no absolute code address;
  DATA copied raw at start; a stripped app has no dictionary), he chose
  "Approve, start lanes (Recommended)".
- 2026-11-05, where machine-specific code lives: asked to name the arm64
  start-up's package, Joel asked "what's best for long term? also, why no
  arch directories under src, e.g. src/habu/arm64/boot.f". Shown that
  src/arch/<isa>/ already holds one directory per instruction set, Wasm's
  linker and kernel words included, while the arm64 and x86-64 engines grew
  in src/habu as suffixed files, he chose "src/arch/arm64/boot.f, A64BOOT
  (Recommended)": "One directory per instruction set, as Wasm's linker
  already is."
- 2026-11-05, the arm64 assembler: shown that the arm64 linker writes each
  code address as adrp+add so it holds wherever the OS loads the program, and
  that A64ASM encodes adr but not adrp, Joel chose "A64ASM:ENC-ADRP
  (Recommended)": "Public beside ENC-ADR; the assembler owns every instruction
  encoding." `A64ASM:ENC-ADRP ( rd pages -- insn )`. Shown the same day that
  the linker's call patcher wraps A64ASM's bl/b encoders in its own alignment
  and reach checks, as T0, tier 1 and the code buffer each do, because those
  encoders mask a distance that does not fit, and asked whether one public
  branch word should be shared, Joel wrote "there's no sharing here. we have 1
  assembler and only one and everyone uses it!" and "there's no fucking way a
  linker should have its own assembler, prove me wrong!" Shown the fix, he
  chose "Approve, byte distances (Recommended)": ENC-B, ENC-BL, ENC-BCOND,
  ENC-CBZ and ENC-CBNZ take a byte distance, as ENC-ADR does; they, ENC-ADR
  and ENC-ADRP refuse a misaligned or unreachable value with E-A64EMIT-REACH;
  each field's reach is written once, in asm.f; T0, tier 1, the code buffer
  and the linker call them and keep no reach check of their own.
- 2026-11-06, the kernel a program carries: shown that every built program
  copied the whole arm64 kernel (hello world 247,932 bytes, of which it runs
  about 2 KB), and that the landed cut has the assembler record every label
  reference it writes so the linker keeps only the kernel code a program
  reaches and the assembler moves it together and re-encodes its references,
  as `ld --gc-sections` does (hello world 33,276 bytes), Joel chose "Keep the
  cut (Recommended)" over the whole kernel or rewriting habu2.f's generator to
  write each primitive alone. He chose `A64ICODE:REFS ( -- n )`,
  `A64ICODE:REF-SITE ( n -- n )` and `A64ICODE:REF-LABEL ( n -- label )` for
  reading the recorded references. Asked to name the cutting word, he asked
  "is this not something you can decide yourself using the "what's best for
  long term" lens?"
- 2026-10-29, recovery: asked "Which recovery route for arm64?", Joel chose
  "Cross-build from a running hb (Recommended)": "Recovery starts from a
  released hb, as Intel does today; the Gforth seed retires with the JIT."
  On 2026-11-04, shown the Gforth chain red on master and a seed mirror
  planned for the cross-build's wid cells, Joel wrote "why do you need gforth
  to build habu from habu? your priority is arm64-to-arm64 cross-build": the
  cross-build's steps carry no Gforth seed mirror. On 2026-11-06, shown that
  AGENTS.md, docs/bootstrap.md and docs/gate.md still told a change to the
  seed or its mirror to run the Gforth recovery check, Joel chose "Retire it
  from the rules (Recommended)": no change runs a Gforth check or edits
  bootstrap/ to follow the tree.
- 2026-10-28, one compiler: "we want a unified backend and frontend that
  allows for cross-compilation. so we want to reuse as much as possible and
  only change arch-specific bits." One T0 front end, one publisher and
  capture path, an emitter per architecture. On 2026-11-04, shown a grant
  row that x86-64's typed interpreter needed and arm64's machine-code
  interpreter did not, Joel wrote "we decided to unify the frontend and just
  have different code-generation backends. why the fuck do arm64 and x86
  have different interpreters? why does arm64 not use the typed one if it's
  better?": the interpreter is front end, one typed interpreter for every
  machine.
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
  doing intel when the focus is arm64? is that necessary?" On 2026-11-05,
  shown x86-64 programs run on the Intel host to prove a change to the shared
  lookup, Joel asked "are you laser-focused on arm64-to-arm64? why do you keep
  working on x86?", then ruled "i don't care about x86 or wasm until
  arm-to-arm is working. get it done": until the arm64-to-arm64 cross-build
  works, x86-64 and Wasm get no work, builds or runs beyond the suite's own
  tests. 2026-10-31, on how other work fits the T0 plan: "i want small tight
  improvements on top of t0."
- 2026-10-29, build cost: "i need fast incremental builds to enable you and
  other agents to quickly iterate on features. right now we are in rust cargo
  build times and it fucking sucks because we are forth!" A build whose cost
  is out of proportion has a defect to root-cause first. On 2026-11-06,
  shown a chain build's engine step timing out at 600 s and its rerun with a
  30-minute limit, Joel wrote "rca and troubleshoot, always immediately! and
  30 minutes to build an engine is denied. 5 minutes at most!": an engine
  build takes at most 5 minutes, a slow or failed build is root-caused at
  once, and no limit is raised to get a build past it.
- 2026-10-31, startup reuse: asked to approve a public word that compares the
  tree's source hashes with the engine's so hb-build could skip its 3.3 s
  startup reload, Joel answered "why are we developing incremental building if
  we are gonna check hashes of source files? one of the goals was incremental
  building of habu itself". On 2026-11-03, shown that under the linker model
  hb is itself the linked result of its startup files, so the same check is
  the linker's out-of-date rule applied to hb while Habu's own build still
  links file by file, he chose "Yes, hb is linked (Recommended)": hb-build
  skips the reload while every startup file hashes as hb recorded.
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

## Processes

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
- 2026-11-05, pipes: shown that the HTTP and WebSocket servers' wake pipes
  reach every program a handler starts, while process.f and the macOS AIO
  loop each mark theirs close-on-exec under the descriptor lock in their own
  copy, Joel chose "Approve FD:PIPE (Recommended)": "One pipe word in FD;
  HTTP, WebSocket, AIO and process code use it." FD:PIPE ( -- read-fd
  write-fd ) makes a pipe whose ends close on exec.
- 2026-11-05, a non-blocking descriptor: shown that SIGNAL:INIT marks its
  pipe non-blocking through process.f's PROC-NONBLOCK!, whose refusal leaves
  both ends open and cleans up a child-process record SIGNAL:INIT never made,
  and that O_NONBLOCK's number is written in fd.f, process.f and tcp4.f, Joel
  chose "Add FD:NONBLOCK! (Recommended)": "One public word in FD, E-FS-IO on
  refusal; signal.f uses it." FD:NONBLOCK! ( fd -- ).

## Tests

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
- 2026-11-05, chosen tests: Joel asked to "have rows run tests in parallel if
  possible and not repeat the master test suite if you are gonna run it
  anyway". Asked whether `bin/hb --load test/run.f -- <test names>` should
  run just those tests in the suite's parallel pool, he answered "yes". A
  landing tip that runs the full suite runs no test separately.
- 2026-11-05, load-bearing tests: "all tests slow down development so only
  the load-bearing tests and suites should be left", and "review these with
  me, each and every one. justify keeping or dropping each before asking
  me". Shown that native-build-recompile (1403 s, the suite's wall time)
  tests the per-definition build cache the file linker replaces, Joel chose
  "Drop it (Recommended)". Shown that native-build-entry builds a whole x86-64
  hb through a retired loader fixture and a whole Linux arm64 hb it checks
  only by its ELF header, beside seven argument checks, he chose "Argument
  checks only (Recommended)". Shown that the gate's whitebox engine build also
  runs seed, growth and source-map checks of the capture build, he chose "Drop
  the check (Recommended)". He then wrote "hey, drop what you think should be
  dropped if best for long term" and "then only ask me about what's
  undecided": which tests to keep, shrink or drop is the lead's call. Shown
  the second removal wave's full-suite run (about 8 minutes), Joel asked "why
  are you running and re-running these suites before pruning them? we should
  fit into 2-3 minutes, 5 at most!": the full suite's wall time is 2-3
  minutes, 5 at most.

## Docs

- 2026-10-23: "if you are trying to sync docs with code it means you should
  drop the docs and document the code instead." A doc passage that must track
  code is deleted and the fact goes into the code's comment.
- 2026-11-03, this file (relayed by phil): "you are not saving lessons to
  LESSONS.md but saving decisions instead. you are also often saving
  decisions before we discuss them. adjust your instructions to name the file
  DECISIONS.md instead of LESSONS.md and to NOT save premature decisions
  before we settle them". Asked where lessons learned go, he chose "Decisions
  only (Recommended)": "Lessons move to docs by subject, or the global rules
  for ways of working."

## Installing

- 2026-10-28: "remember that there's just 1 public hb copy now, no versions!"
  The public Habu is ~/.local/lib/habu, updated in place.
- 2026-10-28: "nobody should ask to hold anything. they should have their own
  copy of habu to run if they need". An install never waits on a client.

