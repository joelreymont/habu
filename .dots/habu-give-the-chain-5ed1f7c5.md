---
title: Give the chain a no-emit compile mode
status: active
priority: 2
issue-type: task
created-at: "2026-08-06T16:07:09.741993+02:00"
---

Claim: agent=noemit workspace=.jj-ws/habu-give-the-chain-5ed1f7c5

THE structural blocker of the cut (thecut audit, 2026-08-06): the chain is a post-pass over the old emitter — its only input is the tape, whose sole producer is the checker's reader at every ';' AFTER the old emitter succeeds, and migrate.f reaches it via evaluate with PUBLISHED-ONE enforcing old-emitter success first. The cut needs compilation-without-publication: the checker certifies and produces the tape, the chain compiles it, and NOTHING publishes until the chain's publisher commits — the old emitter's emission becomes unnecessary rather than prerequisite. This is engine+checker surgery at the ';' seam (habu2.f EM-COMPILE / the reader), designed so a chain refusal leaves the definition uncompiled with a named reason. First consumer: the cut. Blocks habu-cut-colon-compilation-a5aa3f1f.

DESIGN (2026-08-06, agent=noemit, measured at ba7935d1)

The premise the dot was written on is half right, and the half that is wrong makes
the work much smaller. The tape is NOT downstream of the emission. The publication
is the only thing the chain actually depends on, and the engine already owns an
exit that certifies a definition, seals its tape, publishes nothing and gives every
emitted byte back. No-emit is a third verdict on that existing exit, not a new
compiler.

(a) CAN THE TAPE BE PRODUCED WITHOUT THE OLD EMITTER RUNNING?

The tape is not entangled with emission. It is sequenced after it, and only
because the check hook fires at the publish tail.

  - The tape's rows are written by the checker's own reader. CHECK-SCAN
    (src/core/checker.f:10231) walks TBASE/TBLEN — the reconstructed definition
    text — and calls CHECKER-TAPE:TOKEN per token (checker.f:10259-10261). Nothing
    it reads is machine code. src/compiler/native/feed.f:6-11 states the same rule
    from the consumer side: this file adds no lexer, it hangs on the one reader the
    engine already runs.
  - That text is the body capture, not the emission. LBCAP is called at the head of
    the compile-mode dispatch (habu2.f:6339, EM-COMPILE-KEYWORDS) and at the colon
    entry to seed the name (habu2.f:4648), so a token reaches the capture buffer
    before any handler decides what to emit for it.
  - The reader reaches its end-of-scan callback on a path that publishes nothing
    and emits nothing that survives. CHECK! calls CHECKER-TAPE:DONE with whatever
    verdict it reached (checker.f:10959) — reject included — and only AFTER that
    does the hook decide what to do about the verdict (check-hook.f:31-41).

  CORRECTED BY PROBE, and the correction matters for the shape of the change.
  The first draft of this block said a refused definition leaves migrate.f
  refusing the VERDICT. It does not. Today's hook does not RETURN a bad verdict,
  it THROWS (check-hook.f:41, rc 70), so the throw escapes `evaluate` inside SCAN,
  RECORD's catch calls NFEED:ABANDON-UNIT (migrate.f:293-294) and the recorded
  unit is given up. E-NMIGRATE-VERDICT (-8572) is therefore unreachable from an
  ordinary checker refusal. Measured:

      migration of a refused body      rc 70      (the hook's throw, not -8400/-8401)
      migration immediately after it   rc 0

  Two things follow. The failure is NOT in the E-NFEED-* band, so nothing about
  the recorder went wrong on the refused scan — the unit was abandoned by policy,
  not broken. And the following migration succeeding proves ABANDON-UNIT really
  does put the producer back, which is the property the refusal fixture will lean
  on. The design consequence is direct: the hold must be a RETURN and never a
  throw, because a throw is what makes RECORD abandon the very tape the chain is
  waiting for.

  MEASURED. A checker-rejected definition compiled through `evaluate`, and a
  certified one, on ba7935d1 with the refreshed engine:

      checker-rejected    ndict 5729 -> 5729     cp 4359295148 -> 4359295148   rc 70
      certified           ndict 5729 -> 5730     cp 4359295148 -> 4359295208   rc 0

  The rejected definition ran the whole compile loop, emitted its body, ran the
  checker and sealed its tape — and left the dictionary count and the code pointer
  bit-for-bit where it found them. Emission is already reclaimable at this seam;
  the engine does it today for every definition the checker refuses.

(b) WHAT DOES THE OLD EMITTER'S PUBLICATION PROVIDE, AND CAN NPUB CREATE?

The publication provides much less than "the record". The record is built by `:`,
not by the emitter's tail.

  - EM-INTERPRET-COLON (habu2.f:4628-4668) allocates the record at slot NDICT,
    parks its address in PEND-CELL (:4650-4651), writes the qualified name
    (C-QUALIFY-DEF :4649, C-STORE-DEF-NAME :4652) and writes the entry address into
    cell 0 (:4653). EM-COMPILE-FLUSH-PEND (habu2.f:6198-6201) writes cell 1, the
    length, as CP - entry - 4.
  - So the publication tail is only four things: `NDICT NDICT 1 ADDI` and the hash
    index insert LHIDXADD (habu2.f:6253), then EM-REC-WIDE-PUBLISH (:6254) which
    consumes the checker's two latches — RECW through wide-mark and RECMI through
    the DNAME-MIN-IN poke at habu2.f:1874-1877 (checker.f:4722-4740 says why they
    are read after ndict++).

  NPUB HAS NO CREATE PATH, only republish. REPUBLISH (publish.f:595) goes through
  TARGET -> NAME-REC -> XREF-FIND-WL-INDEX and throws E-NPUB-NAME when the name
  does not resolve (publish.f:312-313); RETARGET-REC is `xref-retarget`, which
  writes two cells of a LIVE record (publish.f:238-239); and the log reads
  XREF-START/XREF-LEN off the record it is replacing (publish.f:598-599).

  BUT THE PUBLISHER DOES NOT NEED A CREATE PATH, AND SHOULD NOT GET ONE. Under
  no-emit the record already exists — `:` made it, complete with name, wordlist and
  flags — it is merely unpublished. Minting a second record constructor inside NPUB
  would give the system two ways to build a dictionary record that must agree
  forever. What NPUB needs is a COMMIT of the record the engine is already holding:
  retarget its two cells at the chain's routine, then publish it.

  AND EVERY PRIMITIVE THAT COMMIT NEEDS ALREADY EXISTS.
    - `ndict!` (habu1.f:2679, axiom checker.f:5417, NOT trusted-only) publishes by
      raising the count, and its raise leg calls HIDX:LREBUILD (habu1.f:1210-1212)
      so the name index stays authoritative. LHIDXADD itself has no Habu spelling
      (habu1.f:2974, never registered) — the raise leg is the only door, and it is
      open.
    - `wide-mark` (habu1.f:2682) targets the newest published record, which after
      the raise is exactly ours; `min-in-mark ( rec-idx min -- )` (habu1.f:2684)
      takes the index explicitly.
    - `xref-retarget` and `code-publish` are already wrapped in publish.f.
  COST TO MEASURE IN PHASE 2: HIDX:LREBUILD is O(NDICT) per commit where the
  engine's own insert is O(1). Acceptable for the fixture; NOT obviously acceptable
  for the cut, which commits every definition in the tree. If the measurement says
  so, the honest fix is to expose the engine's existing one-record insert, not to
  keep a rebuild.

(c) THE SMALLEST SEAM CHANGE

Widen the check hook's return from a flag to a three-valued verdict, and give the
publish tail a third exit. Nothing else in the engine changes.

  The hook already decides publication. src/core/check-hook.f HOOK ( ptr u8 n -- n )
  returns -1 to commit, and the engine reads that return at habu2.f:6232
  (`10 G-POP 10 rejected CBZ,`) — non-zero commits, zero takes the `rejected` leg
  which rewinds CP from the pending record and skips the publish label entirely
  (habu2.f:6238-6241). Note that today's hook never actually returns zero: it
  returns -1 or throws (check-hook.f:29-41), so that leg is a fail-closed backstop
  with no production caller.

  THE CHANGE, in full:
    1. habu2.f EM-COMPILE-PUBLISH-HOOKED: after the existing zero test, compare the
       hook's return against the HOLD code and branch to a new `held` exit. `held`
       does what `publish` does NOT do — no ndict++, no LHIDXADD, no
       EM-REC-WIDE-PUBLISH — and, unlike `rejected`, does NOT rewind CP. It falls
       into the shared `finish` tail, so PEND-CELL is cleared and the interpreter
       leaves compile mode exactly as it does today. One comparison, one block.
    2. src/core/check-hook.f: return the HOLD code instead of -1 while no-emit is
       armed. Who arms it is Habu's business, not the engine's, so the engine gains
       no cell, no primitive and no layout slot.
    3. The chain reads the held record at slot `ndict@`. XREF-REC (xref.f:38-42) is
       unbounded, so the unpublished slot is readable: name, wordlist and entry
       address all come off the record the engine built.
    4. NPUB gains COMMIT-HELD: validate exactly as REPUBLISH does, retarget the held
       record's two cells at the emission, then `ndict@ 1+ ndict!` and apply the
       checker's wide/min-in facts. Same two-phase contract — every refusal before
       the first byte moves.
    5. migrate.f: under no-emit, PUBLISHED-ONE inverts. It asserts the count did NOT
       move and exactly one record is held; the name comes off the held record
       instead of off ndict@ 1-.

  WHY THE VERDICT AND NOT A MODE CELL. The engine must not learn what a migration
  is. A cell in layout.f would be engine state describing a chain that the engine
  has no other knowledge of, it would persist into snapshots, and it would need a
  primitive or a raw-offset poke to arm. The hook's return is a value the tail
  already has in a register at exactly the right instant, and the decision stays
  with the party that owns it.

  THE REFUSAL PATH. A chain refusal happens after `evaluate` has returned, so it
  cannot be undone inside the hook. It is undone where migrate.f RUN already
  catches and rethrows (migrate.f:668-679), by the landed three-watermark rollback:
  ndict! (already correct — nothing published), CODE-RECLAIM:TRUNCATE back to the
  held record's entry, and the checker's certified-signature truncation, which is
  what HIDE-DEFS-FROM does (xref.f:415-419) so a later definition of the same name
  does not meet CHECKER-DUP-DEFINITION. The shape to copy is
  src/core/generated-declaration-dictionary.f ROLLBACK (lines 74-81), driven on the
  throw path by src/core/declaration-transaction.f:332-335.

  THE INVARIANT THAT MUST FAIL CLOSED. A held record occupies slot ndict@, which
  the next definition would overwrite. So a held definition must be committed or
  discarded before anything else defines, and a second hold while one is live is
  refused by name — the same rule migrate.f already enforces for a migration inside
  a migration (migrate.f:669).

SCOPE — TWO QUESTIONS FOR THE ORCHESTRATOR BEFORE PHASE 2

  1. FIXPOINT HASH. The design changes habu2.f, so bin/hb changes. The fixpoint
     PROPERTY (stage N == stage N+1, self-hosting byte-identical) must hold and
     codegen-compare must stay at 0 findings, but the resulting binary cannot hash
     equal to master's. "Fixpoint byte-identical on the default path" has to mean
     the property, not the hash. Confirm.
  2. NOT NEEDED, DELIBERATELY. No new layout.f DATA cell, no new primitive, no
     checker axiom row, no snapshot-format change, no AOT seed change. If the
     reviewer expected NPUB record CREATION, the measurement above is the argument
     against it: `:` is the record constructor and the publication is only
     count+index+facts.

ADDENDUM — WHAT THE HELD EXIT MUST DO, AND WHY IT IS NOT THE REJECT EXIT

Working the change through to the instruction, the `held` exit turns out to be a
near-copy of the `rejected` leg. It is worth writing down exactly where the two
differ, because the tempting shortcut — "hold is just a reject the Habu side
remembers" — silently destroys the record's own name.

  WHAT HELD SHARES WITH REJECTED. Both skip the publish label, so neither moves
  the count, the index or the checker facts; both fall into the shared `finish`
  tail, which clears PEND-CELL and leaves compile mode. And both should give the
  emission's code space back — the chain is about to compile the same definition,
  and the old emitter's bytes are dead either way.

  WHERE THEY MUST DIFFER. A name longer than DNAME-INL (16, layout.f:121) is not
  stored in the record. C-STORE-NAME (habu2.f:2124-2135) sets DNAME-EXT, writes the
  name bytes into CODE space, and records the PRE-NAME code pointer in the record
  at offset 24. The `rejected` leg rewinds CP to that pre-name pointer
  (habu2.f:6238-6239) — correct for a reject, which is throwing the whole record
  away, and fatal for a hold, which is keeping it: the chain's routine would be
  emitted straight over the bytes that spell the held word's name.

  SO THE HELD EXIT REWINDS TO THE COLON ENTRY, record cell 0, in both the inline
  and the ext case — one leg, no DNAME-EXT test. That erases every instruction the
  old emitter wrote and stops immediately above the name.

  AND THAT MAKES THE COMMIT ALMOST NOTHING. After the rewind, cp@ equals the held
  record's start cell, which is where NPUB:NEXT-SLOT will answer and where the
  emission will be placed. The record already points at its own new code. The
  commit therefore writes the LENGTH cell, publishes the count, and applies the
  checker's wide/min-in facts — it never has to touch the start cell, and the
  "no two publications claim one slot" line in publish.f:367 is satisfied by the
  reclamation rather than bypassed.

  WHERE THE TEST GOES IN THE TAIL. Immediately after the existing
  `10 G-POP  10 rejected CBZ,` (habu2.f:6232) and BEFORE LOWER-TXN:FREEZE and
  EM-P2-TRIGGER. A held definition therefore never enters the width-aware pass-2
  re-run, which is right — its emission is being discarded, so re-emitting it
  more carefully is wasted work, and the chain's IR has no aggregate kinds to
  compile a wide effect into anyway (the cut audit's blocker 2). Skipping FREEZE
  leaks nothing: TXN-ACTIVE-CELL is set in exactly one place (habu2.f:6103) and
  the trigger follows it in the same instruction stream, so it is never left set
  across definitions.

  WHERE THE SHARED CONSTANT LIVES. src/habu/layout.f. It is loaded both by the
  build that emits the engine (tools/bootstrap.sh:77) and into the running image
  (tools/srclist.f:75), so habu2.f's comparison and check-hook.f's return read one
  definition rather than two numbers that must agree.

  THE VALUE ITSELF. Today's hook returns -1 or throws (check-hook.f:29-41); the
  engine's only test is non-zero (commit) versus zero (reject). The hold code is a
  third value in a domain that currently has two, so it costs one CMP and one
  conditional branch on a register the tail is already holding.
