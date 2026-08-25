---
title: Expose live IR-BUILD table readers
status: open
priority: 1
issue-type: task
created-at: "2026-07-30T17:53:54.366739+02:00"
---

The straight-line HIR leaf found that IR-BUILD hands out no live reader for a module's tables while it is being built: every FSYM-*, FSCHEMA-* and friend needs a frozen module. Two consequences, both real. First, src/compiler/native/hir.f cannot check that the schema table it is registering into really belongs to its own dialect, because IR-SCHEMA:DIALECT@ needs the live rows arena; the dialect supplies its own name through HIR:NEW-BUILDER instead, which stops the ordinary path getting it wrong but is a usage rule rather than a structural check. Second, src/compiler/native/hir-word.f cannot ask the module's symbol interner whether a presented symbol was really interned, the way src/compiler/native/immediate.f does with IR-SYM:LEN@, so it checks module ownership only. Third and largest, src/compiler/native/tape.f needs the module's live IR-SOURCE registry and IR-SYM arenas to append a token, and an IR-BUILD module hides both, so a source tape and the HIR module built from it cannot today be two halves of one module. That last one blocks the elaborator, which has to hold both at once. Fix: add live readers to src/compiler/ir/build.f for the symbol pool and rows, the source registry and the schema pool and rows, with the same ownership checks the frozen readers make, then make HIR:REGISTER check the dialect name, make HIR-WORD's declarers check the interner, and drive NTAPE from an IR-BUILD module. Owner: src/compiler/ir/build.f, src/compiler/native/hir.f, src/compiler/native/hir-word.f.

Claim: agent=livereaders workspace=.jj-ws/habu-expose-live-ir-f0eaed6b (RELEASED 2026-08-21: workspace gone, no live lane - gc)

Landed: IR-BUILD now answers questions about a module still being built without
handing out any table. The readers are SYMBOL-CK (was this symbol interned),
SYMBOL-IS? (is it spelled these bytes), SPAN-CK (does this span lie inside the
source it names), DIALECT@, SCHEMA-MAJOR@ and SCHEMA-MINOR@. Each passes the
same ownership gate every append passes and answers with a scalar, an identity
or a refusal, so the builder is still the only route to a write. NTAPE gained
PUSH-INTO and PUSH-INTO-FROM, which append a token to the tape of a live module
by asking the builder those questions; a tape built that way passes NTAPE:CHECK
against the same module's frozen registries afterwards, which is the join the
elaborator needs. HIR:REGISTER now reads the schema table's own dialect name and
schema version back and refuses a table that is not this dialect's
(E-HIR-DIALECT, -8291). Every HIR-WORD declarer now proves the symbol it is
about to store was really interned, through the module's symbol rows or through
the builder, and the row appender takes a proof-carrying type so a later
declarer cannot skip that. Suites ir-build, native-tape and native-hir carry the
joined-module case and the refusal fixtures.
