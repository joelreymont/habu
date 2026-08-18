---
title: Parse a migrated definition from the input stream
status: active
priority: 2
issue-type: task
created-at: "\"2026-08-01T18:47:50.699022+02:00\""
---

src/compiler/native/migrate.f takes a definition's SOURCE TEXT as a string because the tape recorder's unit has to be opened before the definition and closed after it, and one word has to do both or the tape's IR context dies between them. That works and is honest - evaluate is the engine's own interpret path - but it means a migrated definition is written inside an s" literal instead of at top level, so it is not indented, not highlighted, and not diffed like the rest of the source. Give the migration a definer that reads the definition out of the input stream instead: something shaped like ': NAME ( .. ) body ;' preceded or followed by the migration's arities, where the definer arms the recorder, lets the engine's own parser consume the definition, and runs the chain at the close. Nothing else in migrate.f changes when it lands: the tape, the elaboration, the emission and the publication are already independent of where the text came from. See the WHY THE SOURCE IS HANDED OVER header note in src/compiler/native/migrate.f, and tools/codegen-compare-migrated.f, which is the file that would read best after the change.

Claim: agent=thecut-3 workspace=.jj-ws/habu-thecut

SUPERSEDED (2026-08-18): the clause above asking for the definition to be
"preceded or followed by the migration's arities" no longer applies. Master
b4329129 made the arity the checker's - NMIGRATE:DEFINE takes source only and
KEEP-ARITY reads NDICT:SPELL-ARITY - so the stream definer states nothing at all
and takes nothing but the stream.

FOLLOW-UP: this lane rewrites tools/codegen-compare-migrated.f only. The other
codegen-compare-migrated*.f corpora still build their sources as s" literals and
can move to the stream definer separately.

CELL-BUMP-N STAYS ON THE STRING ENTRY. It is the one row of that corpus that
names a data word, and NMIGRATE:DEFINE-DATA takes that spelling as an argument;
a second stream entry carrying one argument for one call site is ceremony. The
row moves to NMIGRATE:NEXT with no argument at all when the data word's address
becomes derivable from the tape, which is the capability family that owns "let a
migrated body name every constant and callee it mentions"
(habu-let-a-migrated-77d34d82).

THE BOUNDARY THIS LANE OPENED: src/compiler/native/input.f reaches the engine's
interpret cursor through two TRUSTED: rows, because the checker cannot express a
pointer field at a fixed DATA-header offset. Dot
habu-model-the-interpreter-f450db18 owns the modeled cursor that deletes them.
