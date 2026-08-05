---
title: Parse a migrated definition from the input stream
status: open
priority: 2
issue-type: task
created-at: "2026-08-01T18:47:50.699022+02:00"
---

src/compiler/native/migrate.f takes a definition's SOURCE TEXT as a string because the tape recorder's unit has to be opened before the definition and closed after it, and one word has to do both or the tape's IR context dies between them. That works and is honest - evaluate is the engine's own interpret path - but it means a migrated definition is written inside an s" literal instead of at top level, so it is not indented, not highlighted, and not diffed like the rest of the source. Give the migration a definer that reads the definition out of the input stream instead: something shaped like ': NAME ( .. ) body ;' preceded or followed by the migration's arities, where the definer arms the recorder, lets the engine's own parser consume the definition, and runs the chain at the close. Nothing else in migrate.f changes when it lands: the tape, the elaboration, the emission and the publication are already independent of where the text came from. See the WHY THE SOURCE IS HANDED OVER header note in src/compiler/native/migrate.f, and tools/codegen-compare-migrated.f, which is the file that would read best after the change.
