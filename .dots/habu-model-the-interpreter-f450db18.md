---
title: "Model the interpreter's input cursor for the checker"
status: open
priority: 2
issue-type: task
created-at: "2026-08-18T23:38:51.648139+02:00"
---

src/compiler/native/input.f (package NINP) reads and writes the engine's own interpret cursor through two TRUSTED: rows - INP-FIELD and INE-FIELD - because the cells are raw byte offsets into the DATA header (src/habu/layout.f INP-CELL / INE-CELL) and the checker has no way to express 'the pointer field at a fixed header offset'. Every other engine fact the native chain depends on comes through a modeled primitive with a checker axiom (cp@, ndict@, data-base, code-publish, xref-retarget); the cursor is the one that does not, so NINP states it as a named boundary instead. Give the checker a modeled input-stream cursor - either a primitive pair with PRIM: axioms (a reader for the stream tail and a writer that moves the cursor forward inside it) or a modeled byte-offset header field - and delete both TRUSTED: rows in src/compiler/native/input.f, which are the only reason that file has any. The consumer is NMIGRATE:NEXT (dot habu-parse-a-migrated-b38a83d9): it hands the stream tail to evaluate, the checker's tape reader stops that stream where the definition ended, and the interpreter is put back one byte past the ';'. test/compiler/native-stream.f measures that landing byte for byte and is the regression that must stay green through the change.
