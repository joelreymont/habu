# JSON tooling

Tools use the same JSON libraries as Habu programs:

- [json-read.f](../src/lib/json-read.f) is a caller-owned pull reader. Allocate
  JR:STORAGE-BYTES at a cell-aligned address, call JR:INIT with the source
  span and advance with JR:NEXT. JR:FINISH validates the rest of a document
  and consumes its reader; JR:CLOSE abandons it without reading further.
  JR:FIND-KEY finds the first matching member of the current object.
  Reopen a reader on a retained row span when fields are needed out of order.
  JR:STR decodes a string into the caller's byte buffer; JR:SPAN$ retains
  the original number lexeme. JR:FINISH validates the complete document,
  including fields the tool does not use. Reader failures have named
  E-JR-* codes; E-JR-DEPTH is a capacity failure.
- [jsonl.f](../src/lib/jsonl.f) frames physical lines. JSONL:SPLIT-LINE
  operates on an in-memory span, including a final line without LF.
  JSONL:OPEN, JSONL:NEXT and JSONL:READER stream a file with a line limit.
  Consume its live reader with JR:CLOSE or JR:FINISH before another reader,
  JSONL:NEXT or JSONL:CLOSE.
  Each tool decides whether blank, malformed or non-object rows are errors or
  skipped rows.
- [json-write.f](../src/lib/json-write.f) emits compact JSON into a
  caller-owned fixed buffer or growable BUF. It escapes strings and checks
  capacity; callers decide object shape and field order.
- [json-copy.f](../src/lib/json-copy.f) copies the reader's current value into
  an existing writer. JSON-COPY:VALUE takes a JR:reader, a writer and caller-owned
  decode scratch at least as long as the source value. It returns the reader
  at the end of that value and the writer. It emits compact JSON, retaining
  member order and numeric lexemes.

For example, repair-packet skips blank, malformed and non-object diagnostic
rows, but propagates reader depth failures. diag-to-sarif requires each
nonblank row to be an object and validates it before using any field.
