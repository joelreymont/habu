---
title: "Stdlib: checked JSON parser (lib/json-read.f)"
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T10:46:12.108937+02:00"
---

Complement lib/json-write.f (emit-only) with a checked JSON reader. v1 = zero-allocation pull/cursor parser mirroring the writer's streaming style: JR-INIT ( ptr u8 n -- ) over a source buffer; JR-NEXT ( -- n ) returns typed token kinds (named constants: object-begin/end, array-begin/end, key, string, number, true/false/null) with span accessors JR-SPAN$ ( -- ptr u8 n ); JR-STR ( ptr a n -- n ) unescapes the current string token into a caller buffer (all escapes: quote backslash slash b f n r t, \uXXXX incl. surrogate pairs -> UTF-8); numbers via STR>NUMBER?/STR>FLOAT split by JR-INT?/JR-FLOAT kind. Skip/enter helpers: JR-SKIP-VALUE, JR-FIND-KEY ( ptr u8 n -- bool ) within the current object. Fail closed, named throws (lib/errors.f new block -3900..-3999): malformed token, bad escape, bad surrogate, depth cap exceeded, trailing garbage after the top value, number overflow. Depth tracked against a named cap. Tests: positive fixtures (nesting, all escapes, numbers incl. exponents/negatives, unicode), negative fixtures per throw, and a ROUND-TRIP against lib/json-write.f output (write a structure, parse it back, compare events). Wire: lib/std.manifest rows, FILEMAP.md, gate-stdlib-cases suite. Consumers unlocked: reading back repair packets; model config JSON (tokenizer/config.json) for the training workloads. Checked only - no TRUSTED.
