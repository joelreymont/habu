---
title: Share streaming JSONL and exact integers
status: open
priority: 1
issue-type: task
created-at: "2026-10-14T17:54:15.355371+03:00"
---

Joel request relayed by Otto in /tmp/otto-jsonl/README.md: one src/lib JSONL library consolidating tools/json-file.f JSONF framing and Dots main.f streaming JR-based import. Read8KiB chunks, LF framing, empty lines/final unterminated line; caller line-size limit and failure cleanup. Provide exact signed-i64 integral decimal/exponent parsing without float rounding:1e2,100.0,1.5e1 accepted;1.05e1 absent; exact bounds incl i64min. Existing Dots numeric-string/underscore parity must be expressible through generic parsing APIs without moving Dots priority policy into core. Source/reproducer/test generator in /tmp/otto-jsonl; existing Dots owns its eventual consumer migration, Habu owns tools users. Inspect existing library/tool tests, write meaningful real load/file E2Es before code, consolidate duplicated parser/framing implementations, document precise public API, independent Astra review, focused affected gates and install matching qualified public release. No new DOM in library, second source parser, compressed payload or application-project edits.
