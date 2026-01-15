---
title: Update reader for package prefixes
status: open
priority: 2
issue-type: task
created-at: "2026-01-15T20:42:39.722743+02:00"
---

src/reader/parser.zig: Parse package:symbol and package::symbol syntax. Lookup package, intern in correct package. Dependencies: habu-implement-defpackage-macro-8c26986f. Verify: cl:car reads as COMMON-LISP::CAR.
