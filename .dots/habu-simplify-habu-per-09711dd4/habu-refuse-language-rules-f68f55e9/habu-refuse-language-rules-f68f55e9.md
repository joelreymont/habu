---
title: Refuse language rules at the definer
status: open
priority: 2
issue-type: task
created-at: "2026-10-25T22:57:08.868257+02:00"
---

Rule: docs/architecture.md:318-321, the definer refuses a reserved name, a broad TRUSTED: and a global that shadows a primitive; whole-program facts (error codes used) come from walking the dictionary after the program loads. Problem: these are text-scanning tools instead: tools/reserved-name-lint-core.f (427 lines, RNL-RESERVED-* ~132-216; E-RESERVED-DEFINITION is raised only under tools/; on aef1150f a package defining ': variable ( -- ) ;' is E-RESERVED-DEFINITION under tools/check.f, rc 1, while the load accepts it, rc 0), tools/lint/shadow-lint.f (241 lines, tokenizes habu1.f for primitive names); and for whole-program facts tools/error-code-lint-core.f (583 lines, scans .f files for 'constant E-*'). Acceptance: the definer refuses reserved, number-shaped and primitive-shadowing names and broad TRUSTED: with located errors; error codes come from a dictionary walk after load; the three text scanners and their tests are deleted. Related: habu-lint-tools-use-cbb3ffbf (superseded where it touches these files). Verify: rejected-program cases through the real load path, test/run.f.

Acceptance case: with no vocabulary policy the load admits a definition named after an interpreter keyword and never calls it, where tools/check.f rejects it E-RESERVED-DEFINITION at the definition. `: CONSTANT ( -- n ) 7 ;` then `CONSTANT . cr` runs the keyword (E-DUPLICATE-DEFINITION at `.`); `: PACKAGE ( -- n ) 7 ;` then `PACKAGE . cr` opens a package `.` and exits 0 with no output. src/habu/interpreter.f DEF-WALL refuses a design keyword only under a policy (POLICY-NDICT-CELL). Both loaders reject both definitions with one record.

Acceptance case: in a sealed package (`package P` / `private` / `: HID ( -- n ) 9 ;` / `seal-package`), `: 42 ( -- n ) 1 ;` is rejected E-SEALED-PACKAGE by the load and E-NUMERIC-DEFINITION by tools/check.f; both print one record. In the same package, `CALLBACK:` or `DEFTYPE` with `;package` on the next line takes `;package` as its name, and both loaders print "`;package` would change a sealed package", whose help does not name the missing name.
