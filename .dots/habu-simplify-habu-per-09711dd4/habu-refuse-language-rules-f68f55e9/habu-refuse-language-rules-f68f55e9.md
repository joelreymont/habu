---
title: Refuse language rules at the definer
status: open
priority: 2
issue-type: task
created-at: "2026-10-25T22:57:08.868257+02:00"
---

Rule: docs/architecture.md:318-321, the definer refuses a reserved name, a broad TRUSTED: and a global that shadows a primitive; whole-program facts (error codes used) come from walking the dictionary after the program loads. Problem: these are text-scanning tools instead: tools/reserved-name-lint-core.f (427 lines, RNL-RESERVED-* ~132-216; E-RESERVED-DEFINITION is raised only under tools/; on aef1150f a package defining ': variable ( -- ) ;' is E-RESERVED-DEFINITION under tools/check.f, rc 1, while the load accepts it, rc 0), tools/lint/shadow-lint.f (241 lines, tokenizes habu1.f for primitive names); and for whole-program facts tools/error-code-lint-core.f (583 lines, scans .f files for 'constant E-*'). Acceptance: the definer refuses reserved, number-shaped and primitive-shadowing names and broad TRUSTED: with located errors; error codes come from a dictionary walk after load; the three text scanners and their tests are deleted. Related: habu-lint-tools-use-cbb3ffbf (superseded where it touches these files). Verify: rejected-program cases through the real load path, test/run.f.
