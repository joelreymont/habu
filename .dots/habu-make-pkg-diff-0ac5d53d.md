---
title: Make package-diff-lint read core checker sources
status: open
priority: 1
issue-type: task
created-at: "2026-07-25T14:41:14.204844+02:00"
---

Why this is needed: package-diff-lint is a mandatory commit gate for every change that touches Forth source, but it cannot read the two largest core files at all. Running it on a real jj diff that touches src/core/checker.f or src/core/sumtype.f throws E-DIFF-SYNTAX (-7400, defined in tools/lint/diff-error.f) and exits 67 with no findings printed, so the package-ownership rule is never applied to either file. A worker who follows the commit gate on such a diff sees an opaque parse error, not a verdict.

Measured on master 79c50e5a9dbf, appending one plain global definition to a file and running `bin/hb --load tools/package-diff-lint.f -- <jj diff --git artifact>`:
- src/core/checker.f: exit 67, `package-diff-lint: threw -7400`, zero findings.
- src/core/sumtype.f: exit 67, `package-diff-lint: threw -7400`, zero findings.
- src/core/enums.f, src/core/structures.f, src/core/roles.f, src/core/type-family.f: exit 0, no finding, because GLOBAL-IMPLEMENTATION? in tools/package-diff-lint-core.f (line 613) allowlists them as core language surface.
- src/core/structure-decl.f, src/core/enum-decl.f, lib/json-read.f, lib/memory.f, tools/check-core.f: the new global is correctly reported as E-PACKAGE-OWNERSHIP.
The lint reads the real file from the working tree, not just the patch text: pointing a well-formed patch at a path that does not exist fails with a different code (-2101). So the parse failure comes from lexing the real contents of those two files.

Note that src/core/sumtype.f is also on the GLOBAL-IMPLEMENTATION? allowlist at line 200, which is dead code today: the parse fails long before the allowlist is consulted. Deciding whether that entry should survive is part of this work.

Owned result: package-diff-lint parses and evaluates src/core/checker.f and src/core/sumtype.f like any other Forth file. Root-cause the E-DIFF-SYNTAX first and name the construct that defeats the lint's definer grammar in tools/package-diff-lint-core.f; do not widen the allowlist, skip the files, or catch and downgrade the throw to make the gate quiet. If the construct is legitimate Forth the lexer must handle it; if the lint's grammar is right and the source is wrong, fix the source. Then decide explicitly whether src/core/checker.f and src/core/sumtype.f belong on the core-surface allowlist, and record the reason in the header comment next to the existing entries.

Acceptance and smallest owning check: a negative regression in tools/package-diff-lint-test.f built from the minimal construct that reproduces the throw, proving the lint reports a finding rather than throwing. Then, on a real diff artifact, a new global definition added to src/core/checker.f is either reported as E-PACKAGE-OWNERSHIP or admitted by a documented allowlist decision, and the same for src/core/sumtype.f, with no E-DIFF-SYNTAX in either case. The control files above keep their current verdicts unchanged. A mutation that restores the old parse behaviour must red the new regression.

Verify: bin/hb --load tools/package-diff-lint-test.f, the measured survey above rerun on the changed tree, typed-local-diff-lint and package-diff-lint on the exact diff, host-lint.

Files: tools/package-diff-lint-core.f, tools/package-diff-lint-test.f, and whichever source file the root cause implicates. Claim: unassigned.
