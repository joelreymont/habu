---
title: Name the type and the place when a type is declared twice
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T18:32:27.968011+02:00"
---

Problem: a source that declares a DEFLINEAR type twice (a file included twice: 'include lb-own.f' twice, lb-own.f holding 'DEFLINEAR LB:tok') is refused with 'checker: bad or duplicate signature type' and, in a build, 'hb-build: <path> ended the process during the build' (rc 74): the line has no error code, no type name, no file and no line, and the checker ends the process instead of throwing. Seen with the linear-fix engine (carl-deflin kukntvwk, test/linear-suite.f's 'a type declared twice' case, which asserts that line for want of a better one). Whether master and the other type definers (DEFTYPE, NEWTYPE) answer the same is not checked. Acceptance: the refusal names the type, the file and line of the second declaration and carries a code like the checker's other refusals; the build reports a throw, not an ended process; linear-suite.f asserts the code and the name. Files: src/core/checker.f (grep 'bad or duplicate signature type'), test/linear-suite.f. Verify: bin/hb --load test/run.f -- linear. Depends: habu-make-a-linear-953febee (lands first; same file). Ownership: carl. Claim: unassigned.
