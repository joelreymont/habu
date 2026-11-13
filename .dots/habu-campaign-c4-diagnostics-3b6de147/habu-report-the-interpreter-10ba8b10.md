---
title: "Report the interpreter's refusals as JSON under --json-errors"
status: open
priority: 2
issue-type: task
created-at: "2026-11-09T15:21:46.461017+02:00"
---

Problem: the located refusals print plain text through ERROR-TEXT:PREFIX and ERROR-TEXT:TYPE whatever DIAG-JSON! (src/core/checker.f:5063) set: src/habu/interpreter.f REFUSAL (:330) and its callers (UNDEFINED-NAME :335, SEALED-FAIL :370), src/compiler/native/definition.f TARGET-ERROR (:457), every src/core/include.f `INCLUDE-IO-RC die` line, and include.f ?TARGET's E-TARGET line. tools/hb-build-lib.f:512-513 says that under --json-errors stderr holds only JSON, but a Wasm build of a program requiring src/lib/fs.f under --json-errors prints the prose E-SEALED-PACKAGE line and exits 84. Only src/core/render.f DIAG-JSON (:1896) honours the flag. Acceptance: with DIAG-JSON! true every located refusal prints one JSON diagnostic in DIAG-JSON's shape (code, message, token, file, line) and no text line; without it the text stays. Verify: an hb-build test case runs --json-errors on a source with no MAIN, on one using an undefined word and on one reopening a sealed package, and reads one JSON object each. Depends: none. Claim: unassigned.
