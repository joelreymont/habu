---
title: Report interpreter, loader and compiler rejections in the one form
status: closed
priority: 2
issue-type: task
created-at: "2026-11-09T15:21:46.461017+02:00"
closed-at: "2026-11-16T04:07:34.209504+01:00"
---

Problem: the interpreter's, loader's, linker's and compiler's rejections print plain text through ERROR-TEXT:PREFIX whatever --json-errors set, with a line but no column or source line: src/habu/interpreter.f REFUSAL and REFUSE (61 sites, UNDEFINED, SEALED-FAIL), src/core/include.f RESIDUE-REFUSE (a file that leaves cells ends `uncaught throw code -3804`) and its `include: cannot open` lines, src/habu/main.f, src/habu/linker.f, src/compiler/native/definition.f TARGET-ERROR and REPORT-UNDEFINED, src/core/check-hook.f EFFECT-ARG and NAMED-FAMILY. A Wasm build of a program requiring src/lib/fs.f under --json-errors prints the prose E-SEALED-PACKAGE line and exits 84. Which definition.f REPORT-FAILURE calls are a compiler rejection's only report (E-T0-*, TRUSTED: bodies) is not yet known. Acceptance: each prints one diagnostic in the campaign's form through REJECTION (src/core/rejection.f), JSON only under --json-errors, with a specific help; the residue rejection names the leftover cells' producer; ERROR-TEXT:PREFIX is deleted with its last caller. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
