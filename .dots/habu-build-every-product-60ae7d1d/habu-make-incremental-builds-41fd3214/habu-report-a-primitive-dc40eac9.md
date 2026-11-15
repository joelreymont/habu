---
title: "Report a primitive's use on a linker-built hb"
status: open
priority: 2
issue-type: task
created-at: "2026-11-09T18:26:26.917143+02:00"
---

Problem: a linked hb's own words (script-argc, script-arg-start; lsp-boundary definition-primitive steps 8-13) have records but no source location: src/habu/engine-origin.f TARGET, SYMBOL-AT and EACH-IN answer not found because no record carries a source cell. So src/habu/verify-source.f ENGINE-ORIGIN-USE (:2836) never records the engine file, the LSP server never republishes it on open (tools/lsp-core.f, tools/lsp-defs.f), and definition-engine-open (a 64 s timeout), definition-primitive ids 8-11, definition-prefix-origin, definition-sumtype, references-source-backed-builtin and references-source-backed-other fail. lsp-boundary is off behind `false [if]` in test/gate-stdlib-cases.f:17. The approved direction is a source cell on every record (XREF:SOURCE, SOURCE-FILE:PATH$ and FIND); no linker-era design has started. Whether any case has a second cause shows only once origins are carried. Acceptance: records carry their source, ENGINE-ORIGIN answers from them, the `false [if]` around lsp-boundary is deleted and lsp-boundary passes on the linker-built hb. Depends: a linker-era design. Claim: unassigned, owner carl.
