---
title: Add using to the stage0 recovery compiler
status: open
priority: 2
issue-type: task
created-at: "2026-07-29T22:50:34.706774+02:00"
---

Full context: capability gap proven by agent pkglayout 2026-07-30. The Gforth recovery host bakes a keyword table into its stage0 engine (bootstrap/cg/forth.fs:2475-2478) that declares package, public, private, ;package but NOT using/;using; LKWUSING and C-USING exist only in src/habu/habu2.f. tools/bootstrap.sh concatenates src/habu/layout.f with habu1.f/habu2.f/jit.f/prof.f/regalloc.f/xref.f into one source interpreted by stage0, so any using line in those files breaks no-binary recovery at the first hop. Stage0 DOES understand qualified NAME:WORD (forth.fs:1379-1380, 3043). Implement using and ;using in the stage0 keyword table and compiler with the same semantics habu2.f gives them, plus a recovery-path test (HABU_BOOTSTRAP_CHECK_ONLY=1 tools/bootstrap.sh green on a source containing using). This unblocks packaging src/habu/layout.f without requalifying ~2500 bare references. Prerequisite for finishing habu-give-layout-f-315df2ca.
