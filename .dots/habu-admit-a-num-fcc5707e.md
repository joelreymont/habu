---
title: Admit a number-shaped name at tier 1
status: open
priority: 2
issue-type: task
created-at: "2026-10-10T07:05:16.993964+03:00"
---

Problem: docs/forth.md:144-161 and docs/forth-card.md:62-63: a number-shaped name loads under `--load` but is unreachable, since literals parse before lookup; only tools/check.f refuses it (E-NUMERIC-DEFINITION). At tier 1 native refuses the definition with an internal code and no diagnostic: ~/.cache/tmp/carl-gfrest/c/rg2.f (`: 123 ( -- n ) 1 ;  7 . cr`) prints `ncomp: cannot compile 123` and uncaught -8300 (E-NELAB-SHAPE, the tape's first token is not the defined name); rg1.f (`: 99999999999999999999 ( -- n ) 1 ;`, an over-bound shape) prints `ncomp: cannot compile ` with no name and uncaught -8405 (E-NFEED-LITERAL). Measured under the Habu loop (`bin/hb --load test/outer-loop-on.f test/gforth/tier-1.f <file>`). Tier 0 loads both (rg2 prints 7). The census of test/outer-interpret.f's refused cases (its `range` case) reaches it.
Acceptance: at tier 1, under both loops, a number-shaped definition name loads as at tier 0: the elaborator reads the tape's defined name as a name, never as a literal. rg1 and rg2 join test/outer-interpret.f.
Files: src/compiler/native/ (the tape reader that classifies the defined name), test/outer-interpret.f.
Verify: native build per docs/gate.md; `bin/hb --load test/outer-interpret.f`; `bin/hb --load test/run.f`.
Depends: none. Worker: worker-max.
Superseded: the one-pass codegen (docs/architecture.md, "The codegen is one pass over the checked events") deletes the tier-1 code this fixes; its reproducers become that codegen's cases. Do not start.
