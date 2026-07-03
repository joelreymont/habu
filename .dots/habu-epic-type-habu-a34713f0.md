---
title: "EPIC: type habu ground-up, retire TRUSTED"
status: open
priority: 2
issue-type: task
created-at: "2026-07-01T22:46:48.554565+02:00"
---

Goal: eliminate TRUSTED:/TRUST as a category. TCB today: EMIT-HOST-LOAD-PREFIX loads util/structures/checker/render with HOOK-CELL=0 (src/habu/habu2.f:412-415), 226 TRUSTED: defs + ~307 TRUST rows repo-wide. Strategy: (1) staged fixpoint checking - stage N binary CHECKS stage N+1 source (incl checker.f, render.f, builder habu1/habu2/jit) before building it, so nothing in SOURCE is unchecked (habu-self-check-checker-e10ce327 is the first rung); (2) convert TRUST rows on builder emit words to real CHECKED: definitions - reg/label/asm roles already exist for this; (3) discharge each TRUSTED: via a new checker capability (depth introspection typing, records, linear resources) or rewrite as checked code; (4) shrink the irreducible axioms to a single audited primitive-effect table with per-primitive differential tests. Irreducible remainder: primitive axioms + the seed binary (Thompson trust) - document as the explicit trust root. Track per-class progress via habu-trusted-inventory dot.


## OWNERSHIP (2026-07-03): FULL HANDOFF to the second agent
The SECOND AGENT owns this entire retire-TRUSTED epic — BOTH halves:
- CHECKER half: src/core/checker.f, check-hook.f, roles.f, render.f,
  docs/effects.md, engine-suite regressions. Dots: typed-dictionary-record,
  typed-defining-words, single-pass-checking, multishot-quotations,
  checker-self-typing, ddc-cross-check, and discharging every checker-expressible
  TRUSTED/TRUST boundary as the capabilities land.
- BUILD half: bf-certify-stage, staged-fixpoint-src, checked-image-writers,
  builder-trust-rows — making the build pipeline itself checked. Territory:
  tools/build-fixpoint*, tools/hb-build*, image writers (src/os/image-bytes/
  macho/elf, tools/object-image), aot-lib.

SEQUENCING (BLOCKING): src/habu/habu2.f and tools/build-fixpoint* are CURRENTLY
LIVE under the main orchestrator's AOT-REPL worker (compiling the REPL into
bin/hb — a NON-epic feature). The second agent may start the CHECKER-half rungs
NOW (src/core/checker.f is free), but must WAIT on any rung that edits habu2.f or
build-fixpoint* until AOT-REPL lands; the main orchestrator will signal
"engine free". Until then, run the maki-autograd epic (fully disjoint, ready now)
and the checker.f rungs.

IN-FLIGHT DONE WORK being merged by the main orchestrator (do NOT redo): the
linear-once + linear-kind soundness fixes (checker.f execute-path + polymorphic
kind discipline), the derived-ceiling conflict-free trust accounting
(trusted-inventory.f), and the prim-axiom re-own. These land on master as your
clean base — the derived-ceiling format makes concurrent TRUSTED.md row additions
merge without conflict.

MAIN ORCHESTRATOR retains ONLY: AOT-REPL (compiled REPL/debugger), integration/
merging, and non-epic dots. It will not dispatch new checker/epic workers.
BOUNDARY: second agent stays OUT of src/habu/habu2.f and (until "engine free")
tools/build-fixpoint*; main orchestrator stays OUT of lib/ptx*/maki/* and
src/core/checker.f. TRUSTED.md is shared — the derived-ceiling format keeps
concurrent row additions conflict-free.
