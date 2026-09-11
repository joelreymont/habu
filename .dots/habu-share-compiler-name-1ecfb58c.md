---
title: Share compiler name lookup
status: open
priority: 2
issue-type: task
created-at: "2026-07-19T22:20:33.372764+02:00"
---

Measured current-master native compiler bloat after ed6207a factored MATCH stencils: C-FIND-GLOBAL? at src/habu/habu2.f:1621-1629 still meta-expands roughly 64 bytes of package-WID save/clear, name materialization, LFIND call, and WID restore at each source call. Eleven production expansion sites remain, including five in the ADT/MATCH block at 6153, 6185, 6264, 6289, and 6312. C-FIND-CHECKER at 1641-1647 expands five more sites and, on a required-service miss, calls C-FIND-GLOBAL after C-FIND-GLOBAL? already failed, repeating the entire lookup solely to reach diagnostics. Emit one internal runtime global-lookup helper taking the name in the documented registers, preserving x11/x13/LR and package WIDs, plus one shared fail-closed named-service diagnostic tail; make all 16 sites materialize the name and BL once, and make checker-service miss jump directly to the diagnostic without a second LFIND. Preserve lookup precedence, returned xt/flags, exact missing-name diagnostics/exit, W^X transitions, checker bridge behavior, bootstrap mirror, AOT, fixpoint, and every caller. Prove disassembly and an exact byte ledger for every old/new site, zero repeated save/clear/restore stencils, zero duplicate miss lookup, compiler region and macOS/Linux CODELEN reduction, lookup hit/miss/package restoration regressions, fresh fixpoint, and full native gates. Coordinate the landed MATCH factor commit ed6207a and native machine-helper ABI/schema owners; do not re-expand a macro wrapper around the shared body.
