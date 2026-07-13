---
title: "checker: typed top level (execute-laundering + immediate residuals)"
status: open
priority: 2
issue-type: task
created-at: "2026-07-13T09:22:21.172397+02:00"
---

Principled endpoint named by the FOO2 dot (landed 3329ca69) for the residuals its interpret-path guard cannot reach: (1) `' W execute` / run-in-stack cannot see W's min-in through the xt - a certified word executed via xt at underdepth still reads below base (execute itself is LARITY-guarded min 1, but the TARGET's arity is invisible); (2) compile-mode immediate words (EM-COMPILE-CALL BLR) carry no depth guard - immediates mid-compile remain an unchecked boundary; (3) depth-satisfied garbage values (0 0 catch) are value-typing, out of depth-guard scope. The principled fix is a checker-modeled typed top level: the interpreter accumulates a checked row like a body, every certified word application unifies against it, and xt execution carries the target effect (xt values become typed `xt<effect>`). Large design; interacts with REPL ergonomics and the eval harness's top-level probing idiom. Evidence: FOO2 review + underdepth-gate probes, 2026-07-13. Design dot first; type-system lane.
