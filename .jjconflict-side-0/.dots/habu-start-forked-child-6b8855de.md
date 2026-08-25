---
title: Start forked child at global scope
status: open
priority: 2
issue-type: task
created-at: "2026-07-27T17:24:51.999634+02:00"
---

Engine seam found during internal-word-gate packaging. A SUBJECT fork inherits the parent's open package scope, so a forked child that opens its own package is treated as opening a NESTED package and dies with exit status 75 and the bare diagnostic 'package'.

DECIDED 2026-07-27 (orchestrator): fresh global scope. A fork whose purpose is to be a fresh top-level interpreter starts at global scope, and package scope is not inherited across the fork boundary. The alternative - keeping inheritance and improving the refusal diagnostic - is rejected: a child process that is meant to start clean should start clean, and no caller has a reason to want the parent's half-open package.

EVIDENCE LOCATION, CORRECTED BY MEASUREMENT. The earlier text cited lane commit 445aa258 and said a two-sided probe was recorded in the header of test/internal-word-gate.f. Measured on master cd743607, that probe is NOT in the file's header. What the landed internal-word-gate work did keep is the one-line consequence at test/internal-word-gate.f:757-758, immediately above the ACTION word: "ACTION returns a checked quotation for private MAIN before the package closes; execute it globally after the close so SUBJECT forks inherit no open package." That comment, together with the ACTION and ;package arrangement on the lines below it, is the landed witness that the inheritance is real and that the suite is currently working around it. The two-sided probe itself - fork with a package still open exits 75, fork after ;package exits 0 - is not in the tree and has to be rebuilt as part of this leaf; do not go looking for it.

Owned result: change the engine's fork path so a forked child begins with no open package, whatever the parent's scope was at the moment of the fork. Rebuild the two-sided probe as the regression, running it through the real fork path rather than a simulation of it, and keep it as a permanent case so the inheritance cannot come back silently.

Out of scope, deliberately: the bare 'package' token that the nested-package refusal prints today is undebuggable on its own, but with fresh global scope a forked child no longer reaches that refusal, so improving that diagnostic is a separate concern and does not belong here.

Acceptance: the rebuilt probe shows a forked child opening its own package now exits 0 where it previously exited 75; the same probe still shows a genuinely nested package opening inside one process refused; the internal-word-gate suite stays green through its exact owning bin/hb --load path; both diff lints pass. Not on the critical path; schedule after the unified type cutover.
