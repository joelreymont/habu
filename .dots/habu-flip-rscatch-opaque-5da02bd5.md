---
title: Flip RSCATCH opaque-xt branch to reject like execute
status: active
priority: 2
issue-type: task
created-at: "2026-07-19T01:08:31.450491+02:00"
---

Sibling soundness hole to the landed RSEXEC flip (dot habu-checker-exec-of-5923c543, commit 038476cd): RSCATCH's T-VAR branch still models 'variable V ... V @ catch' by binding the opaque xt to a benign quotation, so catch launders an unknown-effect xt past the checker exactly the way execute used to. Fix: flip RSCATCH's T-VAR branch to the same E-EXEC-OPAQUE-XT reject (reuse EXEC-OPAQUE/MD-EXEC-OPAQUE or mint a catch-specific reason naming the 'catch' token), sweep src/lib/tools/maki for checked 'V @ catch' firers and migrate any hits to defer/typed xt cells the slice-2 way, add red-first negatives mirroring the execute ones (raw-variable catch launder + a definer variant through catch) in test/xt-cell-test.f / test/internal-word-gate.f, and keep typed-route catch (quotation param, defer, xt<effect> cell, direct tick) green. Seed-affecting: fixpoint x2 + cold battery. Depends on habu-checker-exec-of-5923c543 landing.

Claim: agent=ptf-opus workspace=.jj-ws/habu-flip-rscatch-opaque-5da02bd5
