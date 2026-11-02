---
title: "wide: compile a definition whose effect is too wide to record"
status: closed
priority: 2
issue-type: task
created-at: "2026-10-30T18:56:34.568966+02:00"
closed-at: "2026-11-02T11:33:14.642596+02:00"
close-reason: "Landed e1016d24d0 'Compile a definition whose effect is unrecorded'."
---

Gap (diag-position F216/F228): REC-SIG records nothing past 23 type variables, SPELL-ARITY answers none, KEEP-ARITY throws uncaught E-NCOMP-ARITY at both native tiers; the JIT loads it. Brief /tmp/jerry-big/t0-wide-brief.md. Base mktzwnmu. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-wide
