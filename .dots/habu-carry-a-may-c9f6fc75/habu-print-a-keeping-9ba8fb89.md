---
title: "Print a keeping's fact record only where a keeping lacks the fact"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-18T10:21:52.641941+01:00"
closed-at: "2026-11-18T11:24:41.924746+01:00"
---

Problem: src/core/checker.f QUOT-PAIR sets FACT-HIT (:4855-4857) for invariant and flipped (ROLE-NEG) pairs, but src/core/render.f :2155 always tells the reader to state the fact in the type that keeps the quotation. A join `if ['] K1 else ['] K2 then` with K1 rendering prints that record at `then`, where nothing keeps, and a plain mismatch in the other branch order; storing a quotation typed `[ n [ n n -- ] -- nominal ]` into a cell typed `[ n [ n n -- nominal ] -- nominal ]` says to state `nominal` in the cell, which already states it, where the fix is the stored quotation's input. Acceptance: each case prints a record whose help names what the reader changes, the same in either branch order. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-nC.
