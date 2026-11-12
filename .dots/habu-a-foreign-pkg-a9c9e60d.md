---
title: A foreign package forges or drops a linear token through a private pointer mint
status: active
priority: 2
issue-type: task
created-at: "2026-11-11T22:36:06.170443+02:00"
---

Problem: docs/forth-card.md:446-450 says only the package that declared a DEFLINEAR type mints or erases its token, but another package does both through memory. Cause (probes on the installed hb, 2026-11-12): a cell may be declared with a type that can hold a linear value (TYPED-VARIABLE HK [ -- Q:tok ], also [ -- a ] and [ R -- S ]), and the checker believes that type at every read whatever was written: the forge works with no cast (byte-view cell-view @ and !, or byte-copy), with one pointer cast, with one integer cast, and from a neighbour's address. src/core/checker.f STORAGE-CELL-W admits any quotation; only the arena's element type (ARENA-TERM-INFO) asks STORED-TERM-HIT. Fix: no storage holds a type that may hold a linear value: STORAGE-TERM-OK?, the one gate under every storage definer, takes the cast walk's answer and refuses with its own E-BAD-STORAGE reason (`stored type may hold a linear value`); storage's partial linear arms go; the card and docs/forth.md state the rule. Acceptance: the two probes (a foreign package forging a token that reaches the owner's CLOSE, and dropping one without the owner's ERASE) and their cast-free variants are refused at the cell's declaration with that reason; the owner's private mints, LINEAR: rows, a defer over a token and a stored [ n -- n ] still load; tools/check.f over Maki, Zen and Tender on the changed engine reports no new refusal. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-linear.
