---
title: Let catch take a quotation that changes the stack
status: open
priority: 2
issue-type: task
created-at: "2026-11-23T11:28:36.715173+01:00"
---

Problem: catch admits only a stack-preserving quotation (src/core/checker.f:7001 RSCATCH), so a site that wants a call's result or its failure needs a typed none, a preserving body and a dead arm: Maki src/project.f:1209-1229 and src/element.f:608-628 (tim, item 6), src/lib/json-rpc.f READ-IN, and 25 placeholder sites in Habu. Acceptance: [: JSON:PARSE ;] catch certifies; its outputs are readable only where that catch's code is 0 (E-STALE-READ elsewhere); a throw leaves the output depth on arm64, x86-64 and Wasm (tier 0 fills the difference after the call); same-shape catches keep today's rule; design.md, the card and forth.md state the new rule; the placeholder sites drop their placeholders. Depends: the computed-0 throw dot. Claim: agent=carl workspace=none yet.
