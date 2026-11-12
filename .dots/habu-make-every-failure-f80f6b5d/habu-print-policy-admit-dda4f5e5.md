---
title: "Print policy-admit's refusals as one line"
status: open
priority: 2
issue-type: task
created-at: "2026-11-12T18:16:44.190440+02:00"
---

Problem: policy-admit (src/habu/habu1.f BPOLICYADMIT :4536, :4558, rc 107) writes 'E-POLICY habu: policy: no package <name>' and '...package <name> publishes keyword <kw>' in several writes with no lr, off the shape `<class> habu: <text>[ <16 hex>] lr <16 hex>`; the names are runtime parts, which crash.f DIE-LINE, cannot hold. Acceptance: each refusal prints one line in the shape, its runtime parts written in pieces as every line holding them is, ending with its lr. Depends: none. Claim: unassigned.
