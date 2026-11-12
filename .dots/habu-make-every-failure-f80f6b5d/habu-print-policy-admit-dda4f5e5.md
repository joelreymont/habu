---
title: "Print policy-admit's refusals as one line"
status: open
priority: 2
issue-type: task
created-at: "2026-11-12T18:16:44.190440+02:00"
---

Problem: policy-admit (src/habu/habu1.f BPOLICYADMIT :4536, :4558, rc 107) writes 'E-POLICY habu: policy: no package <name>' and '...package <name> publishes keyword <kw>' in several writes with no lr, off the shape `<class> habu: <text>[ <16 hex>] lr <16 hex>`; the names are runtime parts, which crash.f DIE-LINE, cannot hold. Acceptance: each refusal is one line with its lr, through an emitter that writes runtime parts and the lr as one write. Depends: none. Claim: unassigned.
