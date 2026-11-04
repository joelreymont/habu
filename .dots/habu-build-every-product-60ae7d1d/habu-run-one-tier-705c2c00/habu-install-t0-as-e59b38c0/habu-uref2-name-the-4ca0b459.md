---
title: "uref2: name the definition an undefined call refuses"
status: closed
priority: 2
issue-type: task
created-at: "2026-10-30T18:41:06.941318+02:00"
closed-at: "2026-11-02T11:33:14.609317+02:00"
close-reason: "Landed, squashed into c155218ab6 'Print where a native refusal stopped': definition.f:553-554 prints REPORT-FAILURE before REPORT-UNDEFINED; elaborate.f:2325 refuses on RESOLVE-SITE's answer; prim-owner-scope.f:211 and tier.f:529 pins updated."
---

uref review fold-in: undefined refusal prints REPORT-FAILURE's line before E-UNDEFINED at both tiers; RESOLVE-SITE reports why it declined; tier.f and prim-owner-scope pins updated to rc 70. Brief /tmp/jerry-big/t0-uref2-brief.md. Base mnzvrsym. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-uref2
