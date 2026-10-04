---
title: Re-own closed-dot claims outside the defectcmt files
status: open
priority: 3
issue-type: task
created-at: "2026-10-03T22:38:13.357001+03:00"
---

Review 548 of 8a8912cc (defectcmt, a4a36e60): that dot's acceptance covered the files it touched. Files it did not touch still cite replaced or closed dot ids, some as live claims about a capability that has since landed, e.g. lib/string-roles.f:36 and lib/vector.f:239 ('retire when TVK-RAW (a3430ef2) lets the ...': TVK-RAW exists, checker.f:336-372), lib/process-env.f:181, lib/num-arithmetic.f:35 (on 8bc7609c). Acceptance: the lane's idcheck (~/.cache/tmp/kestrel-jerry-defectcmt/idcheck.sh) over every .f file under src lib tools test lists each closed-dot citation; each is classed history (stays), owner claim (named open dot, or a new dot the lane proposes), or a condition that has landed: then do the retirement the comment waits for (each with its check: a rejected program or E2E run through the real load path showing the workaround is no longer needed) or restate the comment as the rule. Excludes lines the retptr commit (432b3e9e) rewrites. After 8a8912cc and 432b3e9e are on master.
