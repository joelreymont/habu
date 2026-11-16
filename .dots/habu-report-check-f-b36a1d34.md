---
title: "Report check.f's first rejection in source order"
status: active
priority: 2
issue-type: task
created-at: "2026-11-16T07:54:53.814282+01:00"
---

Problem: tools/check.f runs source discovery over the whole file before the walk, so a discovery rejection of a later form beats the walk's rejection of an earlier token, and check.f names a different mistake than the load: `nosuchword require nothere.f`, `if require nothere.f` and `{: a :} require nothere.f` give E-MISSING-SOURCE from check.f but the first token's record from the load; `: mk ( -- ) create does> {: a ;` gives E-UNCLOSED-LOCALS against the load's E-MISSING-SIGNATURE; `if {: a`, `begin {: a` and `[: {: a ;]` give E-UNCLOSED-LOCALS against the opener's record. The LSP shows check.f's record. Acceptance: without --all-errors, check.f reports the rejection the load reports for each case above (the earliest in source order); a test pins one discovery-vs-walk case. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-w201b.
