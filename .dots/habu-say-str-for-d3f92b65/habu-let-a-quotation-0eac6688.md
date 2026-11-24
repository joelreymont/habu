---
title: Let a quotation literal move a str it receives
status: active
priority: 2
issue-type: task
created-at: "2026-11-24T23:34:33.371925+01:00"
---

Problem: a quotation literal's stack words move its input as one cell (docs/forth-card.md:154), so with str one value `t [: dup c COUNT-EVENT s" response.output_" STR:STARTS-WITH? ;] JSON:WITH-STRING` is rejected (E-MISMATCH: `a` is one cell; `str` is 2 cells) though COUNT-EVENT names the str in the same body; src/lib/net/model.f COUNT-OUTPUT? exists only to name it. 76 quotation bodies in src, tools and test start by moving a two-cell value, 68 of them in files still marked split-str. Acceptance: a quotation literal whose body names its input's type moves a str with dup, swap, over and drop; COUNT-OUTPUT? folds back into READ-EVENT. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-quot (design .out/designs/design-quot-width.md).
