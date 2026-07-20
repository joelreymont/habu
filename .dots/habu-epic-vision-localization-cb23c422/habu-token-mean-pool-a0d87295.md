---
title: Token mean-pool op with VJP
status: open
priority: 2
issue-type: task
created-at: "2026-07-20T11:19:56.811499+02:00"
---

A mean-pool over the token axis reducing (num-tokens, width) to (width), used by recognition heads that summarize the sequence. Forward is a row mean; backward broadcasts the upstream gradient divided by the count. Register in the CAD op registry like the other ops (same SERIALIZE caution on registry file ownership), gradcheck at several shapes including a single token.
