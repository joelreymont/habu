---
title: Frame each file in the AOT source-closure identity
status: open
priority: 1
issue-type: task
created-at: "2026-09-13T20:46:47.059907+03:00"
---

Problem: AOT-IDENT:CHAIN-DIGEST hashes raw concatenated file bytes. The current-review D03 fixture moves a definition into the previous file's EOF comment without changing that digest. Own src/habu/aot-ident.f and the existing artifact identity consumer. Frame a version/domain, ordered file count, exact recorded path bytes and unambiguous file content boundaries; coordinate artifact version with graph payload version 8. Do not introduce a new cache or normalize away path identity. Acceptance: same logical paths with before/after contents from the review produce different identities; real separate-file loads distinguish B; an artifact from before is refused against after despite intact producer/payload digests; unchanged closure accepts. Source: review/current-compiler 51546316, REVIEW D03 / REGRESSION CLOSURE-FRAMING. Claim: cedar; implementation pending.
