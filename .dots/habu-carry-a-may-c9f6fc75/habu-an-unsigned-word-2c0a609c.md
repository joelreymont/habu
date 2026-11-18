---
title: An unsigned word returning a throwing quotation exits 76
status: closed
priority: 2
issue-type: task
created-at: "2026-11-18T09:08:06.304536+01:00"
closed-at: "2026-11-18T09:22:11.096139+01:00"
---

Problem: src/core/checker.f E-BUILD-EFFECT and the verify-record copy save a quotation's dead flag and intact masks, which its facts text cannot say, so ': MK [: T ;] ;' with a throwing T reads back as another effect (exit 76). Acceptance: a saved effect writes quotations in type form; MK loads rc 0, tested in test/xt-effect-test.f. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-c9f2.
