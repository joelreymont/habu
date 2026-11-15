---
title: Retire PRODUCT
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T14:44:16.521821+01:00"
closed-at: "2026-11-15T14:51:21.067073+01:00"
---

Problem: PRODUCT (src/core/sumtype-front.f TDECL-PRODUCT, src/core/sumtype.f CHECKER-DEFPRODUCT-BODY) is a second record declaration beside STRUCTURE, which accepts every PRODUCT shape and generates the same words. Its sites are Habu tests only (116 declarations in 21 files); Tender's 11 moved to STRUCTURE on Tender main 2ca1d02c; Zen and Maki have none. Acceptance: every test PRODUCT is a STRUCTURE or, where it tested a rule only PRODUCT had or one test/structure-decl-suite.f already covers, deleted; PRODUCT's front end, record words, replay and the TDECL-EVENT hooks are deleted (closing habu-call-decl-event-7d2ffdda); product and ;product are no longer reserved; PRODUCT is an undefined word with no special diagnostic; docs name STRUCTURE alone. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-prod.
