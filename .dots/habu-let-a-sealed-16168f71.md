---
title: "Let a sealed design call its host's admitted words"
status: active
priority: 1
issue-type: task
created-at: "2026-11-25T16:48:24.689519+01:00"
---

Problem: with the require rule, a design sealed by VOCABULARY:RESTRICT and confined by SOURCE-ROOT! is rejected E-UNREQUIRED when it calls a word of a package its harness admitted (src/core/requires.f ALLOWED?): it cannot require the host's file, which its source root rejects, and src/lib/vocabulary.f says it may call them. Acceptance: such a design loads with no require of a host file, the sealed source's own parts still require each other, unsealed loads are unchanged, and test/policy/confined has no host.f. Depends: habu-reject-a-file-3120d5e1. Claim: agent=worker workspace=.jj-ws.noindex/jerry-rq-seal.
