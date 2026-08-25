---
title: Prove verifier event retirement
status: open
priority: 1
issue-type: task
created-at: "2026-07-23T07:47:55.753578+02:00"
blocks:
  - habu-load-registry-rollback-d955db59
---

Problem: VERIFY:SOURCE-BUF can currently publish DECL-EVENT FIELD rows and then retire their field targets, leaving dangling published identities. Owned result: production-path regression only after the rollback composer is loaded. Run one-field PRODUCT success and every syntax/field failure through the real VERIFY:SOURCE-BUF provider/build path. Assert exact family, schema, field provisional and published, string, event provisional and published, ordinal, selector, and scope depths before and after; resolve every surviving event. SOURCE-BUF-IN-SCOPE must retain rows until its real CHK-RUN-PREVERIFY caller closes the outer scope, then retire both references and targets. Direct public PRODUCT persists normally. Mutations omitting event SAVE, keeping event counts, skipping row scrub, or reversing restore order fail. Do not add private hooks, copied state machines, test-only cleanup, TRUSTED, or raw substring checks. Files: focused verify-source/check production tests and canonical suite inventory only. Smallest check: the one-field VERIFY:SOURCE-BUF success that currently changes field/event 0/0 to 0/3 and throws E-PF-ID when resolving the FIELD.
