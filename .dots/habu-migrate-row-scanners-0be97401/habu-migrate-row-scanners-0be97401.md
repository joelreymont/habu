---
title: Migrate row scanners onto LINT-LEX registry events
status: open
priority: 2
issue-type: task
created-at: "2026-07-26T09:01:43.403675+02:00"
---

Problem: two classes of consumers still parse or half-trust raw primitive rows instead of consuming the LINT-LEX REGISTRY event. First, src/habu/verify-source.f row scanners never became LINT-LEX REGISTRY consumers (dispatch amendment item 32 of the landed registry-event work; the file has no LINT-LEX reference today), and the char-operand differential fixture - LINT-LEX rule canonical, verify-source conforms - was never written. Second, the fail-open lexer consumers recorded at the raw-primitive delivery never check the lexer ERROR? flag, so malformed rows silently truncate their token tables: check-core nominal, signature lint, aot lint, reserved-name lint, bootstrap-mirror lint. Required result: verify-source row scanners consume the REGISTRY event with the differential fixture; every listed consumer fails closed on lexical row errors before using its token table. Also remove the now-dead NAME-POSITION? PRIM: branch in bootstrap-mirror-lint.f noted at that delivery. Acceptance: a malformed registry row in a hostile fixture reds every listed consumer instead of truncating; the char-operand differential fixture pins verify-source to the LINT-LEX rule; mutations restoring fail-open behavior fail the focused tests. Files: src/habu/verify-source.f, tools/check-core.f, tools/signature-lint-core.f, tools/aot-lint-core.f, tools/reserved-name-lint-core.f, tools/bootstrap-mirror-lint.f, their focused tests. Verify: the lint-tools slice plus each focused test through test/run.f scheduling. Depends: none. Ownership: registry-event consumption in the named consumers only. Claim: unassigned.
